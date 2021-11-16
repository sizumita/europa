open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context
let int_type = i32_type context
let string_type = i8_type context

let get_type = function
  | Ast.Int -> int_type

let rec codegen_expr = function
  | Ast.Ident name -> (try Hashtbl.find named_values name with
    | Not_found -> raise (Error (Printf.sprintf "unknown variable name: %s" name)))
  | Ast.Integer value -> const_int int_type value
  | _ -> raise (Error "unknown operation")

and codegen_statement statement the_fpm =
  match statement with
  | Ast.Expr exp -> codegen_expr exp
  | Ast.Call (Ast.Ident callee, args) -> 
    let callee =
      match lookup_function callee the_module with
      | Some callee -> callee
      | None -> raise (Error (Printf.sprintf "unknown function referenced: %s" callee))
    in
    let params = params callee in
    if Array.length params == Array.length args then () else
      raise (Error "incorrect # arguments passed");
    let args = Array.map codegen_expr args in
    build_call callee args "calltmp" builder
  | Ast.Func (name, args, arg_types, ret_type, body) -> codegen_func (name, args, arg_types, ret_type, body) the_fpm
  | Ast.Operator (op, lhs, rhs) -> 
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin
      match op with
      | Plus -> build_add lhs_val rhs_val "addtmp" builder
      | Minus -> build_sub lhs_val rhs_val "subtmp" builder
      | Mul -> build_mul lhs_val rhs_val "multmp" builder
    end
  | _ -> raise (Error "unknown operation")

and codegen_func data the_fpm = 
  let name, args, arg_types, ret_type, body = data in
  Hashtbl.clear named_values;
  let the_function = codegen_definition (name, args, arg_types, ret_type) in
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;
  try
    let ret_val = codegen_statement (List.hd body) the_fpm in
    let _ = build_ret ret_val builder in

    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function. *)
    let _ = PassManager.run_function the_function the_fpm in

    the_function
  with e ->
    delete_function the_function;
    raise e
and codegen_definition (name, args, arg_types, ret_type) =
  let args = args |> Array.of_list in
  let ret_type = ret_type |> get_type in
  let arg_types = arg_types |> List.map get_type |> Array.of_list in
  let ft = function_type ret_type arg_types in
  let the_function =
    match lookup_function name the_module with
    | None -> declare_function name ft the_module
    | Some f ->
      (* If 'f' already has a body, reject this. *)
      if Array.length (basic_blocks f) == 0 then () else
        raise (Error "redefinition of function");

      (* If 'f' took a different number of arguments, reject. *)
      if Array.length (params f) == Array.length args then () else
        raise (Error "redefinition of function with different # args");
      f
    in (* Set names for all arguments. *)
    Array.iteri (fun i a ->
      let n = args.(i) in
      set_value_name n a;
      Hashtbl.add named_values n a;
    ) (params the_function);
    the_function
