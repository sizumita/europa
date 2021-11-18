open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context Sys.argv.(1)
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context
let int_type = i32_type context
let void_type = void_type context
let bool_type = i1_type context
let char_type = i8_type context
let string_type = pointer_type char_type

let zero_int = const_int int_type 0

let const_string =
  let string_gep_indices = [|zero_int; zero_int|] in
  fun s ->
    let const_s = Llvm.const_stringz context s in
    let global_s = Llvm.define_global s const_s the_module in
    Llvm.const_gep global_s string_gep_indices

let get_body body = function
  | Ast.NilT -> if List.length body = 0 then Ast.Expr (Ast.I32 1) else List.hd body
  | _ -> List.hd body

let get_type = function
  | Ast.I32T -> int_type
  | Ast.NilT -> void_type
  | Ast.BoolT -> bool_type
  | Ast.StrT -> string_type

let rec codegen_expr = function
  | Ast.Ident name -> (try Hashtbl.find named_values name with
    | Not_found -> raise (Error (Printf.sprintf "unknown variable name: %s" name)))
  | Ast.I32 value -> const_int int_type value
  | Ast.Str value -> const_string value
  | Ast.If (cond, then_, else_) ->
    let cond = codegen_expr cond in
    let zero = const_int bool_type 1 in
    let cond_val = build_icmp Icmp.Eq cond zero "ifcond" builder in
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    let then_bb = append_block context "then" the_function in
    position_at_end then_bb builder;
    let then_val = codegen_expr then_ in
    let new_then_bb = insertion_block builder in
    let else_bb = append_block context "else" the_function in
    position_at_end else_bb builder;
    let else_val = codegen_expr else_ in
    let new_else_bb = insertion_block builder in
    let merge_bb = append_block context "ifcont" the_function in
    position_at_end merge_bb builder;
    let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
    let phi = build_phi incoming "iftmp" builder in
    position_at_end start_bb builder;
    ignore (build_cond_br cond_val then_bb else_bb builder);
    position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
    position_at_end new_else_bb builder; ignore (build_br merge_bb builder);
    position_at_end merge_bb builder;

    phi
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
  | Ast.Bool value -> const_int int_type (if value then 1 else 0)
  | Ast.Operator (op, lhs, rhs) -> 
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin
      match op with
      | Plus -> build_add lhs_val rhs_val "addtmp" builder
      | Minus -> build_sub lhs_val rhs_val "subtmp" builder
      | Mul -> build_mul lhs_val rhs_val "multmp" builder
    end
  | _ -> raise (Error "unknown operation expr")

and codegen_statement statement the_fpm =
  match statement with
  | Ast.Expr exp -> codegen_expr exp
  | Ast.Func (name, args, arg_types, ret_type, body) -> codegen_func (name, args, arg_types, ret_type, body) the_fpm
  | Ast.Extern (name, args, arg_types, ret_type) -> codegen_definition (name, args, arg_types, ret_type)
  (* | _ -> raise (Error "unknown operation stmt") *)

and codegen_func data the_fpm = 
  let name, args, arg_types, ret_type, body = data in
  Hashtbl.clear named_values;
  let the_function = codegen_definition (name, args, arg_types, ret_type) in
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;
  try
 
    let _ = match ret_type with
    | Ast.NilT -> build_ret_void builder
    | _ -> let ret_val = codegen_statement (get_body body ret_type) the_fpm in build_ret ret_val builder in
    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function. *)
    let _ = PassManager.run_function the_function the_fpm in

    the_function
  with e ->
    delete_function the_function;
    raise e
and codegen_definition (name, args, arg_types, ret_type) =
  Hashtbl.clear named_values;
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
