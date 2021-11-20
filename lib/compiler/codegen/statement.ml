open Llvm
open Types
module Ast = Europa_compiler.Ast
module Lexer = Europa_compiler.Lexer
open Batteries

let get_type context = function
  | Ast.I32T -> context.t.int32_type
  | Ast.BoolT -> context.t.bool_type
  | Ast.StrT -> context.t.string_type
  | Ast.UnitT -> context.t.unit_type

let create_entry_block_alloca context the_function var_name var_type = 
  let builder = builder_at context.context (instr_begin (entry_block the_function)) in
  build_alloca var_type var_name builder

let rec codegen_statement (context : Types.codegen_context) (statement: Ast.statement) =
  match statement with
  | Ast.Expr exp -> Expr.codegen_expr context exp
  | Ast.Func (name, args, arg_types, ret_type, body) -> codegen_func context (name, args, arg_types, ret_type, body)
  | Ast.Extern (name, args, arg_types, ret_type) -> codegen_definition (name, args, arg_types, ret_type) context
  (* | Ast.Module (name, stmts) ->
    let the_module = Llvm.create_module context.context name in
    let new_context = {context with the_module = the_module} in
    stmts |> List.iter (fun x -> let _ = codegen_statement new_context x in ());
    dump_module the_module;
    Llvm_analysis.assert_valid_module the_module; const_int (i1_type context.context) 1 *)
  | Ast.Use name ->
    let filename = context.directory ^ "/" ^ name ^ ".eu" in
    let lines = (File.lines_of filename |> Enum.fold (fun a b -> a ^ "\n" ^ b) "") in
    let lexbuf = Lexer.create_lexbuf @@ Sedlexing.Utf8.from_string lines in
    Lexer.parse_prog lexbuf |> List.iter (fun x -> let _ = x |> codegen_statement context in ());
    const_int context.t.bool_type 1
  (* | _ -> raise (Error "unknown operation stmt") *)

and create_argument_allocas context the_function args =
  let args = args |> Array.of_list in
  Array.iteri (fun i arg_value -> (
    let arg = args.(i) in
    let alloca = create_entry_block_alloca context the_function arg (type_of arg_value) in
    build_store arg_value alloca context.builder |> ignore;
    Hashtbl.add context.named_values arg alloca
  )) (params the_function)

and codegen_func context data = 
  let name, args, arg_types, ret_type, body = data in
  Hashtbl.clear context.named_values;
  Ast.show_variable_type ret_type |> print_endline;
  let the_function = codegen_definition (name, args, arg_types, ret_type) context in
  let bb = append_block context.context "entry" the_function in
  position_at_end bb context.builder;
  try
 
    create_argument_allocas context the_function args;
    let ret_value = if not @@ List.is_empty body then body |> List.map (fun x -> codegen_statement context x) |> List.rev |> List.hd else Expr.codegen_expr context @@ Ast.Unit in
    let _ = build_ret ret_value context.builder in
    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function. *)
    let _ = PassManager.run_function the_function context.the_fpm in

    the_function
  with e ->
    delete_function the_function;
    raise e
and codegen_definition data (context : codegen_context) =
  let name, args, arg_types, ret_type = data in
  Hashtbl.clear context.named_values;
  let args = args |> Array.of_list in
  let ret_type = ret_type |> get_type context in
  let arg_types = arg_types |> List.map (get_type context) |> Array.of_list in
  let ft = function_type ret_type arg_types in
  let the_function =
    match lookup_function name context.the_module with
    | None -> declare_function name ft context.the_module
    | Some f ->
      (* If 'f' already has a body, reject this. *)
      if Array.length (basic_blocks f) == 0 then () else
        raise (Types.Error "redefinition of function");

      (* If 'f' took a different number of arguments, reject. *)
      if Array.length (params f) == Array.length args then () else
        raise (Types.Error "redefinition of function with different # args");
      f
    in (* Set names for all arguments. *)
    Array.iteri (fun i a ->
      let n = args.(i) in
      set_value_name n a;
      Hashtbl.add context.named_values n a;
    ) (params the_function);
    the_function

