open Llvm
open Types
module Ast = Europa_compiler.Ast

let create_entry_block_alloca context the_function var_name var_type = 
  let builder = builder_at context.context (instr_begin (entry_block the_function)) in
  build_alloca var_type var_name builder

let get_type context = function
  | Ast.I32T -> i32_type context.context
  | Ast.NilT -> void_type context.context
  | Ast.BoolT -> i1_type context.context
  | Ast.StrT -> pointer_type (i8_type context.context)

let rec codegen_expr context expr =
  let get_type = get_type context in
  let zero_int = const_int (get_type Ast.I32T) 0 in
  let const_string =
    let string_gep_indices = [|zero_int; zero_int|] in
    fun s ->
      let const_s = Llvm.const_stringz context.context s in
      let global_s = Llvm.define_global s const_s context.the_module in
      Llvm.const_gep global_s string_gep_indices in
  match expr with
  | Ast.Ident name ->
    let v = (try Hashtbl.find context.named_values name with
    | Not_found -> raise (Error (Printf.sprintf "unknown variable name: %s" name)))
    in
    build_load v name context.builder
  | Ast.I32 value -> const_int (get_type Ast.I32T) value
  | Ast.Str value -> const_string value
  | Ast.Nil -> const_null (void_type context.context)
  | Ast.If (cond, then_, else_) ->
    let cond = codegen_expr context cond in
    let zero = const_int (get_type Ast.BoolT) 1 in
    let cond_val = build_icmp Icmp.Eq cond zero "ifcond" context.builder in
    let start_bb = insertion_block context.builder in
    let the_function = block_parent start_bb in
    let then_bb = append_block context.context "then" the_function in
    position_at_end then_bb context.builder;
    let then_val = then_ |> List.map (fun x -> codegen_expr context x) |> List.rev |> List.hd in
    let new_then_bb = insertion_block context.builder in
    let else_bb = append_block context.context "else" the_function in
    position_at_end else_bb context.builder;
    let else_val = else_ |> List.map (fun x -> codegen_expr context x) |> List.rev |> List.hd in
    let new_else_bb = insertion_block context.builder in
    let merge_bb = append_block context.context "ifcont" the_function in
    position_at_end merge_bb context.builder;
    let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
    let phi = build_phi incoming "iftmp" context.builder in
    position_at_end start_bb context.builder;
    ignore (build_cond_br cond_val then_bb else_bb context.builder);
    position_at_end new_then_bb context.builder; ignore (build_br merge_bb context.builder);
    position_at_end new_else_bb context.builder; ignore (build_br merge_bb context.builder);
    position_at_end merge_bb context.builder;

    phi
  | Ast.Call (Ast.Ident callee, args) -> 
    let callee =
      match lookup_function callee context.the_module with
      | Some callee -> callee
      | None -> raise (Error (Printf.sprintf "unknown function referenced: %s" callee))
    in
    let params = params callee in
    if Array.length params == Array.length args then () else
      raise (Error "incorrect # arguments passed");
    let args = args |> Array.map @@ codegen_expr context in
    build_call callee args "calltmp" context.builder
  | Ast.Bool value -> const_int (get_type Ast.I32T) (if value then 1 else 0)
  | Ast.Binary (op, lhs, rhs) -> 
    let lhs_val = codegen_expr context lhs in
    let rhs_val = codegen_expr context rhs in
    begin
      match op with
      | Plus -> build_add lhs_val rhs_val "addtmp" context.builder
      | Minus -> build_sub lhs_val rhs_val "subtmp" context.builder
      | Mul -> build_mul lhs_val rhs_val "multmp" context.builder
      | Eq -> let i = build_icmp Icmp.Eq lhs_val rhs_val "eqtmp" context.builder in
      build_uitofp i (i1_type context.context) "booltmp" context.builder
    end
  | Ast.Assign (name, value) ->
    let val_ = codegen_expr context value in
    begin
      try 
        let variable = Hashtbl.find context.named_values name 
        in
        ignore(build_store val_ variable context.builder);
        val_
      with
        | Not_found -> assign_new context name value
        | x -> raise x
    end
  | _ -> raise (Error "unknown operation expr")
and assign_new context name value =
  let the_function = block_parent (insertion_block context.builder) in
  let init_val = codegen_expr context value in
  let alloca = create_entry_block_alloca context the_function name (type_of init_val) in
  build_store init_val alloca context.builder |> ignore;
  Hashtbl.add context.named_values name alloca;
  init_val
