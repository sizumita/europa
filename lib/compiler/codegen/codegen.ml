open Llvm
open Types
open Batteries
module Ast = Europa_compiler.Ast

let builtin_functions = [
  ("__String__compare", ["x"; "y"], [Ast.StrT; Ast.StrT], Ast.I32T)
]


let set_builtin_functions context =
  builtin_functions |> List.iter (fun x -> Statement.codegen_definition x context |> ignore) |> ignore


let init_global_context () =
  let paths = String.split_on_char '/' Sys.argv.(1) in
  let context = global_context () in
  let the_module = create_module context (List.rev paths |> List.hd) in
  let builder = builder context in
  let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
  let _ = Llvm_executionengine.initialize () in
  let the_execution_engine = Llvm_executionengine.create the_module in

  let zero_int = const_int (i8_type context) 0 in
  let t = {
    string_type = pointer_type (i8_type context);
    int32_type = i32_type context;
    bool_type = i1_type context;
    nil_type = void_type context;
    const_string = let string_gep_indices = [|zero_int; zero_int|] in
    fun s ->
      let const_s = Llvm.const_stringz context s in
      let global_s = Llvm.define_global s const_s the_module in
      const_gep global_s string_gep_indices
  } in
  let the_fpm = PassManager.create_function the_module in
  (
    {
      context = context;
      the_module = the_module;
      builder = builder;
      named_values = named_values;
      the_fpm = the_fpm;
      directory =
        (let x = (List.rev paths |> List.tl |> List.rev |> List.fold_left (fun a b -> a ^ "/" ^ b) "") in
        if not @@ String.starts_with Sys.argv.(1) "/" then "." ^ x else x);
      dep = [];
      t = t;
    },
    the_execution_engine,
    the_fpm
  )

let codegen_global (context : Types.codegen_context) statements =
  statements |> List.map (Statement.codegen_statement context) |> ignore
