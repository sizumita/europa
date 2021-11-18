open Llvm
open Types
open Batteries
module Ast = Europa_compiler.Ast


let init_global_context () =
  let paths = String.split_on_char '/' Sys.argv.(1) in
  let context = global_context () in
  let the_module = create_module context (List.rev paths |> List.hd) in
  let builder = builder context in
  let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
  let _ = Llvm_executionengine.initialize () in
  let the_execution_engine = Llvm_executionengine.create the_module in

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
      dep = "";
    },
    the_execution_engine,
    the_fpm
  )

let codegen_global (context : Types.codegen_context) statements =
  statements |> List.map (Statement.codegen_statement context) |> ignore
