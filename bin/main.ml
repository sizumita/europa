open Llvm
open Europa_compiler
open Llvm_scalar_opts
open Batteries

let main_function_call the_function ee =
  let exec = Llvm_executionengine.get_function_address
    (Llvm.value_name the_function) (Foreign.funptr Ctypes.(void @-> returning int)) ee
  in
  Printf.printf "Evaluated to %i\n" @@ exec ()

let main () =
  let (context, the_execution_engine, the_fpm) = Europa_compiler_codegen.Codegen.init_global_context () in

  add_memory_to_register_promotion the_fpm;

  add_instruction_combination the_fpm;

  (* reassociate expressions. *)
  add_reassociation the_fpm;

  (* Eliminate Common SubExpressions. *)
  add_gvn the_fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_cfg_simplification the_fpm;

  ignore (PassManager.initialize the_fpm);
  Europa_compiler_codegen.Codegen.set_builtin_functions context;

  let lexbuf = Lexer.create_lexbuf @@
        Sedlexing.Utf8.from_string (File.lines_of Sys.argv.(1) |> Enum.fold (fun a b -> a ^ "\n" ^ b) "") in
        Lexer.parse_prog lexbuf |> Europa_compiler_codegen.Codegen.codegen_global context;
  dump_module context.the_module;
  match lookup_function "main" context.the_module with
  | None -> failwith "need function: main"
  | Some the_function ->
    let _ = print_endline "\n\n---- run main ----" in
    main_function_call the_function the_execution_engine;;


main ()
