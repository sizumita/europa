open Llvm
open Europa_compiler
open Llvm_scalar_opts

let main () =
  let _ = Llvm_executionengine.initialize () in
  let the_execution_engine = Llvm_executionengine.create Codegen.the_module in

  let the_fpm = PassManager.create_function Codegen.the_module in

  add_instruction_combination the_fpm;

  (* reassociate expressions. *)
  add_reassociation the_fpm;

  (* Eliminate Common SubExpressions. *)
  add_gvn the_fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_cfg_simplification the_fpm;

  ignore (PassManager.initialize the_fpm);

  (* Run the main "interpreter loop" now. *)
  Toplevel.main_loop the_fpm the_execution_engine;
  dump_module Europa_compiler.Codegen.the_module;;

main ()
