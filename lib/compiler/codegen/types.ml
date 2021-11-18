open Llvm

type codegen_context = {
  context: llcontext;
  the_module: llmodule;
  builder: llbuilder;
  named_values: (string, llvalue) Hashtbl.t;
  the_fpm: [ `Function ] PassManager.t;
  directory: string;
  dep: string;
}

exception Error of string
