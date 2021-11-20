open Llvm

type types = {
  string_type: lltype;
  int32_type: lltype;
  bool_type: lltype;
  unit_type: lltype;
  const_string: string -> llvalue;
}

type codegen_context = {
  context: llcontext;
  the_module: llmodule;
  builder: llbuilder;
  named_values: (string, llvalue) Hashtbl.t;
  the_fpm: [ `Function ] PassManager.t;
  directory: string;
  dep: string list;
  t: types;
}

exception Error of string
