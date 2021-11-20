open Llvm
module Ast = Europa_compiler.Ast

type types = {
  string_type: lltype;
  int32_type: lltype;
  bool_type: lltype;
  unit_type: lltype;
  float64_type: lltype; 
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

let get_type context = function
  | Ast.I32T -> context.t.int32_type
  | Ast.BoolT -> context.t.bool_type
  | Ast.StrT -> context.t.string_type
  | Ast.UnitT -> context.t.unit_type
  | Ast.F64T -> context.t.float64_type

exception Error of string
