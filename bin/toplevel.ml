open Europa_compiler
open Llvm

let function_call the_function ee ret_type =
  match ret_type with
  | Ast.Int -> (
    let exec = Llvm_executionengine.get_function_address
      (Llvm.value_name the_function) (Foreign.funptr Ctypes.(void @-> returning int)) ee
    in
    Printf.printf "Evaluated to %i\n" @@ exec ()
  )

let rec main_loop the_fpm the_execution_engine =
  Printf.printf "ready> %!";
  match
    let lexbuf = Lexer.create_lexbuf @@
      Sedlexing.Utf8.from_string (read_line ()) in
      Lexer.parse_statement lexbuf
  with
  | Some (Func (name, args, arg_types, ret_type, body)) -> begin
    let the_function = Codegen.codegen_statement (Func (name, args, arg_types, ret_type, body)) the_fpm in
      dump_value the_function;
      print_endline "";
      function_call the_function the_execution_engine ret_type;
      print_endline "";
      main_loop the_fpm the_execution_engine
    end

  | exception Lexer.ParseError e -> begin
      print_endline @@ Ast.string_of_ParseError e;
      main_loop the_fpm the_execution_engine
    end

  | Some _ -> ()
  | None -> print_newline ()
