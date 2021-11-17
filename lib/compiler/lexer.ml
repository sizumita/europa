open Parser

let digit = [%sedlex.regexp? '0'..'9']
let ident = [%sedlex.regexp? 'A'..'Z' | 'a'..'z', Star ('A'..'Z' | 'a'..'z' | '0'..'9')]

type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

let new_line ?(n=0) lexbuf =
  let _ = n in
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
        pos_lnum = lcp.pos_lnum + 1;
        pos_bol = lcp.pos_cnum;
    }

exception ParseError of (string * int * int * string)

let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

let lexeme { stream; _ } = Sedlexing.Utf8.lexeme stream
let sub_lexeme {stream; _} start end_ = Sedlexing.Utf8.sub_lexeme stream start end_

let raise_ParseError lexbuf =
  let {pos; _} = lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  let tok = lexeme lexbuf in
  raise @@ ParseError (pos.pos_fname, line, col, tok)

let rec lex lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
    | '\n' ->
      update lexbuf; new_line lexbuf;
      lex lexbuf

    (** 空白文字 *)
    | white_space ->
      update lexbuf;
      lex lexbuf
    | digit -> update lexbuf ; INT (lexeme lexbuf |> int_of_string)
    | "func" -> update lexbuf ; FUNC
    | "i32" -> update lexbuf ; TYPE "integer"
    | "extern" -> update lexbuf ; EXTERN
    | "nil" -> update lexbuf ; NIL
    | "true" -> update lexbuf ; TRUE
    | "false" -> update lexbuf ; FALSE
    | "if" -> update lexbuf ; IF
    | "else" -> update lexbuf ; ELSE
    | ident -> update lexbuf ; IDENT (lexeme lexbuf)
    | '{' -> update lexbuf ; LB
    | '}' -> update lexbuf ; RB
    | '(' -> update lexbuf ; LP
    | ')' -> update lexbuf ; RP
    | ';' -> update lexbuf ; SEMI
    | ':' -> update lexbuf ; COLON
    | ',' -> update lexbuf ; COMMA
    | '+' -> update lexbuf ; PLUS
    | '-' -> update lexbuf ; MINUS
    | '/' -> update lexbuf ; DIV
    | '*' -> update lexbuf ; MUL
    | eof -> update lexbuf ; EOF
    | _ -> update lexbuf; raise_ParseError lexbuf

let parse f lexbuf =
  let lexer () = 
    let ante_position = lexbuf.pos in
    let token = lex lexbuf in
    let post_position = lexbuf.pos
  in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised f
  in
    try
      parser lexer
    with
    | Sedlexing.MalFormed
    | Sedlexing.InvalidCodepoint _
      -> failwith "parse error"


let parse_prog lexbuf = 
  parse Parser.prog lexbuf

let parse_statement lexbuf =
  parse Parser.toplevel lexbuf;
