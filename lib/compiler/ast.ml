type operator =
  | Plus
  | Minus
  | Mul

type variable_type =
  | IntT
  | NilT
  | BoolT

type expr =
  | Ident of string
  | Integer of int
  | Type of string
  | Bool of bool
  | Nil
  | If of expr * expr * expr
  | Call of expr * expr array
  | Operator of operator * expr * expr
  [@@deriving show]

type statement =
  | Expr of expr
  (* name, arguments, argument types, return type, statements *)
  | Func of string * string list * variable_type list * variable_type * statement list
  [@@deriving show]

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, column %i, token %s"
    (file_to_string file)
    line cnum tok

let get_type = function
  | Some("integer") -> IntT
  | Some "nil" -> NilT
  | None -> NilT
  | _ -> failwith "unknown type"
