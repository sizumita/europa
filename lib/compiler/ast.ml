type operator =
  | Plus
  | Minus
  | Mul
  | Eq
  [@@deriving show]

type variable_type =
  | I32T
  | BoolT
  | StrT
  | UnitT
  [@@deriving show]

type expr =
  | Ident of string
  | I32 of int
  | Str of string
  | Type of string
  | Bool of bool
  | Unit
  | If of expr * expr list * expr list
  | Call of expr * expr array
  | Binary of operator * expr * expr
  | Assign of string * expr
  [@@deriving show]

type statement =
  | Expr of expr
  (* name, arguments, argument types, return type, statements *)
  | Func of string * string list * variable_type list * variable_type * statement list
  | Extern of string * string list * variable_type list * variable_type
  | Use of string
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
  | Some "i32" -> I32T
  | Some "str" -> StrT
  | Some "bool" -> BoolT
  | None -> UnitT
  | _ -> failwith "unknown type"
