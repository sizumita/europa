type argument_type =
  | Int

type operator =
  | Plus
  | Minus
  | Mul

type expr =
  | Ident of string
  | Integer of int
  | Type of string
  | Nil 
  [@@deriving show]

type statement =
  | Expr of expr
  (* name, arguments, argument types, return type, statements *)
  | Func of string * string list * argument_type list * argument_type * statement list
  | Call of expr * expr array
  | Operator of operator * expr * expr
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
  | "integer" -> Int
  | _ -> failwith "unknown type"

let parse_args (args: (string * string) list) =
args |> List.fold_left (fun (values, types) (v, t) -> 
  let arg_type = get_type t in
  (values @ [v], types @ [arg_type])
  ) ([], [])
