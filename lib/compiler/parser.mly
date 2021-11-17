%{
open Ast
%}

%token <string> IDENT TYPE
%token <int> INT
%token TRUE FALSE NIL
%token FUNC IF ELSE EXTERN
%token PLUS MINUS DIV MUL
%token LB RB LP RP
%token SEMI COMMA COLON
%token EOF

%left PLUS MINUS
%left DIV MUL

%start <Ast.statement list> prog

%start <Ast.statement option> toplevel

%%

prog:
  | statements = list(statement); EOF { statements }
  | error
    { failwith 
          (Printf.sprintf "parse error at line %d column %d"
              ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol)
              )}

toplevel:
  | stmt = statement { Some stmt }
  | EOF { None }
  | error
    { failwith 
          (Printf.sprintf "parse error at line %d column %d"
              ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol)
              )}

// statement is toplevel syntax.
statement:
  | FUNC name = IDENT; LP args = option(arguments) RP ret_type = option(TYPE) LB body = option(list(expression)) RB {
    let body = match body with
    | None -> []
    | Some body -> body |> List.map (fun x -> Expr x) in

    match args with
    | None -> Func (name, [], [], get_type ret_type, body)
    | Some((names, types)) -> Func (name, names, types, get_type ret_type, body)
  }
  | EXTERN name = IDENT; LP args = option(arguments); RP ret_type = option(TYPE); SEMI { 
    match args with
    | None -> Extern (name, [], [], get_type ret_type)
    | Some((names, types)) -> Extern (name, names, types, get_type ret_type)
    }

arguments:
  | name = IDENT; arg_type = TYPE; COMMA rest = arguments { let (names, types) = rest in (name :: names, (get_type @@ Some arg_type) :: types) }
  | name = IDENT; arg_type = TYPE; option(COMMA) { ([name], [get_type @@ Some (arg_type)]) }

expression:
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NIL { Nil }
  | value = IDENT { Ident value }
  | value = INT { Integer value }
  | IF cond = expression; LB then_ = expression; RB ELSE LB else_ = expression; RB { If (cond, then_, else_) }
  | name = IDENT; LP RP { Call (Ident name, [||]) }
  | name = IDENT; LP args = list(expression) RP { Call (Ident name, args |> Array.of_list)}
  | lhs = expression; PLUS rhs = expression { Operator (Plus, lhs, rhs) }
  | lhs = expression; MINUS rhs = expression { Operator (Minus, lhs, rhs) }
  | lhs = expression; MUL rhs = expression { Operator (Mul, lhs, rhs) }
