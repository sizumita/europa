%{
open Ast
%}

%token <string> IDENT TYPE STRING CALLIDENT
%token <int> INT
%token TRUE FALSE NIL
%token FUNC IF ELSE EXTERN EQ ASSIGN
%token USE
%token PLUS MINUS DIV MUL
%token LB RB LP RP
%token SEMI COMMA COLON
%token EOF

%left ASSIGN
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
  | EXTERN name = IDENT; LP args = option(arguments); RP ret_type = option(TYPE) { 
    match args with
    | None -> Extern (name, [], [], get_type ret_type)
    | Some((names, types)) -> Extern (name, names, types, get_type ret_type)
    }
  | USE name = STRING { Use name }

arguments:
  | name = IDENT; arg_type = TYPE; COMMA rest = arguments { let (names, types) = rest in (name :: names, (get_type @@ Some arg_type) :: types) }
  | name = IDENT; arg_type = TYPE; option(COMMA) { ([name], [get_type @@ Some (arg_type)]) }

expression:
  | bin = binary { bin }
  | name = IDENT; ASSIGN value = expression; { Assign (name, value) }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | NIL { Nil }
  | value = IDENT { Ident value }
  | value = INT { I32 value }
  | value = STRING { Str value }
  | IF cond = expression; LB then_ = list(expression); RB ELSE LB else_ = list(expression); RB { If (cond, then_, else_) }
  | name = IDENT; LP RP { Call (Ident name, [||]) }
  | name = CALLIDENT; LP RP { Call (Ident name, [||]) }
  | name = IDENT; LP args = call_args RP { Call (Ident name, args |> Array.of_list)}
  | name = CALLIDENT; LP args = call_args RP { Call (Ident name, args |> Array.of_list)}

call_args:
  | value = expression; COMMA rest = call_args { value :: rest }
  | value = expression; option(COMMA) { [value] }

binary:
  | lhs = expression; PLUS rhs = expression { Binary (Plus, lhs, rhs) }
  | lhs = expression; MINUS rhs = expression { Binary (Minus, lhs, rhs) }
  | lhs = expression; MUL rhs = expression { Binary (Mul, lhs, rhs) }
  | lhs = expression; EQ rhs = expression { Binary (Eq, lhs, rhs) }
