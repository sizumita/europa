%{
open Ast
%}

%token <string> IDENT TYPE
%token <int> INT
%token FUNC
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
  | stmt = statement; EOF { Some stmt }
  | EOF { None }
  | error
    { failwith 
          (Printf.sprintf "parse error at line %d column %d"
              ($startpos.pos_lnum) ($startpos.pos_cnum - $startpos.pos_bol)
              )}

statement:
  | FUNC name = IDENT; LP args = option(arguments); RP ret_type = TYPE; LB statements = list(statement); RB { 
    match args with
    | None -> Func (name, [], [], get_type ret_type, statements)
    | Some(args) -> let args, arg_types = parse_args args in Func (name, args, arg_types, get_type ret_type, statements)
  }
  | name = IDENT; LP args = list(call_arguments) RP SEMI { Call (Ident name, args |> Array.of_list) }
  | exp = expr { Expr exp }
  | o = operators { o }

operators:
  | lhs = expr; PLUS rhs = expr { Operator (Plus, lhs, rhs) }
  | lhs = expr; MINUS rhs = expr { Operator (Minus, lhs, rhs) }
  // | lhs = expr; DIV rhs = expr { Operator (Div, lhs, rhs) }
  | lhs = expr; MUL rhs = expr { Operator (Mul, lhs, rhs) }

expr:
  | value = INT { Integer value }
  | value = IDENT { Ident value }

arguments:
  | rest = arguments; COMMA value = argument; option(COMMA) { rest @ [value] }
  | value = argument { [value] }

call_arguments:
  | value = IDENT { Ident value }
  | value = INT { Integer value }

argument:
  | name = IDENT; COLON t = TYPE { (name, t) }
