%token <string> IDENT
%token EOF

%start <string> prog

%%

prog:
  | ident = IDENT EOF { ident }
