%{
  open Ast
%}
%token <int> NUMBER
%token PLUS MINUS LPAREN RPAREN
%token EOF
%left PLUS MINUS 
%start <Ast.expr> parse
%%

parse: 
  | e=expr EOF { e }
;
expr:
  | n=NUMBER { Num n }
  | LPAREN e=expr RPAREN { e }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  ;
