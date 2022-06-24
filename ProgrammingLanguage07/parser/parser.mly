%{
  open Ast
%}
%token <int> NUMBER
%token <string> IDENT
%token PLUS MINUS LPAREN RPAREN LET IN EQ NEG
%token EOF
%nonassoc high
%left PLUS MINUS 
%type <Ast.expr> expr
%start <Ast.expr> parse
%%

parse: 
  | e=expr EOF { e }
;
expr:
  | n=NUMBER { Num n }
  | NEG LPAREN e=expr RPAREN { Sub (Num 0, e) }
  | LPAREN e=expr RPAREN { e }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | LET x=IDENT EQ e1=expr IN e2=expr %prec high { LetIn (x, e1, e2) }
  | x=IDENT { Id x }
  ;
