%{
  open Ast
%}
%token <int> NUMBER
%token <string> IDENT
%token PLUS MINUS LPAREN RPAREN LET IN EQ NEG DEF ENDEF COMMA
%token EOF
%nonassoc high
%left PLUS MINUS 
%type <Ast.expr> expr
%type <Ast.fundef list> decl_list
%type <Ast.fundef> decl
%type <string list> param_list
%type <Ast.expr list> expr_list
%start <Ast.prog> parse
%%

parse: 
  | e=expr EOF { Prog ([], e) }
  | dl=decl_list e=expr EOF { Prog (dl, e) }
  ;
decl_list:
  | d=decl { [d] }
  | d=decl dl=decl_list { d :: dl }
  ;
decl: 
  | DEF x=IDENT EQ e=expr ENDEF { FunDef (x, [], e) }
  | DEF x=IDENT pl=param_list EQ e=expr ENDEF { FunDef (x, pl, e) }
  ;
param_list:
  | x=IDENT { [x] }
  | x=IDENT xl=param_list { x :: xl }
  ;
expr:
  | n=NUMBER { Num n }
  | MINUS n=NUMBER { Num (-1 * n) }
  | NEG LPAREN e=expr RPAREN { Sub (Num 0, e) }
  | LPAREN e=expr RPAREN { e }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | LET x=IDENT EQ e1=expr IN e2=expr %prec high { LetIn (x, e1, e2) }
  | x=IDENT { Id x }
  | x=IDENT LPAREN RPAREN { Call (x, []) }
  | x=IDENT LPAREN el=expr_list RPAREN { Call (x, el) }
  ;
expr_list:
  | e=expr { [e] }  
  | e=expr COMMA el=expr_list { e :: el }
  ;
