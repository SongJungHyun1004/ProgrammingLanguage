%{
  open Ast
%}
%token <int> NUMBER
%token <string> IDENT
%token PLUS MINUS LPAREN RPAREN LBRACE RBRACE EQ IF ELSE AND OR DEQ
%token TRUE FALSE LANGLE RANGLE SEMICOLON WHILE REF STAR DEF
%token EOF
%left OR
%left AND
%left LANGLE RANGLE
%left DEQ
%left PLUS MINUS 
%type <unit> unop
%type <Ast.stmt> stmt
%type <Ast.expr> expr
%start <Ast.prog> parse
%%

parse: 
  | sl=stmt+ EOF { Program sl }
  ;
stmt:
  | DEF id=IDENT SEMICOLON { VarDeclStmt id } 
  | STAR e1=expr EQ e2=expr SEMICOLON  { StoreStmt (e1, e2) } 
  | id=IDENT EQ STAR e=expr SEMICOLON { LoadStmt (id, e) } 
  | id=IDENT EQ e=expr SEMICOLON { StoreStmt (Ref id, e)}
  | IF e=expr LBRACE sl=stmt+ RBRACE { IfStmt (e, sl, []) }
  | IF e=expr LBRACE sl1=stmt+ RBRACE ELSE LBRACE sl2=stmt+ RBRACE { IfStmt (e, sl1, sl2) }
  | WHILE e=expr LBRACE sl=stmt+ RBRACE { WhileStmt (e, sl) }
  ;
expr:
  | n=NUMBER { Num n }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | unop n=NUMBER { Num (-1 * n) }
  | LPAREN e=expr RPAREN { e }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | e1=expr LANGLE e2=expr { Lt (e1, e2) }
  | e1=expr RANGLE e2=expr { Gt (e1, e2) }
  | e1=expr DEQ e2=expr { Eq (e1, e2) }
  | e1=expr AND e2=expr { And (e1, e2) }
  | e1=expr OR e2=expr { Or (e1, e2) }
  | REF x=IDENT { Ref x }
  | x=IDENT { Name x }
  ;
unop:
  | MINUS { () }
