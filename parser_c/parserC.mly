%{
  open AstC
%}
%token <int> NUMBER
%token <string> IDENT
%token PLUS MINUS LPAREN RPAREN LET IN EQ NEG FUN ARROW IF THEN ELSE ENDIF 
%token TRUE FALSE LANGLE
%token EOF
%nonassoc low
%left LANGLE
%left PLUS MINUS 
%nonassoc NUMBER NEG LPAREN LET IDENT IF TRUE FALSE
%nonassoc mid
%nonassoc high
%type <unit> unop
%type <AstC.expr> expr
%type <AstC.expr list> args
%type <string list> IDENT+
%start <AstC.expr> parse
%%

parse: 
  | e=expr EOF { e }
  ;
expr:
  | n=NUMBER { Num n }
  | TRUE { Num 1 }
  | FALSE { Num 0 }
  | unop n=NUMBER { Num (-1 * n) }
  | NEG LPAREN e=expr RPAREN { Sub (Num 0, e) }
  | LPAREN e=expr RPAREN { e }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | e1=expr LANGLE e2=expr { LessThan (e1, e2) }
  | IF e1=expr THEN e2=expr ELSE e3=expr ENDIF { Cond (e1, e2, e3) }
  | LET x=IDENT EQ e1=expr IN e2=expr %prec low { LetIn (x, e1, e2) }
  | LET x=IDENT xl=IDENT+ EQ e1=expr IN e2=expr %prec low { 
    let ld = List.fold_right (fun x en -> Lambda (x, en)) xl e1 in
    LetIn (x, ld, e2) 
  }
  | x=IDENT { Id x }
  | e=expr el=args %prec mid { 
    match List.rev (e :: el) with
    | [] -> failwith "Unreachable"
    | h :: t -> 
        List.fold_right (fun e en -> App (e, en)) (List.rev t) h
  }
  | LPAREN FUN xl=IDENT+ ARROW e=expr RPAREN { 
    List.fold_right (fun x en -> Lambda (x, en)) xl e
  }
  ;
args:
  | e=expr %prec high { [e] }
  | el=args e=expr %prec high { el @ [e] }
  ;
unop:
  | MINUS { () }
