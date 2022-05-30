{
  open ParserC

  exception LexingError of string
}

(* Decimal number *)
let pos_digit = ['1'-'9']
let digit = ['0'-'9']
let pos_number = pos_digit digit*
let number = "0" | pos_number 

(* White space *)
let ws = [' ''\t''\n']*

(* identifier *)
let id = ['a'-'z']['a'-'z''A'-'Z''\'''_''1'-'9']*

rule read = 
  parse
  | '+' { PLUS }
  | '-' { MINUS }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '<' { LANGLE }
  | '~' { NEG }
  | '=' { EQ }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "endif" { ENDIF }
  | "->" { ARROW }
  | number as n { NUMBER (int_of_string n) }
  | id as x { IDENT x }
  | ws { read lexbuf }
  | eof { EOF }
  | _ { raise (LexingError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }


