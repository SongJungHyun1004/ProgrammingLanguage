let parse (s : string) : AstC.expr = 
  let lbuf = Lexing.from_string s in
  ParserC.parse LexerC.read lbuf
