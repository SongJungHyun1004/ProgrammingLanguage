let parse (s : string) : AstFP.expr = 
  let lbuf = Lexing.from_string s in
  ParserFP.parse LexerFP.read lbuf
