type token =
  | NAME of (string)
  | COMPOSE
  | LC
  | LB
  | LP
  | RP
  | RB
  | RC
  | DOLLAR
  | SC
  | ARROW
  | COMMA
  | ANY
  | AT
  | QUOTE
  | DOT
  | SHORTEST
  | LONGEST
  | UNKNOWN
  | CONCAT
  | INPUT
  | OUTPUT
  | TYPE
  | IS
  | EOF

val input_with_eof :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
