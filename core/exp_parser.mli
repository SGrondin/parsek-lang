type token =
  | NAME of (string)
  | LC
  | LP
  | RP
  | RC
  | SC
  | COMMA
  | QUOTE
  | UNKNOWN

val kvalue_SC :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Prints.rel_t
