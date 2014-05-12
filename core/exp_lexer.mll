(* file: exp_lexer.mll *)
{
  open Exp_parser 
  open Lexing

}

let name = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '+' '*' '!' '/' '?']+ | ('"' [^ '"' '\n']* '"') 
let comment = ("//" | "#" | "%" | "--") [^ '\n']* '\n'
let ccomment = "/*" ([^ '*']* '*' '*'* [^ '*' '/'])* [^ '*']* '*'+  '/'
let mlcomment = "(*" ([^ '*']* '*' '*'* [^ '*' ')'])* [^ '*']* '*'+  ')'
rule token = parse
  | ccomment as s { token lexbuf }
  | mlcomment as s {token lexbuf }
  | comment	{  token lexbuf }
  | [' ' '\t'] 	{ token lexbuf }
  | ['\n']	{ token lexbuf }
  | ['^' '\'']  { QUOTE }
  | '{' { LC }
  | '(' { LP }
  | ')' { RP }
  | '}' { RC }
  | ';' { SC }
  | ","  { COMMA }
  | name as name { NAME (name) }
  | _ { UNKNOWN }
