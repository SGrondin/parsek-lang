(* file: lexer.mll *)
{
  open Parser (* Assumes the parser file is "parser.mly". *)
  open Lexing

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p 
    in
      lexbuf.lex_curr_p <- 
	{ pos with
	    pos_lnum = pos.pos_lnum + 1;
	    pos_bol = pos.pos_cnum;
	};;
  
  let update_lineno lexbuf str =
    let rec nb_nl s =
      if String.contains s '\n' then
	let pos_nl = String.index s '\n' in
	  1 + nb_nl (String.sub s (pos_nl+1) ((String.length s)-pos_nl-1))
      else 0
    in 
    let pos = lexbuf.lex_curr_p 
    in
      lexbuf.lex_curr_p <- 
	{ pos with
	    pos_lnum = pos.pos_lnum + (nb_nl str)
	};;

}

let name = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '+' '*' '!' '/' '?' '>' '<' '=' '~']+ | ('"' [^ '"' '\n']* '"') 
let comment = ("//" | "#" | "%" | "--") [^ '\n']* '\n'
let ccomment = "/*" ([^ '*']* '*' '*'* [^ '*' '/'])* [^ '*']* '*'+  '/'
let mlcomment = "(*" ([^ '*']* '*' '*'* [^ '*' ')'])* [^ '*']* '*'+  ')'
rule token = parse
  | ccomment as s { update_lineno lexbuf s; token lexbuf }
  | mlcomment as s { update_lineno lexbuf s; token lexbuf }
  | comment	{ incr_lineno lexbuf; token lexbuf }
  | [' ' '\t'] 	{ token lexbuf }
  | ['\n']	{ incr_lineno lexbuf; token lexbuf }
  | ['^' '\'']  { QUOTE }
  | '{' { LC }
  | '[' { LB }
  | '(' { LP }
  | ')' { RP }
  | ']' { RB }
  | '}' { RC }
  | '$' { DOLLAR }
  | '@' { AT }
  | ['.'] { DOT }
  | ';' { SC }
  | "->" { ARROW }
  | ( "<type>" | "<code>" ) { TYPE }
  | ("<is>"|"=") { IS }
  | "<shortest>" { SHORTEST }
  | "<longest>" { LONGEST }
  | "<unknown>" { UNKNOWN }
  | ("<serialize>" | "<concat>") { CONCAT }
  | "<input>" { INPUT }
  | "<output>" { OUTPUT }
  | ("<any>" | "...") { ANY }
  | ","   { COMMA }
  | ( "::" | "|" ) { COMPOSE }
  | name as name { NAME (name) }
  | eof { EOF }
