(* file: main.ml *)
(* Assumes the parser file is "parser.mly" and the lexer file is "lexer.mll". *)
open Lexer
open Parser
open Lexing
open Prints
open Lexer

let main () =
  let lexbuf = Lexing.from_channel stdin 
  in 
    lexbuf.lex_curr_p <- 
      { lexbuf.lex_curr_p  with
    pos_fname = "<stdin>";
      };
    try
      Parser.input_with_eof Lexer.token lexbuf 
    with
  Parsing.Parse_error -> 
    begin
      Prints.report_position (lexbuf.lex_curr_p,lexbuf.lex_curr_p);
      print_string "ABORTING ...\n"
    end
;;
  

let _ = Printexc.print main ();;
