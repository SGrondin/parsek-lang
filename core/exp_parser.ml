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

open Parsing;;
let _ = parse_error;;
# 2 "exp_parser.mly"
  

(**** PARSE ERRORS ****) 
  open Lexing
  open Typegraph
  open Typing
  open Prints

  let parse_error x = 
    Printf.eprintf "# SYNTAX ERRORS:\n"
  ;;

  let no_ftype = (TypeUnknown (OpenUnknown, []), 
		  TypeUnknown (OpenUnknown, []));;

# 31 "exp_parser.ml"
let yytransl_const = [|
  258 (* LC *);
  259 (* LP *);
  260 (* RP *);
  261 (* RC *);
  262 (* SC *);
  263 (* COMMA *);
  264 (* QUOTE *);
  265 (* UNKNOWN *);
    0|]

let yytransl_block = [|
  257 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\004\000\004\000\
\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\002\000\000\000\001\000\002\000\003\000\
\004\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
\006\000\000\000\001\000\000\000\002\000\003\000\000\000\000\000\
\009\000\000\000\010\000"

let yydgoto = "\002\000\
\005\000\006\000\008\000\009\000"

let yysindex = "\008\000\
\255\254\000\000\002\255\012\255\000\000\009\255\015\255\013\255\
\000\000\255\254\000\000\254\254\000\000\000\000\002\255\010\255\
\000\000\002\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\014\255\000\000\000\000\000\000\000\000\000\000\
\000\000\253\254\000\000\016\255\000\000\000\000\000\000\017\255\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\002\000\000\000\249\255"

let yytablesize = 22
let yytable = "\003\000\
\003\000\004\000\004\000\004\000\015\000\004\000\004\000\017\000\
\001\000\007\000\019\000\014\000\010\000\016\000\011\000\012\000\
\018\000\013\000\005\000\000\000\007\000\008\000"

let yycheck = "\002\001\
\002\001\005\001\006\001\007\001\007\001\008\001\008\001\015\000\
\001\000\008\001\018\000\010\000\001\001\012\000\006\001\001\001\
\007\001\005\001\005\001\255\255\005\001\005\001"

let yynames_const = "\
  LC\000\
  LP\000\
  RP\000\
  RC\000\
  SC\000\
  COMMA\000\
  QUOTE\000\
  UNKNOWN\000\
  "

let yynames_block = "\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Prints.rel_t) in
    Obj.repr(
# 35 "exp_parser.mly"
            ( _1 )
# 109 "exp_parser.ml"
               : Prints.rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (string * Prints.rel_t) list) in
    Obj.repr(
# 40 "exp_parser.mly"
    ( RelProduct (no_ftype,_2,[]) )
# 116 "exp_parser.ml"
               : Prints.rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Prints.rel_t) in
    Obj.repr(
# 42 "exp_parser.mly"
      ( RelLabel (no_ftype,_2,_3) )
# 124 "exp_parser.ml"
               : Prints.rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "exp_parser.mly"
      ( RelLabel (no_ftype,_2,RelProduct(no_ftype, [ ],[] )) )
# 131 "exp_parser.ml"
               : Prints.rel_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "exp_parser.mly"
              ( [ ] )
# 137 "exp_parser.ml"
               : (string * Prints.rel_t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (string * Prints.rel_t) list) in
    Obj.repr(
# 50 "exp_parser.mly"
      ( _1 )
# 144 "exp_parser.ml"
               : (string * Prints.rel_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "exp_parser.mly"
               ( [ ( _2, RelProduct( no_ftype, [ ],[] ) ) ] )
# 151 "exp_parser.ml"
               : (string * Prints.rel_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Prints.rel_t) in
    Obj.repr(
# 56 "exp_parser.mly"
      ( [(_2, _3)] )
# 159 "exp_parser.ml"
               : (string * Prints.rel_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : (string * Prints.rel_t) list) in
    Obj.repr(
# 58 "exp_parser.mly"
      ( (_2,  RelProduct(no_ftype, [ ],[] ) ) :: _4 )
# 167 "exp_parser.ml"
               : (string * Prints.rel_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Prints.rel_t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : (string * Prints.rel_t) list) in
    Obj.repr(
# 60 "exp_parser.mly"
      ( (_2, _3) :: _5 )
# 176 "exp_parser.ml"
               : (string * Prints.rel_t) list))
(* Entry kvalue_SC *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let kvalue_SC (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Prints.rel_t)
;;
# 65 "exp_parser.mly"
  
# 203 "exp_parser.ml"
