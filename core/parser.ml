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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  
  open Typegraph
  open Prints
  open Typing

  let type_defs_ht = Hashtbl.create 100;;
  let rel_defs_ht = Hashtbl.create 100;;
  let type_aliases_ht = Hashtbl.create 100;;
  
  Hashtbl.add type_defs_ht "~0" (TypeUnion [ ]);; 
  Hashtbl.add type_defs_ht "~1" (TypeProduct [ ]);; 

  
(* 
open Typegraph
let type_graph_ht = Hashtbl.create 100;;
*)

(**** PARSE ERRORS ****) 
  open Lexing

  let parse_error x = 
    Printf.eprintf "# SYNTAX ERRORS:\n"
  ;;


  let ht_add_new ht key value =
	if (Hashtbl.mem ht key) then 
	  begin 
	    Printf.eprintf 
	      "# ERROR: \"%s\" already defined! Ignoring this definition  ...\n" key;
	    let error_pos_start = rhs_start_pos 2
	    and error_pos_end = rhs_end_pos 2
	    and skip_pos_start = rhs_end_pos 1 
	    and skip_pos_end = rhs_end_pos 6 in 
	    report_position (error_pos_start,error_pos_end);
	    Printf.eprintf "#  --- ALL input from line:%d ch:%d, till line:%d ch:%d is ignored!\n"
	      skip_pos_start.pos_lnum (skip_pos_start.pos_cnum - skip_pos_start.pos_bol)
	      skip_pos_end.pos_lnum (skip_pos_end.pos_cnum - skip_pos_end.pos_bol)
	      (*	  raise Multiple_definitions *)
	  end
        else Hashtbl.add ht key value ;;

  let no_ftype = (TypeUnknown (OpenUnknown, []), 
		  TypeUnknown (OpenUnknown, []));;

# 78 "parser.ml"
let yytransl_const = [|
  258 (* COMPOSE *);
  259 (* LC *);
  260 (* LB *);
  261 (* LP *);
  262 (* RP *);
  263 (* RB *);
  264 (* RC *);
  265 (* DOLLAR *);
  266 (* SC *);
  267 (* ARROW *);
  268 (* COMMA *);
  269 (* ANY *);
  270 (* AT *);
  271 (* QUOTE *);
  272 (* DOT *);
  273 (* SHORTEST *);
  274 (* LONGEST *);
  275 (* UNKNOWN *);
  276 (* CONCAT *);
  277 (* INPUT *);
  278 (* OUTPUT *);
  279 (* TYPE *);
  280 (* IS *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\014\000\014\000\014\000\014\000\015\000\015\000\007\000\007\000\
\008\000\008\000\008\000\008\000\010\000\010\000\009\000\009\000\
\012\000\012\000\011\000\011\000\011\000\011\000\011\000\011\000\
\013\000\013\000\016\000\016\000\016\000\016\000\016\000\016\000\
\003\000\003\000\003\000\004\000\004\000\005\000\005\000\005\000\
\005\000\000\000"

let yylen = "\002\000\
\002\000\000\000\006\000\010\000\006\000\005\000\005\000\005\000\
\006\000\010\000\010\000\002\000\003\000\003\000\007\000\005\000\
\003\000\003\000\002\000\003\000\002\000\001\000\002\000\001\000\
\001\000\002\000\003\000\003\000\001\000\003\000\000\000\001\000\
\002\000\003\000\004\000\005\000\000\000\001\000\001\000\003\000\
\001\000\003\000\001\000\001\000\003\000\003\000\002\000\003\000\
\000\000\001\000\001\000\002\000\003\000\003\000\004\000\005\000\
\001\000\003\000\003\000\000\000\001\000\002\000\003\000\004\000\
\005\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\066\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\061\000\
\000\000\057\000\000\000\000\000\000\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\059\000\058\000\000\000\000\000\000\000\000\000\000\000\032\000\
\000\000\038\000\000\000\044\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\021\000\023\000\000\000\007\000\008\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\029\000\000\000\013\000\000\000\025\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\064\000\
\000\000\000\000\003\000\009\000\005\000\000\000\000\000\000\000\
\040\000\000\000\000\000\026\000\000\000\000\000\048\000\045\000\
\000\000\046\000\042\000\000\000\065\000\000\000\035\000\000\000\
\030\000\028\000\027\000\054\000\000\000\000\000\000\000\016\000\
\000\000\036\000\055\000\000\000\000\000\000\000\000\000\000\000\
\056\000\015\000\010\000\011\000\004\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\015\000\016\000\041\000\039\000\040\000\
\042\000\043\000\050\000\051\000\078\000\075\000\069\000\079\000"

let yysindex = "\016\000\
\000\000\000\000\000\000\001\000\000\000\254\254\020\255\020\255\
\204\255\038\255\000\000\045\255\033\255\067\255\064\255\000\000\
\078\255\000\000\082\255\070\255\099\255\000\000\087\255\166\255\
\048\255\127\255\135\255\141\255\115\255\134\255\013\255\194\255\
\000\000\000\000\204\255\204\255\005\255\152\255\042\255\000\000\
\071\255\000\000\079\255\000\000\003\255\020\255\158\255\000\000\
\178\255\145\255\180\255\000\000\000\000\166\255\000\000\000\000\
\166\255\000\000\195\255\020\255\190\255\203\255\200\255\201\255\
\065\255\151\255\000\000\211\255\000\000\166\255\000\000\212\255\
\213\255\208\255\000\000\205\255\215\255\210\255\000\000\214\255\
\186\255\186\255\186\255\166\255\206\255\206\255\000\000\000\000\
\020\255\219\255\000\000\000\000\000\000\087\255\105\255\042\255\
\000\000\079\255\079\255\000\000\003\255\173\255\000\000\000\000\
\216\255\000\000\000\000\156\255\000\000\202\255\000\000\087\255\
\000\000\000\000\000\000\000\000\003\255\207\255\166\255\000\000\
\055\255\000\000\000\000\003\255\162\255\220\255\221\255\124\255\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\222\255\217\255\225\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\096\255\128\255\
\000\000\072\255\000\000\000\000\000\000\000\000\000\000\004\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\175\255\000\000\000\000\000\000\226\255\225\255\000\000\000\000\
\000\000\224\255\000\000\000\000\000\000\093\255\000\000\000\000\
\000\000\000\000\000\000\000\000\058\255\000\000\000\000\000\000\
\000\000\107\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\228\255\000\000\000\000\000\000\000\000\
\000\000\192\255\000\000\000\000\112\255\131\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\153\255\000\000\
\000\000\000\000\000\000\000\000\000\000\229\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\230\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\251\255\250\255\199\255\243\255\000\000\169\255\
\154\000\000\000\187\255\218\255\000\000\085\000\132\000\160\255"

let yytablesize = 280
let yytable = "\031\000\
\011\000\017\000\088\000\019\000\116\000\022\000\111\000\023\000\
\024\000\025\000\062\000\062\000\106\000\026\000\057\000\076\000\
\001\000\077\000\027\000\028\000\123\000\013\000\058\000\065\000\
\122\000\064\000\061\000\129\000\059\000\062\000\063\000\109\000\
\118\000\022\000\014\000\023\000\024\000\025\000\020\000\080\000\
\085\000\026\000\105\000\086\000\107\000\021\000\027\000\028\000\
\044\000\067\000\045\000\046\000\095\000\029\000\030\000\022\000\
\047\000\023\000\024\000\025\000\048\000\068\000\049\000\026\000\
\063\000\063\000\057\000\032\000\027\000\028\000\108\000\033\000\
\057\000\022\000\093\000\126\000\127\000\022\000\022\000\022\000\
\059\000\022\000\070\000\022\000\034\000\071\000\059\000\022\000\
\022\000\022\000\022\000\022\000\035\000\036\000\019\000\072\000\
\073\000\074\000\019\000\019\000\019\000\038\000\019\000\031\000\
\019\000\125\000\057\000\128\000\019\000\019\000\019\000\019\000\
\019\000\018\000\033\000\031\000\112\000\018\000\018\000\018\000\
\059\000\018\000\037\000\018\000\055\000\057\000\033\000\052\000\
\018\000\018\000\018\000\018\000\020\000\133\000\037\000\053\000\
\020\000\020\000\020\000\059\000\020\000\054\000\020\000\056\000\
\037\000\037\000\037\000\020\000\020\000\020\000\020\000\022\000\
\066\000\023\000\024\000\025\000\083\000\057\000\081\000\026\000\
\034\000\120\000\094\000\057\000\027\000\028\000\022\000\130\000\
\023\000\024\000\025\000\059\000\034\000\044\000\026\000\045\000\
\046\000\059\000\082\000\027\000\028\000\039\000\114\000\115\000\
\117\000\048\000\044\000\049\000\045\000\046\000\084\000\039\000\
\039\000\039\000\018\000\087\000\007\000\008\000\048\000\047\000\
\049\000\089\000\047\000\047\000\018\000\060\000\007\000\008\000\
\090\000\091\000\092\000\096\000\098\000\099\000\100\000\102\000\
\101\000\103\000\124\000\110\000\104\000\059\000\057\000\097\000\
\060\000\121\000\119\000\113\000\000\000\131\000\132\000\060\000\
\000\000\049\000\041\000\051\000\052\000\053\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\006\000\000\000\007\000\008\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000"

let yycheck = "\013\000\
\000\000\008\000\060\000\009\000\101\000\001\001\094\000\003\001\
\004\001\005\001\007\001\008\001\082\000\009\001\002\001\013\001\
\001\000\015\001\014\001\015\001\117\000\024\001\010\001\037\000\
\112\000\021\001\032\000\124\000\016\001\035\000\036\000\089\000\
\102\000\001\001\015\001\003\001\004\001\005\001\001\001\046\000\
\054\000\009\001\081\000\057\000\083\000\001\001\014\001\015\001\
\001\001\008\001\003\001\004\001\066\000\021\001\022\001\001\001\
\009\001\003\001\004\001\005\001\013\001\020\001\015\001\009\001\
\007\001\008\001\002\001\001\001\014\001\015\001\084\000\008\001\
\002\001\002\001\010\001\021\001\022\001\006\001\007\001\008\001\
\016\001\010\001\012\001\012\001\007\001\007\001\016\001\016\001\
\017\001\018\001\019\001\020\001\011\001\024\001\002\001\017\001\
\018\001\019\001\006\001\007\001\008\001\015\001\010\001\008\001\
\012\001\119\000\002\001\121\000\016\001\017\001\018\001\019\001\
\020\001\002\001\008\001\020\001\012\001\006\001\007\001\008\001\
\016\001\010\001\024\001\012\001\010\001\002\001\020\001\001\001\
\017\001\018\001\019\001\020\001\002\001\010\001\007\001\001\001\
\006\001\007\001\008\001\016\001\010\001\001\001\012\001\010\001\
\017\001\018\001\019\001\017\001\018\001\019\001\020\001\001\001\
\001\001\003\001\004\001\005\001\012\001\002\001\001\001\009\001\
\008\001\006\001\012\001\002\001\014\001\015\001\001\001\006\001\
\003\001\004\001\005\001\016\001\020\001\001\001\009\001\003\001\
\004\001\016\001\001\001\014\001\015\001\007\001\098\000\099\000\
\012\001\013\001\001\001\015\001\003\001\004\001\011\001\017\001\
\018\001\019\001\001\001\001\001\003\001\004\001\013\001\008\001\
\015\001\012\001\011\001\012\001\001\001\012\001\003\001\004\001\
\006\001\010\001\010\001\001\001\001\001\001\001\007\001\001\001\
\012\001\008\001\012\001\001\001\007\001\016\001\001\001\070\000\
\008\001\024\001\011\001\096\000\255\255\010\001\010\001\007\001\
\255\255\008\001\011\001\008\001\008\001\008\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\001\001\255\255\003\001\004\001\005\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\023\001"

let yynames_const = "\
  COMPOSE\000\
  LC\000\
  LB\000\
  LP\000\
  RP\000\
  RB\000\
  RC\000\
  DOLLAR\000\
  SC\000\
  ARROW\000\
  COMMA\000\
  ANY\000\
  AT\000\
  QUOTE\000\
  DOT\000\
  SHORTEST\000\
  LONGEST\000\
  UNKNOWN\000\
  CONCAT\000\
  INPUT\000\
  OUTPUT\000\
  TYPE\000\
  IS\000\
  EOF\000\
  "

let yynames_block = "\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    Obj.repr(
# 89 "parser.mly"
( 

  (* DEBUG: *
  print_string "\n  * AFTER PARSING ... \n";
  print_string (string_of_type_ht type_defs_ht "\n---  TYPE DEFS  \n"); 
  print_string (string_of_rel_ht rel_defs_ht "\n--- REL DEFS \n"); 
  ***)

(*  run it for early discovering of type errors...  *)

    normalize_types type_defs_ht type_aliases_ht;
    
(* DEBUG: *
    print_string "\nTYPES AFTER NORMALIZATION ... \n";
    print_string (string_of_type_ht type_defs_ht "\n--- base types:\n");
    print_string (string_of_alias_ht type_aliases_ht "\n--- aliases:\n");
***)

  typing rel_defs_ht type_defs_ht type_aliases_ht;
  normalize_types type_defs_ht type_aliases_ht; 
  clean_aliases rel_defs_ht type_defs_ht type_aliases_ht;


(* TYPE INFOS: *)
  print_string 
    (string_of_type_ht type_defs_ht "\n--- TYPE DEFS  \n"); 

  print_string 
    (string_of_alias_ht type_aliases_ht "\n--- TYPE ALIASES\n");

  print_string (string_of_rel_ht rel_defs_ht "\n--- REL DEFS \n"); 
(***)

  print_string "\n--- EVALUATION (by interpretation) of main(): \n";

  let (i,o,e) = (Hashtbl.find rel_defs_ht "main") 
  in
  print_string 
    (string_of_rel_t 
       false
       (Eval.evaluate rel_defs_ht type_defs_ht type_aliases_ht e)
    );
  print_string "\n\n"
)
# 353 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
                     ()
# 359 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : unit) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : type_t) in
    Obj.repr(
# 138 "parser.mly"
      ( if (Hashtbl.mem type_defs_ht _3) 
	then begin
	  Printf.eprintf 
	    "# ERROR: Type: \"%s\" already defined! Trying to recover ...\n" _3;
	  let error_pos_start = rhs_start_pos 2
	  and error_pos_end = rhs_end_pos 2
	  and skip_pos_start = rhs_end_pos 1 
	  and skip_pos_end = rhs_end_pos 6 in 
	    report_position (error_pos_start,error_pos_end);
	    Printf.eprintf "#  --- ALL input from line:%d ch:%d, till line:%d ch:%d is ignored!\n"
	      skip_pos_start.pos_lnum (skip_pos_start.pos_cnum - skip_pos_start.pos_bol)
	      skip_pos_end.pos_lnum (skip_pos_end.pos_cnum - skip_pos_end.pos_bol)
	  (*	  raise Multiple_definitions *)
	end
	else Hashtbl.add type_defs_ht _3 _5 )
# 382 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : unit) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : type_t) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : type_t) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : rel_t) in
    Obj.repr(
# 154 "parser.mly"
      ( 
	let t_in =
	  match _3 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht _3)
	and t_out =  
	  match _5 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht _5)
	in
	  ht_add_new rel_defs_ht _7 (TypeName t_in, TypeName t_out, _9)
      )
# 406 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : type_t) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : rel_t) in
    Obj.repr(
# 169 "parser.mly"
      ( let t_out =
	  match (_2) with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht _2) 
	in
	  ht_add_new rel_defs_ht _3 (TypeName "~1", TypeName t_out, _5)
      )
# 423 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : rel_t) in
    Obj.repr(
# 178 "parser.mly"
      ( ht_add_new rel_defs_ht _2 (TypeName "~1", TypeName "~1", _4)
      )
# 433 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 182 "parser.mly"
      ( ht_add_new rel_defs_ht _2 
	  (TypeName "~1", TypeName "~1", 
	   RelExternal (no_ftype,Input_fifo)))
# 443 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 186 "parser.mly"
      ( ht_add_new rel_defs_ht _2 
	  (TypeName "~1", TypeName "~1", 
	   RelExternal (no_ftype,Output_fifo)))
# 453 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : type_t) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 190 "parser.mly"
      ( let t_out =
	  match (_2) with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht _2) 
	in
	  ht_add_new rel_defs_ht _3 (TypeName "~1", TypeName t_out, 
				      RelExternal(no_ftype,Input_fifo))
      )
# 470 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : unit) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : type_t) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : type_t) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 200 "parser.mly"
      ( 
	let t_in =
	  match _3 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht _3)
	and t_out =  
	  match _5 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht _5)
	in
	  ht_add_new rel_defs_ht _7 (TypeName t_in, TypeName t_out, 
				      RelExternal(no_ftype,Input_fifo))
      )
# 494 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : unit) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : type_t) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : type_t) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 216 "parser.mly"
      ( 
	let t_in =
	  match _3 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht _3)
	and t_out =  
	  match _5 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht _5)
	in
	  ht_add_new rel_defs_ht _7 (TypeName t_in, TypeName t_out, 
				      RelExternal(no_ftype,Output_fifo))
      )
# 518 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    Obj.repr(
# 232 "parser.mly"
                   ( 
	let error_pos_start = rhs_start_pos 2
	and error_pos_end = rhs_end_pos 2
	and skip_pos_start = rhs_end_pos 1 
	and skip_pos_end = rhs_end_pos 2 in 
	  report_position (error_pos_start,error_pos_end);
	  Printf.eprintf "#  --- ALL input from line:%d ch:%d, till line:%d ch:%d is ignored!\n"
	    skip_pos_start.pos_lnum (skip_pos_start.pos_cnum - skip_pos_start.pos_bol)
	    skip_pos_end.pos_lnum (skip_pos_end.pos_cnum - skip_pos_end.pos_bol)
      )
# 534 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (string * rel_t) list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : concat_t) in
    Obj.repr(
# 246 "parser.mly"
    ( RelProduct (no_ftype,_2,_3) )
# 542 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : rel_t list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : priority_t) in
    Obj.repr(
# 249 "parser.mly"
      ( 
	match (_2) with
	    [i] -> i
	  | _ ->RelSum (no_ftype,_2,_3) 
      )
# 554 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : pat_t list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : rel_t) in
    Obj.repr(
# 256 "parser.mly"
      ( RelAbst  (no_ftype,_3,_4,_6) )
# 563 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : pat_t list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : rel_t) in
    Obj.repr(
# 258 "parser.mly"
      ( RelAbst  (no_ftype,"",_2,_4) )
# 571 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : rel_t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 260 "parser.mly"
      ( RelProj (no_ftype,_1,_3) )
# 579 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : rel_t) in
    Obj.repr(
# 262 "parser.mly"
      ( RelLabel (no_ftype,_2,_3) )
# 587 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 264 "parser.mly"
      ( RelLabel (no_ftype,_2,RelProduct(no_ftype, [ ], [] )) )
# 594 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : rel_t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : rel_t) in
    Obj.repr(
# 266 "parser.mly"
      ( 
	match (_1,_3) with
	    (RelComp (_,le1), RelComp (_,le2)) -> RelComp (no_ftype, le1 @ le2)
	  | (RelComp (_,le1), _ ) ->  RelComp (no_ftype, le1 @ [_3])
	  | (_, RelComp (_,le2) ) ->  RelComp (no_ftype, _1 :: le2)
          | _ -> RelComp (no_ftype, [_1; _3])
      )
# 608 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 274 "parser.mly"
      ( RelVar (no_ftype, _2) )
# 615 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 276 "parser.mly"
      ( RelVar (no_ftype, "") )
# 621 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 278 "parser.mly"
      ( RelField (no_ftype, _2) )
# 628 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 280 "parser.mly"
      ( RelCall (no_ftype, _1) )
# 635 "parser.ml"
               : rel_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 285 "parser.mly"
        ( First_match )
# 641 "parser.ml"
               : priority_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 286 "parser.mly"
               ( Unknown_match )
# 647 "parser.ml"
               : priority_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : priority_t) in
    Obj.repr(
# 287 "parser.mly"
                                   ( Longest_match (_2, _3) )
# 655 "parser.ml"
               : priority_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : priority_t) in
    Obj.repr(
# 288 "parser.mly"
                                    ( Shortest_match (_2, _3) )
# 663 "parser.ml"
               : priority_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 292 "parser.mly"
       ( [] )
# 669 "parser.ml"
               : concat_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : concat_t) in
    Obj.repr(
# 294 "parser.mly"
      ( _2 :: _3 )
# 677 "parser.ml"
               : concat_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 299 "parser.mly"
              ( [ ] )
# 683 "parser.ml"
               : (string * rel_t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (string * rel_t) list) in
    Obj.repr(
# 301 "parser.mly"
      ( _1 )
# 690 "parser.ml"
               : (string * rel_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 305 "parser.mly"
               ( [ ( _2, RelProduct( no_ftype, [ ], [] ) ) ] )
# 697 "parser.ml"
               : (string * rel_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : rel_t) in
    Obj.repr(
# 307 "parser.mly"
      ( [(_2, _3)] )
# 705 "parser.ml"
               : (string * rel_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : (string * rel_t) list) in
    Obj.repr(
# 309 "parser.mly"
      ( (_2,  RelProduct(no_ftype, [ ], [] ) ) :: _4 )
# 713 "parser.ml"
               : (string * rel_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : rel_t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : (string * rel_t) list) in
    Obj.repr(
# 311 "parser.mly"
      ( (_2, _3) :: _5 )
# 722 "parser.ml"
               : (string * rel_t) list))
; (fun __caml_parser_env ->
    Obj.repr(
# 315 "parser.mly"
               ( [] )
# 728 "parser.ml"
               : rel_t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : rel_t list) in
    Obj.repr(
# 316 "parser.mly"
                          ( _1 )
# 735 "parser.ml"
               : rel_t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : rel_t) in
    Obj.repr(
# 320 "parser.mly"
         ( [ _1 ] )
# 742 "parser.ml"
               : rel_t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : rel_t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : rel_t list) in
    Obj.repr(
# 322 "parser.mly"
      ( _1 :: _3 )
# 750 "parser.ml"
               : rel_t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : pat_t) in
    Obj.repr(
# 326 "parser.mly"
           ( [ _1 ] )
# 757 "parser.ml"
               : pat_t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : pat_t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : pat_t list) in
    Obj.repr(
# 328 "parser.mly"
      ( _1 :: _3 )
# 765 "parser.ml"
               : pat_t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 333 "parser.mly"
    ( PatAny )
# 771 "parser.ml"
               : pat_t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 335 "parser.mly"
      ( PatType( TypeName _1  ) )
# 778 "parser.ml"
               : pat_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (string * type_t) list) in
    Obj.repr(
# 337 "parser.mly"
      ( 
	PatType( TypeName (add_with_new_name type_defs_ht ( TypeUnion (_2) ) ) )
      )
# 787 "parser.ml"
               : pat_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : pat_t) in
    Obj.repr(
# 341 "parser.mly"
      ( PatConst (_2,_3) )
# 795 "parser.ml"
               : pat_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 343 "parser.mly"
      ( PatConst (_2, PatProd( [] )) )
# 802 "parser.ml"
               : pat_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (string * pat_t) list) in
    Obj.repr(
# 345 "parser.mly"
      ( if (List.exists (fun (x,_) -> x = "...") _2) 
	then
	  PatOpenProd ( List.filter (fun (x,_) -> not (x = "...")) _2 )
	else
	  PatProd (_2) 
      )
# 814 "parser.ml"
               : pat_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 354 "parser.mly"
              ( [ ] )
# 820 "parser.ml"
               : (string * pat_t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_list_of_label_pat) in
    Obj.repr(
# 356 "parser.mly"
      ( sort_by_label _1 )
# 827 "parser.ml"
               : (string * pat_t) list))
; (fun __caml_parser_env ->
    Obj.repr(
# 361 "parser.mly"
        ( [("...", PatAny )] )
# 833 "parser.ml"
               : 'non_empty_list_of_label_pat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 362 "parser.mly"
                 ( [(_2, PatProd([]) )] )
# 840 "parser.ml"
               : 'non_empty_list_of_label_pat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : pat_t) in
    Obj.repr(
# 364 "parser.mly"
      ( [(_2, _3)] )
# 848 "parser.ml"
               : 'non_empty_list_of_label_pat))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_list_of_label_pat) in
    Obj.repr(
# 366 "parser.mly"
      ( ("...", PatAny ) :: _3 )
# 855 "parser.ml"
               : 'non_empty_list_of_label_pat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_list_of_label_pat) in
    Obj.repr(
# 368 "parser.mly"
      ( (_2, PatProd( [] ) ) :: _4 )
# 863 "parser.ml"
               : 'non_empty_list_of_label_pat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : pat_t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_list_of_label_pat) in
    Obj.repr(
# 370 "parser.mly"
      ( (_2, _3) :: _5 )
# 872 "parser.ml"
               : 'non_empty_list_of_label_pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 374 "parser.mly"
         ( TypeName( _1 ) )
# 879 "parser.ml"
               : type_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (string * type_t) list) in
    Obj.repr(
# 376 "parser.mly"
      ( TypeUnion (_2) )
# 886 "parser.ml"
               : type_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (string * type_t) list) in
    Obj.repr(
# 378 "parser.mly"
      ( TypeProduct (_2) )
# 893 "parser.ml"
               : type_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 381 "parser.mly"
              ( [ ] )
# 899 "parser.ml"
               : (string * type_t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (string * type_t) list) in
    Obj.repr(
# 383 "parser.mly"
      ( sort_by_label _1 )
# 906 "parser.ml"
               : (string * type_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 387 "parser.mly"
               ( [(_2, TypeName( "~1" )  )] )
# 913 "parser.ml"
               : (string * type_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : type_t) in
    Obj.repr(
# 389 "parser.mly"
      ( match _3 with
	    TypeName (_) -> [(_2, _3)]
	  | _ -> [(_2, TypeName (add_with_new_name type_defs_ht _3))] 
      )
# 924 "parser.ml"
               : (string * type_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : (string * type_t) list) in
    Obj.repr(
# 394 "parser.mly"
      ( (_2, TypeName( "~1" ) ) :: _4 )
# 932 "parser.ml"
               : (string * type_t) list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : type_t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : (string * type_t) list) in
    Obj.repr(
# 396 "parser.mly"
      ( match _3 with
	    TypeName (_) -> (_2, _3) :: _5
	  | _  -> (_2, TypeName (add_with_new_name type_defs_ht _3)) :: _5 
      )
# 944 "parser.ml"
               : (string * type_t) list))
(* Entry input_with_eof *)
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
let input_with_eof (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
# 403 "parser.mly"
  
# 971 "parser.ml"
