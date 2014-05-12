%{
  
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

%}
  
  
%token <string> NAME
%token COMPOSE
%token LC LB LP RP RB RC DOLLAR SC ARROW COMMA ANY AT QUOTE 
%token DOT
%token SHORTEST LONGEST UNKNOWN CONCAT
%token INPUT OUTPUT

%token TYPE IS EOF

%left COMPOSE
%left QUOTE
%left DOT




%start input_with_eof
%type <unit> input input_with_eof

%type <type_t> typeExp
%type <(string * type_t) list> list_of_label_type non_empty_list_of_label_type

%type <rel_t> iExp
%type <(string * rel_t) list> list_of_label_inv non_empty_list_of_label_inv
%type <rel_t list> non_empty_list_of_inv list_of_inv


%type <pat_t> patExp
%type <pat_t list>  non_empty_list_of_patExp
%type <(string * pat_t) list> list_of_label_pat 

%type <priority_t>  priority_order_RB
%type <concat_t> concat_RC

%% /* Grammar rules and actions follow */

input_with_eof: 
input EOF 
{ 

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
}
  ;
  
  input:    
  /* empty */	       {}
  |   input TYPE NAME IS typeExp SC 
      { if (Hashtbl.mem type_defs_ht $3) 
	then begin
	  Printf.eprintf 
	    "# ERROR: Type: \"%s\" already defined! Trying to recover ...\n" $3;
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
	else Hashtbl.add type_defs_ht $3 $5 }
  |   input LP typeExp ARROW typeExp RP NAME IS iExp SC
      { 
	let t_in =
	  match $3 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht $3)
	and t_out =  
	  match $5 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht $5)
	in
	  ht_add_new rel_defs_ht $7 (TypeName t_in, TypeName t_out, $9)
      }
  |   input typeExp NAME IS iExp SC
      { let t_out =
	  match ($2) with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht $2) 
	in
	  ht_add_new rel_defs_ht $3 (TypeName "~1", TypeName t_out, $5)
      }
  |   input NAME IS iExp SC
      { ht_add_new rel_defs_ht $2 (TypeName "~1", TypeName "~1", $4)
      }
/* Externally defined */
  |   input NAME IS INPUT SC 
      { ht_add_new rel_defs_ht $2 
	  (TypeName "~1", TypeName "~1", 
	   RelExternal (no_ftype,Input_fifo))}
  |   input NAME IS OUTPUT SC 
      { ht_add_new rel_defs_ht $2 
	  (TypeName "~1", TypeName "~1", 
	   RelExternal (no_ftype,Output_fifo))}
  |   input typeExp NAME IS INPUT SC 
      { let t_out =
	  match ($2) with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht $2) 
	in
	  ht_add_new rel_defs_ht $3 (TypeName "~1", TypeName t_out, 
				      RelExternal(no_ftype,Input_fifo))
      }
  |   input LP typeExp ARROW typeExp RP NAME IS INPUT SC 
      { 
	let t_in =
	  match $3 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht $3)
	and t_out =  
	  match $5 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht $5)
	in
	  ht_add_new rel_defs_ht $7 (TypeName t_in, TypeName t_out, 
				      RelExternal(no_ftype,Input_fifo))
      }
  |   input LP typeExp ARROW typeExp RP NAME IS OUTPUT SC 
      { 
	let t_in =
	  match $3 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht $3)
	and t_out =  
	  match $5 with
	      TypeName (x) -> x
	    | TypeProduct ([]) -> "~1"
	    | _ -> (add_with_new_name type_defs_ht $5)
	in
	  ht_add_new rel_defs_ht $7 (TypeName t_in, TypeName t_out, 
				      RelExternal(no_ftype,Output_fifo))
      }
/* Errors */
  |   input error  { 
	let error_pos_start = rhs_start_pos 2
	and error_pos_end = rhs_end_pos 2
	and skip_pos_start = rhs_end_pos 1 
	and skip_pos_end = rhs_end_pos 2 in 
	  report_position (error_pos_start,error_pos_end);
	  Printf.eprintf "#  --- ALL input from line:%d ch:%d, till line:%d ch:%d is ignored!\n"
	    skip_pos_start.pos_lnum (skip_pos_start.pos_cnum - skip_pos_start.pos_bol)
	    skip_pos_end.pos_lnum (skip_pos_end.pos_cnum - skip_pos_end.pos_bol)
      }  
  ;

  iExp:
    LC list_of_label_inv concat_RC 
    { RelProduct (no_ftype,$2,$3) }
/*  |    LC list_of_label_inv RC    { RelProduct (no_ftype,$2) } */
  |   LB list_of_inv priority_order_RB
      { 
	match ($2) with
	    [i] -> i
	  | _ ->RelSum (no_ftype,$2,$3) 
      }
/*  |   LB list_of_inv RB BY NAME { RelSum (no_ftype,$2,$5) } */
  |   LP DOLLAR NAME non_empty_list_of_patExp ARROW iExp RP
      { RelAbst  (no_ftype,$3,$4,$6) }
  |   LP non_empty_list_of_patExp ARROW iExp RP
      { RelAbst  (no_ftype,"",$2,$4) }
  |   iExp DOT NAME %prec DOT
      { RelProj (no_ftype,$1,$3) }
  |   QUOTE NAME iExp %prec QUOTE
      { RelLabel (no_ftype,$2,$3) }
  |   QUOTE NAME  
      { RelLabel (no_ftype,$2,RelProduct(no_ftype, [ ], [] )) }
  |   iExp COMPOSE iExp %prec COMPOSE
      { 
	match ($1,$3) with
	    (RelComp (_,le1), RelComp (_,le2)) -> RelComp (no_ftype, le1 @ le2)
	  | (RelComp (_,le1), _ ) ->  RelComp (no_ftype, le1 @ [$3])
	  | (_, RelComp (_,le2) ) ->  RelComp (no_ftype, $1 :: le2)
          | _ -> RelComp (no_ftype, [$1; $3])
      }
  |   DOLLAR NAME 
      { RelVar (no_ftype, $2) }
  |   DOLLAR 
      { RelVar (no_ftype, "") }
  |   AT NAME
      { RelField (no_ftype, $2) } 
  |   NAME
      { RelCall (no_ftype, $1) }
  ;


  priority_order_RB:
    RB  { First_match }
  | UNKNOWN RB { Unknown_match }
  | LONGEST NAME priority_order_RB { Longest_match ($2, $3) }
  | SHORTEST NAME priority_order_RB { Shortest_match ($2, $3) }
;

  concat_RC:
    RC { [] }
  | CONCAT NAME concat_RC 
      { $2 :: $3 }
;


  list_of_label_inv:
  /* empty */ { [ ] }
  |   non_empty_list_of_label_inv
      { $1 }
  ;

  non_empty_list_of_label_inv:  
    QUOTE NAME { [ ( $2, RelProduct( no_ftype, [ ], [] ) ) ] }
  |   QUOTE NAME iExp
      { [($2, $3)] }
  |   QUOTE NAME COMMA non_empty_list_of_label_inv
      { ($2,  RelProduct(no_ftype, [ ], [] ) ) :: $4 }
  |   QUOTE NAME iExp COMMA non_empty_list_of_label_inv  
      { ($2, $3) :: $5 }
  ;

  list_of_inv:
    /*empty */ { [] }
  | non_empty_list_of_inv { $1 }
;

  non_empty_list_of_inv:
    iExp { [ $1 ] }
  | iExp COMMA non_empty_list_of_inv
      { $1 :: $3 }
  ;

  non_empty_list_of_patExp:
    patExp { [ $1 ] }
  | patExp COMMA non_empty_list_of_patExp 
      { $1 :: $3 }
  ;

  patExp:
    ANY 
    { PatAny }
  |    NAME 
      { PatType( TypeName $1  ) } 
  |   LB list_of_label_type RB 
      { 
	PatType( TypeName (add_with_new_name type_defs_ht ( TypeUnion ($2) ) ) )
      }
  |   QUOTE NAME patExp
      { PatConst ($2,$3) }
  |   QUOTE NAME 
      { PatConst ($2, PatProd( [] )) }
  |   LC list_of_label_pat RC
      { if (List.exists (fun (x,_) -> x = "...") $2) 
	then
	  PatOpenProd ( List.filter (fun (x,_) -> not (x = "...")) $2 )
	else
	  PatProd ($2) 
      }
  ;
  
  list_of_label_pat: 
  /* empty */ { [ ] }
  |   non_empty_list_of_label_pat  
      { sort_by_label $1 }
  ;


  non_empty_list_of_label_pat: 
    ANY { [("...", PatAny )] }
  |   QUOTE NAME { [($2, PatProd([]) )] }
  |   QUOTE NAME patExp 
      { [($2, $3)] }
  |   ANY COMMA non_empty_list_of_label_pat  
      { ("...", PatAny ) :: $3 }
  |   QUOTE NAME COMMA non_empty_list_of_label_pat  
      { ($2, PatProd( [] ) ) :: $4 }
  |   QUOTE NAME patExp COMMA non_empty_list_of_label_pat  
      { ($2, $3) :: $5 }
  ;

  typeExp:  
    NAME { TypeName( $1 ) }
  |   LB list_of_label_type RB 
      { TypeUnion ($2) }
  |   LC list_of_label_type RC 
      { TypeProduct ($2) }
  ;
  list_of_label_type: 
  /* empty */ { [ ] }
  |   non_empty_list_of_label_type  
      { sort_by_label $1 }
  ;
  
  non_empty_list_of_label_type: 
    QUOTE NAME { [($2, TypeName( "~1" )  )] }
  |   QUOTE NAME typeExp 
      { match $3 with
	    TypeName (_) -> [($2, $3)]
	  | _ -> [($2, TypeName (add_with_new_name type_defs_ht $3))] 
      }
  |   QUOTE NAME COMMA non_empty_list_of_label_type  
      { ($2, TypeName( "~1" ) ) :: $4 }
  |   QUOTE NAME typeExp COMMA non_empty_list_of_label_type   
      { match $3 with
	    TypeName (_) -> ($2, $3) :: $5
	  | _  -> ($2, TypeName (add_with_new_name type_defs_ht $3)) :: $5 
      }
  ;

%%
  
