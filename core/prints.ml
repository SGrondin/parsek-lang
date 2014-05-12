(* TYPE EXPRESSIONS *)

open Printf

exception Empty_type
exception Not_found_type
exception Circular_type
exception Multiple_type_definitions

module StringSet = Set.Make(String)

type open_type_t = OpenUnknown | OpenProduct | OpenUnion
;;

type type_t = 
    TypeUnknown of (open_type_t * ((string * type_t) list))
  | TypeName of string 
  | TypeUnion of (string * type_t) list 
  | TypeProduct of (string * type_t) list
;;


type ftype_t = type_t * type_t;;


(* PATTERN EXPRESSIONS *) 

type pat_t = 
    PatAny
  | PatType of type_t
  | PatProd of (string * pat_t) list 
  | PatOpenProd of (string * pat_t) list 
  | PatConst of string * pat_t
;;
      

(* order of conflict resolution *)
type priority_t = First_match | Unknown_match 
	       | Longest_match of string * priority_t
	       | Shortest_match of string * priority_t
;;

let rec string_of_priority_t l =
  match (l) with
      First_match -> ""
    | Unknown_match -> "<unknown>" 
    | Shortest_match (n, ll) 
      -> ("<shortest> "^n^" "^(string_of_priority_t ll))
    | Longest_match (n, ll) 
      -> ("<longest> "^n^" "^(string_of_priority_t ll))
;;

type concat_t = string list;;

let string_of_concat_t l =
  match l with
      [] -> ""
    | _ -> ("<concat> "^(String.concat " " l))
;; 


type fifo_t = Input_fifo | Output_fifo
;;
let string_of_fifo_t l =
    match l with
      Input_fifo -> "<input>"
    | Output_fifo -> "<output>"
;; 


(* RELATION EXPRESSIONS *)


type rel_t =
    RelProduct of ftype_t * ((string * rel_t) list) * concat_t
  | RelSum of ftype_t * (rel_t list) * priority_t
  | RelAbst of ftype_t * string * (pat_t list) * rel_t
  | RelProj of ftype_t * rel_t * string
  | RelLabel of ftype_t * string * rel_t
  | RelComp of ftype_t * (rel_t list)
  | RelCall of ftype_t * string
  | RelVar of ftype_t * string
  | RelField of ftype_t * string
  | RelExternal  of ftype_t * fifo_t
;;

(*********************************)

let string_of_name_x_list print_x l =
 String.concat ", " 
    (List.map (fun (name,x) -> "\'"^name^" "^(print_x x)) l)
;;

let rec string_of_type_t te =
  match te with
      TypeName( name ) ->  (name)
    | TypeUnion ltl ->
	("["^(string_of_name_x_list string_of_type_t ltl)^"]")	
    | TypeProduct ltl ->
	("{"^(string_of_name_x_list string_of_type_t ltl)^"}")

    | TypeUnknown ( OpenUnknown , [] ) ->  "<?>"
    | TypeUnknown ( OpenUnknown , ltl ) -> 
	("<"^(string_of_name_x_list string_of_type_t ltl)^", ...>")	
    | TypeUnknown ( OpenProduct,  []) -> "{?}"
    | TypeUnknown ( OpenProduct, ltl) -> 
	("{"^(string_of_name_x_list string_of_type_t ltl)^", ...}")
    | TypeUnknown ( OpenUnion, [] ) -> "[?]"
    | TypeUnknown ( OpenUnion, ltl ) -> 
	("["^(string_of_name_x_list string_of_type_t ltl)^", ...]")
;;

  
let rec string_of_pat_t p =
  match p with
      PatAny -> "..."
    | PatType (t) ->  (string_of_type_t t) 
    | PatOpenProd [] -> "{...}"
    | PatProd (lt) -> 
	("{"^(string_of_name_x_list string_of_pat_t lt)^"}")
    | PatOpenProd (lt) -> 
	("{"^(string_of_name_x_list string_of_pat_t lt)^", ...}")
    | PatConst (l,p) ->
	("'"^l^" "^(string_of_pat_t p))
;;

let string_of_pat_list lp = 
  String.concat ", " 
    (List.map string_of_pat_t lp)
;;


let rec string_of_rel_t show_types e =
  let show_type t = (string_of_type_t t)
  in
(*  let show_type_in t = "[ "^(show_type t)^" :: " *)
  let show_type_in t = "["
(* and show_type_out t = " :: "^(show_type t)^" ]" *)
  and show_type_out t = ":"^(show_type t)^"]"
  in
  let show_type_in t = if show_types then show_type_in t else ""
  and show_type_out t = if show_types then show_type_out t else ""
  in
  match e with
      RelProduct ((t_i,t_o), lp , cl ) ->
	((show_type_in t_i)^
	   "{"^(string_of_name_x_list (string_of_rel_t show_types) lp)^
	   (string_of_concat_t cl)^"}"
(*	 ^ (if (by = "") then "" else (" <by> "^by)) *)
	 ^ (show_type_out t_o)
	)
    | RelSum ((t_i,t_o),le,by) ->
	((show_type_in t_i)^
	  "["
	 ^ (String.concat ", " (List.map (string_of_rel_t show_types) le)) 
	 ^ (string_of_priority_t by)
	 ^"]"
	 ^ (show_type_out t_o)
	 
	)
    | RelAbst ((t_i,t_o),x,pl,d) ->
	((show_type_in t_i)^
	  "("^(if (x = "") then "" else ("$"^x))^" "^(string_of_pat_list pl)^" -> "
	 ^(string_of_rel_t show_types d)^")" 
	 ^ (show_type_out t_o)
	)
    | RelProj ((t_i,t_o),e,l) ->
	((show_type_in t_i)^
	  (string_of_rel_t show_types e)^"."^l 
	 ^ (show_type_out t_o)
	)
    | RelLabel ((t_i,t_o),l,e) ->
	((show_type_in t_i)^
	  "'"^l^" "^(string_of_rel_t show_types e) 
	 ^ (show_type_out t_o)
	)
    | RelComp ((t_i,t_o),le) ->
	((show_type_in t_i)^
	  "["^(String.concat " | " (List.map (string_of_rel_t show_types) le))^"]" 
	 ^ (show_type_out t_o)
	)
    | RelVar ((t_i,t_o),n) ->
	((show_type_in t_i)^
	  "$"^n 
	 ^ (show_type_out t_o)
	)
    | RelField ((t_i,t_o),n) ->
	((show_type_in t_i)^
	  "@"^n 
	 ^ (show_type_out t_o)
	)
    | RelCall ((t_i,t_o),n) -> ((show_type_in t_i)^
	  n 
	 ^ (show_type_out t_o)
	)
    | RelExternal ((t_i,t_o), fifo) -> ((show_type_in t_i)^
	  (string_of_fifo_t fifo)
	 ^ (show_type_out t_o)
	)
;;

let sort_by_label x =
  List.stable_sort
    (fun (a,_) (b,_) -> compare a b)
    x
;;

(* ================================= *)

let string_of_type_ht type_ht header =
  Hashtbl.fold
    (fun t te s -> 
       match te with
	   TypeProduct [] ->
	     (s^" <type> "^t^" <is> {};\n")
	 | _ -> (s^" <type> "^t^" <is> "^(string_of_type_t te)^";\n")
    )
    type_ht
    header
;;

let string_of_alias_ht alias_ht header =
  Hashtbl.fold
    (fun a n s -> (s^" "^a^" -> "^n^"\n"))
    alias_ht
    header
;;

let string_of_part_ht part_ht header =
  Hashtbl.fold
    (fun t e s -> 
       (s^" "^t^" -> {"
	^
	(String.concat "," (StringSet.elements e))
	^"}\n"))
    part_ht
    header
;;


let string_of_rel_ht rel_defs_ht header=
 Hashtbl.fold
    (fun  n (i,o,e) s->
       ( s^" ("^(string_of_type_t i)^" -> "^(string_of_type_t o)^") "^n
	 ^" <is> " ^ (string_of_rel_t false e) ^";\n"))
   rel_defs_ht
   header
;;






let report_position (pos_start,pos_end) =
  if (pos_start = pos_end) 
  then 
    Printf.eprintf "# LOCATION: File %s, line %d, character %d:\n"
      pos_start.Lexing.pos_fname
      pos_start.Lexing.pos_lnum
      (pos_start.Lexing.pos_cnum - pos_start.Lexing.pos_bol)
  else
    Printf.eprintf "# LOCATION: File %s, line %d, characters %d-%d:\n"
      pos_start.Lexing.pos_fname
      pos_start.Lexing.pos_lnum
      (pos_start.Lexing.pos_cnum - pos_start.Lexing.pos_bol)
      (pos_end.Lexing.pos_cnum - pos_end.Lexing.pos_bol)
;;
