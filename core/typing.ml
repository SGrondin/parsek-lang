open Printf

open Prints
open Typegraph

exception Type_mismatch
exception Unresolved_type
exception Reference_instead_of_type
exception Undefined_identifier

(* module Binding = Map.Make(String) *)

let get_ftype e =
   match e with
      RelProduct( ftype, _ , _ )
    | RelSum ( ftype, _ , _ ) 
    | RelAbst ( ftype, _ , _, _ )
    | RelProj ( ftype, _, _ )
    | RelLabel ( ftype, _, _)
    | RelComp ( ftype, _) 
    | RelCall ( ftype, _ )  
    | RelVar ( ftype, _ ) 
    | RelField ( ftype, _ ) 
    | RelExternal ( ftype, _ ) -> ftype
;;

let rec get_type_field types_ht alias_ht t l =
(* DEBUG: *
  printf ".... get_type_field %s -> %s\n" (string_of_type_t t) l; 
***)
  match t with
      TypeUnknown ( _ , [] ) -> TypeUnknown (OpenUnknown, [])
    | TypeUnknown ( o , (lab,t)::ltl ) -> 
	if (lab = l) then t
	else
	  get_type_field types_ht alias_ht (TypeUnknown ( o , ltl )) l
    | TypeUnion (lt) 
    | TypeProduct (lt) ->
	begin
	  let res = List.filter (fun (ll,_) -> ll = l) lt
	  in
	    match res with
		[(_,tt)] -> tt
	      | _ -> (printf "TM 1\n"; raise Type_mismatch)
	end  
    | TypeName tn -> 
	try 
	  get_type_field types_ht alias_ht
	    (Hashtbl.find types_ht (unalias alias_ht tn))
	    l
	with Not_found ->
	  begin
	    eprintf "Unknown type name: \"%s\" \n" (unalias alias_ht tn);
	    raise Undefined_identifier
	  end

 ;;

let is_unit_type t =
  match t with 
       TypeName "~1" 
    | TypeProduct [] -> true
    | _ -> false
;;

let rec compare_types types_ht alias_ht t1 t2 =
(* DEBUG *
  printf ".... compare_types %s and %s ...\n"  (string_of_type_t t1) (string_of_type_t t2); 
***)
  if (t1=t2) then true 
  else
    match (t1,t2) with
	(TypeUnknown _ ,_)
      | (_,TypeUnknown _) -> false
      | (TypeName n1, _) ->
	  (try 
	    compare_types types_ht alias_ht 
	      (Hashtbl.find types_ht (unalias alias_ht n1)) 
	      t2
	  with Not_found ->
	    begin
	      eprintf "Unknown type name: \"%s\" \n" (unalias alias_ht n1);
	      raise Undefined_identifier
	    end)
      | (_, TypeName n2) -> 
	  (try  
	    compare_types types_ht alias_ht 	
	      t1
	      (Hashtbl.find types_ht (unalias alias_ht n2)) 
	  with Not_found ->
	    begin
	      eprintf "Unknown type name: \"%s\" \n" (unalias alias_ht n2);
	      raise Undefined_identifier
	    end)
      | (TypeUnion lt1, TypeUnion lt2)
      | (TypeProduct lt1, TypeProduct lt2) ->
	  if ((List.length lt1) = (List.length lt2))
	  then
	    List.for_all2 
	      (fun (l1,t1) (l2,t2) -> 
		 (l1 = l2) &
		   (compare_types types_ht alias_ht t1 t2)
	      )
	      lt1
	      lt2
	  else
	    raise (printf "TM 2\n"; Type_mismatch)
      | _ -> raise (printf "TM 3\n"; Type_mismatch)
;;


let rec merge_types types_ht alias_ht t1 t2 =
(* DEBUG *
    printf "\n  -- merge types: %s, %s  \n"
      (string_of_type_t t1)
      (string_of_type_t t2); 
**)
  let merge_op o1 o2 =
    match (o1,o2) with
	(OpenUnknown, o) 
      | (o, OpenUnknown) -> o
      | (OpenProduct,OpenProduct) 
      | (OpenUnion,OpenUnion) -> o1
      | _ -> raise (printf "TM 4\n"; Type_mismatch)
  in
  let rec merge lt1 lt2 =
    match (lt1,lt2) with
	([],l) | (l,[]) -> l
      | ((l1,t1):: llt1, (l2,t2)::llt2) ->
	  if (l1 < l2) then
	    (l1,t1) :: (merge llt1 lt2)
	  else if (l2 < l1) then
	    (l2,t2) :: (merge lt1 llt2)
	  else 
	    (l1, merge_types types_ht alias_ht t1 t2) ::
	      (merge llt1 llt2)
  in
  if (compare_types types_ht alias_ht t1 t2) then t1 
  else
    match (t1,t2) with
	(TypeUnknown ( o1, lt1), TypeUnknown (o2, lt2)) ->
	  TypeUnknown ( merge_op o1 o2, merge lt1 lt2 )
      | (TypeName n, t) | (t, TypeName n) -> 
	  (try 
	    merge_types types_ht alias_ht 
	      (Hashtbl.find types_ht (unalias alias_ht n)) 
	      t
	  with  Not_found ->
	  begin
	    eprintf "Unknown type name: \"%s\" \n" (unalias alias_ht n);
	    raise Undefined_identifier
	  end)
      | (TypeUnion lt1, TypeUnion lt2) ->
	  let ltl = merge lt1 lt2
	  in 
	    if (((List.length ltl) = (List.length lt1)) 
		& ((List.length ltl) = (List.length lt2)))
	    then
	      TypeUnion ltl
	    else
	      raise (printf "TM 5\n"; Type_mismatch)
      | (TypeUnion lt1, TypeUnknown ( OpenUnion, lt2))
      | (TypeUnion lt1, TypeUnknown ( OpenUnknown, lt2))
      | (TypeUnknown ( OpenUnion, lt2), TypeUnion lt1) 
      | (TypeUnknown ( OpenUnknown, lt2), TypeUnion lt1) ->
	  let ltl = merge lt1 lt2
	  in 
	    if (((List.length ltl) = (List.length lt1)) 
		& ((List.length ltl) >= (List.length lt2)))
	    then
	      TypeUnion ltl
	    else
	      raise (printf "TM 6\n"; Type_mismatch)

      | (TypeProduct lt1, TypeProduct lt2) ->
	  let ltl = merge lt1 lt2
	  in 
	    if (((List.length ltl) = (List.length lt1)) 
		& ((List.length ltl) = (List.length lt2)))
	    then
	      TypeProduct ltl
	    else
	      raise (printf "TM 7\n"; Type_mismatch)
      | (TypeProduct lt1, TypeUnknown ( OpenProduct, lt2))
      | (TypeProduct lt1, TypeUnknown ( OpenUnknown, lt2))
      | (TypeUnknown ( OpenProduct, lt2), TypeProduct lt1)
      | (TypeUnknown ( OpenUnknown, lt2), TypeProduct lt1)
	->
	  let ltl = merge lt1 lt2
	  in 
	    (* DEBUG *
	       printf "... merging types %s with %s gives %s\n"
	       (string_of_type_t t1)
	       (string_of_type_t t2)
	       (string_of_type_t (TypeProduct ltl));
	    ***)
	    if ((List.length ltl) = (List.length lt1)) 
	    then
	      TypeProduct ltl
	    else begin
	      raise (printf "TM 8\n"; Type_mismatch)
	    end
      | _ ->  
	  begin
	    raise (printf "TM 9\n"; Type_mismatch)
	  end
;;
let rec merge_type_with_pattern_rec types_ht alias_ht t p =
  let rec merge lt lp =
    match (lt,lp) with
	(_,[]) -> lt
      |	([],_) -> 
	    List.map 
	      (fun (l,p) -> 
		 (l, merge_type_with_pattern_rec types_ht alias_ht 
		    (TypeUnknown ( OpenUnknown,[])) p)
	      )
	      lp 
      | ((l1,t1):: lt1, (l2,p2)::lp2) ->
	  if (l1 < l2) then
	    (l1,t1) :: (merge lt1 lp)
	  else if (l2 < l1) then
	    (l2, 
	     merge_type_with_pattern_rec types_ht alias_ht 
	       (TypeUnknown ( OpenUnknown,[])) p2)
	    :: (merge lt lp2)
	  else (* (l1=l2) *)
	    (l1, 
	     merge_type_with_pattern_rec types_ht alias_ht 
	       t1 p2)
	    :: (merge lt1 lp2)
  in
  match p with
      PatAny -> t
    | PatType (pt) -> merge_types types_ht alias_ht t pt
    | PatProd (lp) ->
	begin
	  match t with
	      TypeName (tn) -> 
		(try 
		  merge_type_with_pattern_rec types_ht alias_ht  
		    (Hashtbl.find types_ht (unalias alias_ht tn))
		    p
		with Not_found ->
		  begin
		    eprintf "Unknown type name: \"%s\" \n" (unalias alias_ht tn);
		    raise Undefined_identifier
		  end)
	    | TypeUnknown ( OpenUnknown, lt )
	    | TypeUnknown ( OpenProduct, lt ) -> 
		let res = merge lt lp 
		in
		  if ((List.length res) = (List.length lp))
		  then TypeProduct res 
		  else (printf "E4\n";raise Type_mismatch)
	    | TypeProduct lt ->
		let res = merge lt lp 
		in
		  if (((List.length res) = (List.length lp)) &
			(List.length res) = (List.length lt))
		  then TypeProduct res 
		  else (printf "E0\n"; raise Type_mismatch)
	end
    | PatOpenProd (lp) ->
	begin
	  match t with
	      TypeName (tn) -> 
		(try 
		  merge_type_with_pattern_rec types_ht alias_ht  
		    (Hashtbl.find types_ht (unalias alias_ht tn))
		    p
		with Not_found ->
		  begin
		    eprintf "Unknown type name: \"%s\" \n" (unalias alias_ht tn);
		    raise Undefined_identifier
		  end)
	    | TypeUnknown ( OpenUnknown, lt )
	    | TypeUnknown ( OpenProduct, lt ) -> 
		let res = merge lt lp 
		in TypeUnknown (OpenProduct, res) 

	    | TypeProduct lt ->
		let res = merge lt lp 
		in
		  if ((List.length res) = (List.length lt))
		  then TypeProduct res 
		  else (printf "E1\n"; raise Type_mismatch)
	end
    | PatConst (lab,pp) ->
	match t with
	    TypeName (tn) -> 
	      (try 
		merge_type_with_pattern_rec types_ht alias_ht  
		  (Hashtbl.find types_ht (unalias alias_ht tn))
		  p
	      with Not_found ->
		begin
		  eprintf "Unknown type name: \"%s\" \n" (unalias alias_ht tn);
		  raise Undefined_identifier
		end)
	  | TypeUnknown (o, lt) ->
	       let res = merge lt [(lab,pp)] 
	       in
		 if (o = OpenProduct) then (printf "E3\n";raise Type_mismatch)
		 else TypeUnknown ( OpenUnion, res )
	  | TypeUnion (lt) ->
	      let res = merge lt [(lab,pp)]
	      in 
		if ((List.length res) = (List.length lt))
		then TypeUnion res
		else (printf "E2\n";raise Type_mismatch)
;;

let merge_type_with_pattern types_ht alias_ht t p =
  let res = merge_type_with_pattern_rec types_ht alias_ht t p
  in 
    (* printf "*  %s with pattern %s -> %s\n " 
    (string_of_type_t t) 
    (string_of_pat_t p)
    (string_of_type_t res)
    ; *)
    res
;;

let merge_type_with_pattern_list types_ht alias_ht t pl =
  List.fold_left
    (merge_type_with_pattern types_ht alias_ht)
    t
    pl
;;


let update_type_of_flat_value types_ht alias_ht (o,e) =
  ()
  (* returns (e') *)  
  (* raises exception Type_mismatch if type error *)
(*  let utype = TypeUnknown (OpenUnknown, [])
  and merge_types = merge_types types_ht alias_ht 
  and get_type_field = get_type_field types_ht alias_ht 
  and compare_types = compare_types types_ht alias_ht in
  let (_,old_o) = get_ftype e in
  let (new_o) = merge_types o old_o
  in
    match e with
	RelProduct( _, le, by_mod ) ->
	  begin
	    match new_i with
		TypeUnknown (OpenProduct, [])
	      | TypeUnknown (OpenUnknown, [])
	      | TypeProduct [] 
	      | TypeName "~1" 
		-> 
		  let res =
		    List.fold_left
		      (* 
			 res:list of (label,rel) - collected when folding
		      *)
		      (fun res (l,e) ->
			 let new_e =  
			   update_type rel_ht types_ht alias_ht env 
			     (TypeProduct [], get_type_field  new_o l, e) in
			 let (_,new_o_type) = get_ftype new_e
			 in
			   Hashtbl.add env ("@"^l) new_o_type ;
			    res @ [(l,new_e)]
		      )
		      []
		      le
		  in
		    begin  
		      List.iter
			(fun (l,_) -> 
			   Hashtbl.remove env ("@"^l))
			(List.rev le);
		      
		      let new_out_type =
			TypeProduct
			  (sort_by_label 
			     ( List.map 
				 (fun (l,e) -> let (_,tt) = get_ftype e in (l,tt))
				 res 
			     )
			  )
		      in 
			RelProduct(
			  (TypeProduct[], new_out_type),
			  res,
			  by_mod)
		    end
	      | _ -> raise (printf "TM 10\n"; Type_mismatch)
	  end   
      | RelLabel ( _, lab, e) -> 
	  begin
	    let new_e =  
	      update_type rel_ht types_ht alias_ht env 
		(new_i,get_type_field  new_o lab, e) 
	    in
	    let (t_i,_) = get_ftype new_e
	    in
	      RelLabel ((t_i,new_o),lab,new_e)
	  end *)
;;

let rec update_type rel_ht types_ht alias_ht env (i,o,e) =
  (* returns (e') *)  
  (* raises exception Type_mismatch if type error *)
  let utype = TypeUnknown (OpenUnknown, [])
  and merge_types = merge_types types_ht alias_ht 
  and get_type_field = get_type_field types_ht alias_ht 
  and compare_types = compare_types types_ht alias_ht in
  let (old_i, old_o) = get_ftype e in
  let (new_i, new_o) = (merge_types i old_i, merge_types o old_o)
  in
    match e with
	RelProduct( _, le, cl ) ->
	  begin
	    match new_i with
		TypeUnknown (OpenProduct, [])
	      | TypeUnknown (OpenUnknown, [])
	      | TypeProduct [] 
	      | TypeName "~1" 
		-> 
		  let res =
		    List.fold_left
		      (* 
			 res:list of (label,rel) - collected when folding
		      *)
		      (fun res (l,e) ->
			 let new_e =  
			   update_type rel_ht types_ht alias_ht env 
			     (TypeProduct [], get_type_field  new_o l, e) in
			 let (_,new_o_type) = get_ftype new_e
			 in
			   Hashtbl.add env ("@"^l) new_o_type ;
			    res @ [(l,new_e)]
		      )
		      []
		      le
		  in
		    begin  
		      List.iter
			(fun (l,_) -> 
			   Hashtbl.remove env ("@"^l))
			(List.rev le);
		      
		      let new_out_type =
			TypeProduct
			  (sort_by_label 
			     ( List.map 
				 (fun (l,e) -> let (_,tt) = get_ftype e in (l,tt))
				 res 
			     )
			  )
		      in 
			RelProduct(
			  (TypeProduct[], new_out_type),
			  res,
			  cl
			)
		    end
	      | _ -> raise (printf "TM 10\n"; Type_mismatch)
	  end   
      | RelSum ( _, es, by_mod ) ->
	  begin
	    let (t_i, t_o, res) =
	      List.fold_left
		(* t_i, t_o: the global type
		   res:list of rels - collected when folding
		*)
		(fun (t_i, t_o, res) e ->
		   let new_e =  
		     update_type rel_ht types_ht alias_ht env 
		       (t_i, t_o, e) 
		   in
		   let (t_i,t_o) = get_ftype new_e
		   in
		     (t_i, t_o, res @ [new_e] )
		)
		(new_i, new_o, [])
		es
	    in
	      RelSum ( (t_i,t_o), res, by_mod )
	  end
      | RelLabel ( _, lab, e) -> 
	  begin
	    let new_e =  
	      update_type rel_ht types_ht alias_ht env 
		(new_i,get_type_field  new_o lab, e) 
	    in
	    let (t_i,_) = get_ftype new_e
	    in
	      RelLabel ((t_i,new_o),lab,new_e)
	  end
      | RelAbst ( _, x, pat_list, e ) -> 
	  begin
	    let t_i =
	      merge_type_with_pattern_list types_ht alias_ht new_i pat_list
	    in
	    let new_e =  
	      Hashtbl.add env ("$"^x) t_i;
	      update_type rel_ht types_ht alias_ht 
		env
		(TypeProduct[], new_o, e)
	    in	
	    let (_,t_o) = get_ftype new_e
	    and t_i = merge_types (Hashtbl.find env ("$"^x)) t_i
	    in
	      Hashtbl.remove env ("$"^x);
	      RelAbst ((t_i,t_o),x,pat_list,new_e)
	  end 
      | RelProj ( _ , e, lab )  ->
	  begin 	    
	    let new_e =
	      let (_,ttt) = get_ftype e 
	      in
		update_type rel_ht types_ht alias_ht env 
		  (new_i,
		   (merge_types 
		      (TypeUnknown (OpenUnknown, [(lab, new_o)]))
		      ttt), (* we say that ttt has to be a type with field "lab" *)
		   e) 
	    in
	    let (t_i,tt_o) = get_ftype new_e
	    in
	    let t_o = merge_types new_o (get_type_field tt_o lab) 
	    in
	      RelProj((t_i,t_o), new_e, lab)
	  end
	    
      | RelComp ( _ , [e]) ->
	  let new_e =  
	    update_type rel_ht types_ht alias_ht env (new_i,new_o,e) 
	  in 
	  let (t_i,t_o) = get_ftype new_e
	  in
	    RelComp ((t_i,t_o), [new_e])
      | RelComp ( _ , e::le) -> 
	  (* THIS IS PROBABLY WRONG !!!! *)
	  begin
	    let (_,t_o) = get_ftype e in
	    let RelComp ((t2_i,t2_o),le2) =
	      update_type rel_ht types_ht alias_ht env 
		(t_o,new_o,RelComp ((t_o,new_o), le))
	    in 
	    let e3 =  
	      update_type rel_ht types_ht alias_ht env (new_i,t2_i,e) 
	    in 
	    let (t_i,_) = get_ftype e3 
	    in
	      RelComp ( (t_i,t2_o), e3::le2)
	  end
      | RelCall ( _ , r ) ->
	  let (t_i, t_o, _) = 
	    (try 
	      Hashtbl.find rel_ht r
	    with Not_found ->
	      begin
		eprintf "Unknown relation name: \"%s\" \n" r;
		raise Undefined_identifier
	      end)
	  in
	    RelCall	( (merge_types new_i t_i, merge_types new_o t_o), r )
	      
      | RelVar ( _ , x ) ->
	  if (Hashtbl.mem env ("$"^x)) then
	    let t_o = merge_types new_o (Hashtbl.find env ("$"^x)) 
	    in
	      Hashtbl.replace env ("$"^x) t_o;
	      RelVar ((merge_types new_i (TypeProduct []), t_o), x)
	  else
	    begin
	      eprintf "Unknown parameter name: $%s \n" x;
	      raise Undefined_identifier
	    end
      | RelField ( _ , f ) -> 
	  if (Hashtbl.mem env ("@"^f)) then
	    let t_o = merge_types new_o (Hashtbl.find env ("@"^f)) 
	    in
	      Hashtbl.replace env ("@"^f) t_o;
	      RelField ((merge_types new_i (TypeProduct []), t_o), f)
	  else
	    begin
	      eprintf "Unknown field name: @%s \n" f;
	      raise Undefined_identifier
	    end
      | RelExternal( _ , fifo ) -> RelExternal ((new_i, new_o), fifo)
;;



(************************)
  


let normalize_type types_ht alias_ht t =
  let rec is_resolved t =
    match t with
	TypeName tn -> true
      | TypeUnion lt  
      | TypeProduct lt -> List.for_all (fun (_,t) -> is_resolved t) lt 
      | _ -> false
  in
    if (is_resolved t) then
      match t with
	  TypeName _ -> t
	| TypeProduct [] -> TypeName "~1"
	| _ -> TypeName (add_with_new_name types_ht t)
    else raise Unresolved_type
;;



(*********************)
let rec replace_type_exp_by_names types_ht alias_ht e =
  let normalize_type = normalize_type types_ht alias_ht
  and replace_type_exp_by_names = replace_type_exp_by_names types_ht alias_ht in
    match e with
	RelProduct( (i,o), le, cl ) ->
	  RelProduct( 
	    (normalize_type i,normalize_type o),
	    (List.map (fun (l,e) -> (l, replace_type_exp_by_names e)) le),
	    cl
	  )
      | RelSum ( (i,o), es, by_mod ) ->
	  RelSum ( 
	    (normalize_type i,normalize_type o),
	    List.map (replace_type_exp_by_names) es, 
	    by_mod 
	  )
      | RelLabel ( (i,o), lab, e) -> 
	  RelLabel (
	    (normalize_type i,normalize_type o),
	    lab,
	    replace_type_exp_by_names e
	  )
      | RelAbst ( (i,o), x, pat, e ) -> 
	  RelAbst (
	    (normalize_type i,normalize_type o),
	    x,pat, 
	    replace_type_exp_by_names e
	  )
      | RelProj ( (i,o) , e, lab )  ->
	  RelProj(
	    (normalize_type i,normalize_type o), 
	    replace_type_exp_by_names e,
	    lab)
      | RelComp ( (i,o) , le) -> 
	  RelComp (
	    (normalize_type i,normalize_type o), 
	    List.map (replace_type_exp_by_names) le
	  )
      | RelCall ( (i,o) , r ) ->
	  RelCall ( (normalize_type i,normalize_type o), r )
      | RelVar ( (i,o) , x ) ->     
	  RelVar ((normalize_type i,normalize_type o), x)
      | RelField ( (i,o) , f ) -> 
	  RelField ((normalize_type i,normalize_type o), f)
      | RelExternal ( (i,o), fifo ) -> 
	  RelExternal ((normalize_type i,normalize_type o), fifo)
;;

(**********************)



	  

let typing rel_ht types_ht alias_ht =
  begin
    let changed = ref true
    and context = Hashtbl.create 100
    in
      while (!changed) do
	changed := false;
	Hashtbl.iter
	  (fun n (i,o,e) ->
	     
(* DEBUG: *
	     printf "%d \n ===TYPING EXPRESSION: %s : %s -> %s\n - BEFORE: %s\n"
	       n (string_of_type_t i) (string_of_type_t o)
	       (string_of_rel_t true e);
***)    
	     
	     let new_e = 
	       update_type 
		 rel_ht types_ht alias_ht 
		 context 
		 (i,o,e)
	     in
	
(* DEBUG: *
	       printf " - AFTER : %s\n" (string_of_rel_t true new_e);
***)      
	       changed := (!changed || (not (e = new_e)));
	       
	       Hashtbl.replace  rel_ht n (i,o,new_e)
	  )
	  rel_ht
      done;
  end;

  (* check if all types are fully resolved and replace expressions by names  
     1 . we have to get rid of alias_ht by moving it into types_ht in order to
         avoid duplicates!!!
     2. and then we check and add new names!
  *)

  
  Hashtbl.iter 
    (fun a n -> Hashtbl.add types_ht a (TypeName n))
    alias_ht;

  Hashtbl.clear alias_ht;
 
  Hashtbl.iter
    (fun n (i,o,e) ->
       let new_e = replace_type_exp_by_names types_ht alias_ht e
       in
	 Hashtbl.replace rel_ht n (i,o,new_e)
    )
    rel_ht;
  
(** DEBUG * 
  
  Hashtbl.iter
    (fun n (i,o,e) ->
       
       printf "\n ===TYPING EXPRESSION: %s : %s -> %s\n  %s\n" 
	 n (string_of_type_t i) (string_of_type_t o)
	 (string_of_rel_t true e);
    )
    rel_ht
***)
  
;;



(*************************************** TUTAJ ****)
(*********************)


let clean_aliases rel_ht types_ht alias_ht =
 (* eliminates useless type names and unalias types in expressions*)
  let un (TypeName name) = TypeName (Typegraph.unalias alias_ht name) 
  in let rec unalias_typed_exp e =
      match e with
	  RelProduct( (i,o), le, cl ) ->
	    RelProduct( 
	      (un i,un o),
	      (List.map (fun (l,e) -> (l, unalias_typed_exp e)) le),
	      cl
	    )
	| RelSum ( (i,o), es, by_mod ) ->
	    RelSum ( 
	      (un i,un o),
	      List.map unalias_typed_exp es, 
	      by_mod 
	    )
	| RelLabel ( (i,o), lab, e) -> 
	    RelLabel (
	      (un i,un o),
	      lab,
	      unalias_typed_exp e
	    )
	| RelAbst ( (i,o), x, pat, e ) -> 
	    RelAbst (
	      (un i, un o),
	      x,pat, 
	      unalias_typed_exp e
	    )
	| RelProj ( (i,o) , e, lab )  ->
	    RelProj(
	      (un i, un o), 
	      unalias_typed_exp e,
	      lab)
	| RelComp ( (i,o) , le) -> 
	    RelComp (
	      (un i,un o), 
	      List.map unalias_typed_exp le
	    )
	| RelCall ( (i,o) , r ) ->
	    RelCall	( (un i,un o), r )
	| RelVar ( (i,o) , x ) ->     
	    RelVar ((un i, un o), x)
	| RelField ( (i,o) , f ) -> 
	    RelField ((un i,un o), f)
	| RelExternal ( (i,o), fifo) -> 
	    RelExternal ((un i,un o), fifo)
  in    
    Hashtbl.iter
      (fun n (i,o,e) ->
	 Hashtbl.replace  rel_ht n (un i, un o, unalias_typed_exp e)
      )
      rel_ht;

    Hashtbl.iter
      (fun a t ->
	 if (a.[0]='~') then
	   Hashtbl.remove alias_ht a
      )
      alias_ht


;;

