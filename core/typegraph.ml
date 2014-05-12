open Prints
open Printf

(* ~0 = [], ~1 = {} *) 
let gen_name_counter = ref 2;; 

let are_bisimilar part_ht graph_ht n1 n2 = 
  let (t1,t2) =
    (Hashtbl.find graph_ht n1, Hashtbl.find graph_ht n2) 
  in
    match (t1,t2) with
	(TypeProduct (lt1), TypeProduct (lt2)) 
      | (TypeUnion (lt1), TypeUnion (lt2)) ->
	  if (List.length lt1) = (List.length lt2) then
	    List.fold_left2 
	      (fun res (l1,TypeName(tn1)) (l2,TypeName(tn2)) -> 
		 res & (l1 = l2) 
		 & (StringSet.mem tn1 
		      (Hashtbl.find part_ht tn2))
	      )
	      true
	      lt1
	      lt2
	  else false
      | (TypeName (t1), TypeName(t2)) ->  t1 = t2 
      | _ -> false
;;




let add_with_new_name htab data = 
      let gen_key = ("~"^(string_of_int !gen_name_counter))
      in
	gen_name_counter := !gen_name_counter + 1;
	Hashtbl.add htab gen_key data;
	gen_key
;;


let unalias alias_ht name =
  let rec rec_unalias n seen = 
    if (Hashtbl.mem alias_ht n) 
    then 
      if (StringSet.mem n seen) then 
	begin
	  Printf.eprintf
	    "TYPE_ERROR: Circular definition between types: \"%s\"!\n"
	    (String.concat "\",\"" (StringSet.elements seen));
	  raise Circular_type
	end
      else
	let result = 
	  rec_unalias 
	    (Hashtbl.find alias_ht n) 
	    (StringSet.add n seen)
	in
	  begin
	    (Hashtbl.replace alias_ht n result);
	    result
	  end
    else 
      n
  in
    rec_unalias name (StringSet.empty)
;;

(* Eliminate all undirect references (aliases) from ht_flat_types
   with respect to ht_alias and detects undefined types *)
let unalias_flat_type_ht ht_flat_types ht_alias =
  Hashtbl.iter
    (fun n t ->
       let local_unalias lt =
	 (List.map
	    (fun (l, TypeName(n)) -> 
	       let u = unalias ht_alias n
	       in
		 if (Hashtbl.mem ht_flat_types u)
		 then
		   (l, TypeName(unalias ht_alias n))
		 else begin
		   Printf.eprintf "TYPE_ERROR: Undefined type: \"%s\"!\n (from def of %s = %s \n" u n (string_of_type_t t);
		   raise Not_found_type
		 end
	    )
	    lt
	 )
       in
	 match t with
	     TypeUnion (lt) ->
	       Hashtbl.replace
		 ht_flat_types n 
		 (TypeUnion (local_unalias lt))
	   | TypeProduct (lt) ->
	       Hashtbl.replace
		 ht_flat_types n 
		 (TypeProduct (local_unalias lt))
    )
    ht_flat_types  
;;


(* ht_type and ht_alias will be rewritten! *)

let normalize_types ht_types ht_alias =
  let ht_part1 = Hashtbl.create 100 
  and ht_part2 = Hashtbl.create 100 
  and ht_flat_types = Hashtbl.create 100 
  and are_partitions_the_same ht1 ht2 = 
    Hashtbl.fold
      (fun a b res ->
	if (Hashtbl.mem ht2 a) 
	then ( res & (0 = StringSet.compare  (Hashtbl.find ht2 a) b) )
	else false)
      ht1
      true
  in 
  let rec flatten_lt_list lt =
    List.map
      (fun (l,t) ->
	 match t with
	     TypeName (_) -> (l,t)
	   | TypeUnion (lt) ->
	       let new_name = add_with_new_name ht_flat_types (TypeUnion (flatten_lt_list lt))
	       in 
		 (l,TypeName (new_name))
	   | TypeProduct (lt) -> 
	       let new_name = add_with_new_name ht_flat_types (TypeProduct (flatten_lt_list lt))
	       in 
		 (l,TypeName (new_name))
      )
      lt
  in
    begin
      (* initialize "ht_flat_types" and "ht_alias" *)

      Hashtbl.clear ht_alias; 

      Hashtbl.iter
	(fun  n t ->
	   match t with
	       TypeName (name) -> 
		 Hashtbl.add ht_alias n name;
	     | TypeUnion (lt) ->
		 Hashtbl.add ht_flat_types n (TypeUnion (flatten_lt_list lt))
	     | TypeProduct (lt) ->
		 Hashtbl.add ht_flat_types n (TypeProduct (flatten_lt_list lt))
	)
	ht_types
      ;

(*DEBUG *
      print_string (string_of_type_ht ht_flat_types "\n --- Flatten types\n");
      print_string (string_of_alias_ht ht_alias "\n --- Initial aliases \n");
***)
    
      (* unalias *
      unalias_flat_type_ht ht_flat_types ht_alias;

* DEBUG *
      print_string 
	(string_of_type_ht ht_flat_types "\n --- Unaliassed flatten types\n");
***)

      (* Generate the set of all types and initialize the partition *)
      begin
	let all_type_name_set = 
	  (Hashtbl.fold
	     (fun n _ res ->
		StringSet.add n res )
	     ht_flat_types
	     StringSet.empty)
	in
	  StringSet.iter
	    (fun x -> Hashtbl.add ht_part1 x all_type_name_set)
	    all_type_name_set
      end
      ;
	
      (* iteration "till convergence" starts here *)  
      Hashtbl.clear ht_part2;
      Hashtbl.iter
	(fun s p ->
	   Hashtbl.replace ht_part2 
	     s 
	     (StringSet.filter (are_bisimilar ht_part1 ht_flat_types s) p )
	)
	ht_part1;
      while (not (are_partitions_the_same ht_part1 ht_part2)) do
	Hashtbl.clear ht_part1;
	Hashtbl.iter 
	  (fun a b -> Hashtbl.add ht_part1 a b)
	  ht_part2;
	Hashtbl.clear ht_part2;
	Hashtbl.iter
	  (fun s p ->
	     Hashtbl.replace ht_part2 
	       s 
	       (StringSet.filter (are_bisimilar ht_part1 ht_flat_types s) p )
	  )
	  ht_part1;
      done
      ;

(*
      print_string (string_of_part_ht ht_part1 "\n --- Partition:\n");
*)
      (* move the final partition into aliasses 
         the "min_elt" of a partition become its representative *)

      Hashtbl.iter
	(fun n s ->
	   let e = StringSet.min_elt s
	   in
	     if (n = e) then ()
	     else Hashtbl.add ht_alias n e)
	ht_part1
      ;
      
      unalias_flat_type_ht ht_flat_types ht_alias;
      
      (* rewrite original type_ht *)
      Hashtbl.clear ht_types;
      Hashtbl.iter 
	(fun a b ->
	   if (not (Hashtbl.mem ht_alias a)) then 
		 Hashtbl.add ht_types a b)
	ht_flat_types;

      (* flatten the aliases *)
      Hashtbl.iter 
	(fun a _ -> let _ = unalias ht_alias a in ())
	ht_alias


    end
;;

