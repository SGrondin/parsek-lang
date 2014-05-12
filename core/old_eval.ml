open Prints
open Typing
open Exp_parser
open Lexing

exception Incompatible_argument
exception Position_error
exception In_Out_channel
exception Unknown_modifier

let relUnit = RelProduct ((TypeName "__unit", TypeName "__unit"), [], "");;
let relUndefined = RelSum ((TypeName "__unit", TypeName "__unit"), [], "");;
let bogus_type = (TypeName "bogus");;
let bogus_ftype = (bogus_type,bogus_type);;

(* external functions are just files: history/look-ahead of every 
   channel activity is memorized *)
let in_channel_ht = Hashtbl.create 10;; 
let out_channel_ht = Hashtbl.create 10;;

(* Environment is defned as a map from fifos to positions. *)
module Env = Map.Make(String);; 


let merge_env x y =
 Env.fold
   (fun key data res ->
      (*DEBUG 
	Printf.printf "... merging (%s,%d) ... \n" key data; 
      *)

      if (Env.mem key res) then
	( if (Env.find key res) < data then
	    Env.add key data res
	  else
	    res
	)
      else
	Env.add key data res
   )
   x
   y
;;

(* it is not an order !, returns true iff x is greater or equal to y *)
let ge_env x y =
  let diff_x_y = 
    Env.fold
      (fun key_y data_y res ->
	let data_x =
	  if (Env.mem key_y x) then
	    Env.find key_y x
	  else
	    0
	in
	Env.add key_y (data_x - data_y) res
      )
      y (* for every element (key_y data_y)from y *)
      x (* initially result is x *)
  in
   Env.fold
    (fun _ data res ->
      if (data < 0) then false
      else res
    )
    diff_x_y
    true
;;




let system_call (t_i,t_o) arg name = 
  if (is_unit_type t_i) then
    let fifo_in =
      if not (Hashtbl.mem in_channel_ht name) then
	Hashtbl.add in_channel_ht name (Lexing.from_channel (open_in name));
      Hashtbl.find in_channel_ht name
    in 	
      try
	Exp_parser.kvalue_SC  Exp_lexer.token fifo_in
      with
	  _ ->  relUndefined
  else if (is_unit_type t_o) then
    let fifo_out =
      if not (Hashtbl.mem out_channel_ht name) then
	Hashtbl.add out_channel_ht name (open_out name);
      Hashtbl.find out_channel_ht name
    in 	    
      output_string fifo_out ((string_of_rel_t false arg) ^" ;\n");
      flush fifo_out;
      relUnit
  else raise In_Out_channel
;;

let rec match_with_pattern arg pat =
  match pat with
    PatAny 
  | PatType (_) -> true
  | PatProd lp ->
      begin
	match arg with
	  RelProduct (_,la,_) ->
	    List.for_all2 
	      (fun (_,aa)  (_,pp) ->
		match_with_pattern aa pp )
	      (sort_by_label la)
	      lp (* the list is sorted during parsing *)
      end
  | PatOpenProd lp ->  
      begin
	match arg with
	  RelProduct (_,la,_) ->
	    begin
	      match ((sort_by_label la),lp) with
		(_,[]) -> true
	      | ((al,a)::lla,(pl,p)::llp) ->
		  if (al = pl) then
		    ( (match_with_pattern a p) &&
		      (match_with_pattern 
			 (RelProduct (bogus_ftype,lla,""))
			 (PatOpenProd llp) )
		     )
		  else 
		    (match_with_pattern 
		       (RelProduct (bogus_ftype,lla,""))
		       (PatOpenProd lp) )
	    end      
      end
  | PatConst (lp,p) ->
      begin
	match arg with
	  RelLabel (_,la,a) ->
	    if (la = lp) then match_with_pattern a p
	    else false
      end
;;

let match_with_pattern_list arg pat_list =
  List.exists 
    (match_with_pattern arg)
    pat_list
;;

let evaluate rel_ht type_ht taliases_ht e =
  let context = Hashtbl.create 100 and
      journal_ht =  Hashtbl.create 50 in
  let external_call env arg name =
    let history =
      if (Hashtbl.mem journal_ht name) then Hashtbl.find journal_ht name else []
    and position =
      if (Env.mem name env) then Env.find name env else 0 
    in
      if (position < (List.length history)) then
	begin
	  let (arg_at_position, res_at_position) = List.nth history position in
	    if (arg = arg_at_position) then 
	      (Env.add name (position +1) env, res_at_position)
	    else raise Incompatible_argument
	end
      else 
	if ( (List.length history) < position) 
	then raise Position_error 
	else
	  let (t_i,t_o,_) = Hashtbl.find rel_ht name in
	  let new_res = (system_call (t_i,t_o) arg name) in
	    Hashtbl.replace journal_ht name (List.append history [(arg,new_res)]);
	    (Env.add name (position +1) env, new_res)
  in
  let rec eval env arg expr = (* returns (env, expr) *)
    match arg with 
	RelSum (_,[],_) -> (env, relUndefined)
      | _ ->
	  begin
	    match expr with
		RelProduct ((t_i,t_o), lp ,by ) -> 
		  begin
		    begin (* arg should be unit! *)
		      match arg with
			  RelProduct (_,[],"") -> ()
		    end;
		    match by with
			"INTERSECT" | "INTER" ->
			  let (new_env, exprs) = 
			    List.fold_left
			      (fun (aux_env, aux_expr) (li,ei) ->
				 let (n_env,n_ei) = eval env arg ei in
				   begin
				     Hashtbl.add context ("@"^li) n_ei;
				     (merge_env n_env aux_env, aux_expr @ [(li,n_ei)]) 
				   end)
			      (env,[])
			      lp
			  in
			    begin
			      List.iter 
				(fun (li, _) -> Hashtbl.remove context ("@"^li) )
				(List.rev lp);
			      if 
				(List.exists 
				   (fun (li,ei) ->
				      match ei with
					  RelSum (_,[],_) -> true
					| _ -> false)
				   exprs)
			      then 
				(env,RelSum ((t_i,t_o), [], "" ))
			      else
				(new_env, RelProduct ((t_i,t_o), exprs ,"" )) 
			    end
			      
		      (* for the moment all other modifiers are treated the 
			 same as CONCAT *)
		      | "CONCAT" | "CONCATENATE" | "" -> 
			  let (new_env, exprs) = 
			    List.fold_left
			      (fun (aux_env, aux_expr) (li,ei) ->
				 let (n_env,n_ei) = eval aux_env arg ei in
				   begin
				     Hashtbl.add context ("@"^li) n_ei;
				     (n_env, aux_expr @ [(li,n_ei)]) 
				   end)
			      (env,[])
			      lp
			  in
			    begin
			      List.iter 
				(fun (li, _) -> Hashtbl.remove context ("@"^li) )
				(List.rev lp);
			      if 
				(List.exists 
				   (fun (li,ei) ->
				      match ei with
					  RelSum (_,[],_) -> true
					| _ -> false)
				   exprs)
			      then 
				(env,RelSum ((t_i,t_o), [], "" ))
			      else
				(new_env, RelProduct ((t_i,t_o), exprs ,"" )) 
			    end
		      | _ -> raise Unknown_modifier
		  end
	      | RelSum ((t_i,t_o),le,by) -> 
		  begin
		    match by with
			(* for the moment all sum modifiers are 
			   treated the same as FIRST_MATCH*)
		      	"SHORTEST" | "SHORTEST_MATCH" ->
			  begin
			    let all_res = 
			      List.filter
				(fun (_,res) -> 
				   match res with 
				       RelSum (_,[],_) -> false
				     | _ -> true)
				(List.map (eval env arg) le) 
			    in
			      match all_res with
				  [] -> (env,RelSum ((t_i,t_o),[],""))
				| hh :: ll -> 
				    List.fold_left
				      (fun (new_env,new_res) (env_i,res_i) ->
					 if (ge_env env_i new_env ) then
					   (new_env,new_res)
					 else if (ge_env new_env env_i) (* but not equal ! *)
					 then
					   (env_i,res_i)
					 else (* when incomparable *)
					   (new_env,new_res)
				      )
				      hh
				      ll
			  end
		      | "LONGEST" | "LONGEST_MATCH"-> 
			  begin
			    let all_res = 
			      List.filter
				(fun (_,res) -> 
				   match res with 
				       RelSum (_,[],_) -> false
				     | _ -> true)
				(List.map (eval env arg) le) 
			    in
			      match all_res with
				  [] -> (env,RelSum ((t_i,t_o),[],""))
				| hh :: ll -> 
				    List.fold_left
				      (fun (new_env,new_res) (env_i,res_i) ->
					 if (ge_env env_i new_env) then
					   (env_i,res_i)
					 else
					   (new_env,new_res)
				      )
				      hh
				      ll
			  end
		      | "FIRST" | "FIRST_MATCH" | "" ->
				  begin
				    match le with
					[] -> (env,RelSum ((t_i,t_o),[],""))
				      | ei::lle ->
					  let (new_env, new_expr) = eval env arg ei in
					    match new_expr with
						RelSum (_,[],_) -> eval env arg (RelSum ((t_i,t_o),lle,by))
					      | _ -> (new_env, new_expr)
				  end
		      | _ -> raise Unknown_modifier
		  end
	      | RelAbst ((t_i,t_o),x,pl,d) -> 
		  if (match_with_pattern_list arg pl) 
		  then 
		    begin
		      Hashtbl.add context ("$"^x) arg;
		      let res = eval env relUnit d in
			begin
			  Hashtbl.remove context ("$"^x);
			  res
			end
		    end
		  else
		    (env,relUndefined)
	      | RelProj ((t_i,t_o),e,l) -> 
		  begin
		    let (new_env, aux) = (eval env arg e) in
		      match aux with
			  RelProduct (_,le,"") -> 
			    begin
			      match (List.filter (fun (lab,ee) -> (l = lab)) le) with
				  [(_,new_expr)] -> (new_env, new_expr)
			    end
			| RelLabel (_,lab,new_expr) ->
			    if (lab = l) then
			      (new_env,new_expr)
			    else
			      (env, RelSum ((t_i,t_o),[],""))
		  end
	      | RelLabel ((t_i,t_o),l,e) -> 
		  begin
		    let (new_env,aux) = (eval env arg e) in
		      match aux with
			  RelSum (_,[],_) -> (env, RelSum ((t_i,t_o),[],""))
			| _ -> (new_env, RelLabel ((t_i,t_o),l,aux))
		  end
	      | RelComp ((t_i,t_o),le) -> 
		  begin
		    match le with
			[] -> (env,arg)
		      | ee :: lee ->
			  let (new_env, new_arg) = (eval env arg ee) in
			    (* maybe we have to reset env if failure ??? *)
			    eval new_env new_arg (RelComp ((bogus_type,t_o),lee))
		  end
	      | RelVar ((t_i,t_o),n) -> 
		  begin
		    begin (* arg should be unit! *)
		      match arg with
			  RelProduct (_,[],"") -> ()	       
		    end;
		    (env, Hashtbl.find context ("$"^n))
		  end
	      | RelField ((t_i,t_o),n) -> 
		  begin
		    begin (* arg should be unit! *)
		      match arg with
			  RelProduct (_,[],"") -> ()	       
		    end;
		    (env, Hashtbl.find context ("@"^n))
		  end
	      | RelCall ((t_i,t_o),n) -> 
		  begin
		    let (_,_,aux) = Hashtbl.find rel_ht n
		    in
		      match aux with
			  RelExternal _ -> 
			    begin
			      external_call env arg n 
			    end
			| _ -> eval env arg aux
		  end
	  end
  in
    (* every external call is memorized in the hash table called
       journal_ht. The data associated with a key is a history of the
       calls, represented by the list of value pairs for the argument and
       the result.
       
       Environment at a given time is represented by a map 
       (ext_fun_name, last_committed_position). 
    *)
    Hashtbl.clear journal_ht;
    let (env,result) = eval Env.empty relUnit e
    in
      result
;;

