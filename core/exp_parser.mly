%{
  

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

%}
  
  
%token <string> NAME
%token LC LP RP RC SC COMMA QUOTE 
%token UNKNOWN 

%left QUOTE


%start kvalue_SC 
%type <Prints.rel_t> kvalue kvalue_SC  
%type <(string * Prints.rel_t) list> list_of_label_inv non_empty_list_of_label_inv


%% /* Grammar rules and actions follow */

kvalue_SC: 
  kvalue SC { $1 }
  ;
  
  kvalue:
    LC list_of_label_inv RC 
    { RelProduct (no_ftype,$2,[]) }
  |   QUOTE NAME kvalue %prec QUOTE
      { RelLabel (no_ftype,$2,$3) }
  |   QUOTE NAME  
      { RelLabel (no_ftype,$2,RelProduct(no_ftype, [ ],[] )) }
  ;
  
  list_of_label_inv:
  /* empty */ { [ ] }
  |   non_empty_list_of_label_inv
      { $1 }
  ;
  
  non_empty_list_of_label_inv:  
    QUOTE NAME { [ ( $2, RelProduct( no_ftype, [ ],[] ) ) ] }
  |   QUOTE NAME kvalue
      { [($2, $3)] }
  |   QUOTE NAME COMMA non_empty_list_of_label_inv
      { ($2,  RelProduct(no_ftype, [ ],[] ) ) :: $4 }
  |   QUOTE NAME kvalue COMMA non_empty_list_of_label_inv  
      { ($2, $3) :: $5 }
  ;


%%
  
