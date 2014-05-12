/* Example of unary encoding of natural number with
   addition (add) and multiplication (mult) */

-- assumes that file "get_unat" is in the working directory

/* recursive type definition */
<type> unat <is> ['o, 'i unat ];

/* few natural numbers in our encoding */
unat zero = 'o;
unat one = 'i'o; 
unat two ='i'i'o;
unat three = 'i'i'i'o;
/* the constructor 'i of "unat" acts as "increment" */
unat four = 'i three;

/* Since every kernel relation is binary (takes one argument), we
   introduce a new <type> "upair" being a pair of unat's. The type
   will be the argument for binary operations "add" and "mult". */

<type> upair <is> {'x unat, 'y unat};

/* Addition is defined recursively, as the union (merge) of relations */
(upair -> unat) add =
	[ 
	  /* if 'x = 0 and 'y is of any value (represented by "..."),
	     then return 'y. */

          ($arg {'x 'o , 'y ...} -> $arg.y),
	   
	  /* if 'x = 1 + z, then return (1 + (z + y)). Notice, that
	     the default name of the argument is the empty string,
	     thus we write $.y in order to refer to the field "y" of
	     the actual argument. */

          ({'x 'i ..., ...} -> 'i [{'x $.x.i, 'y $.y} :: add] )

	  /* A list of relations within square brackets represents in
	     general the union of the relations. However, we can use
	     the square brackets around a single relation to force a
	     particular grouping (i.e., as parenthesis). 

	     An expression ['i f :: g] is parsed as [['i f] :: g]
	     since label constructor binds stronger than composition.
	     Thus, in the above definition we cannot omit the square
	     brackets. Thus,

	     ({'x 'i ..., ...} -> 'i {'x $.x.i, 'y $.y} :: add )

	     would result in a type mismatch error. */
 ];

/* Definition of the multiplication is very similar to
   addition. Notice, that we use an anonymous type "{'x unat, 'y
   unat}" as the argument instead of "upair".  They are equivalent. */

({'x unat, 'y unat} -> unat) mult =
	[
          ({'x 'o , ...} -> 'o),
          ( ... -> {'x [{'x $.x.i , 'y $.y} :: mult], 'y $.y}) :: add
        ];

/* the following relation "test" takes a natural number n and produces
   as 4-uplet consisting of {'x=n, 'twice=n+n, 'four_times=n+n+n+n,
   'square=n*n } */
<type> test <is> {'x unat, 'twice unat, 'four_times unat, 'square unat};

(unat -> test)
test = ( ... ->
           {'x $, 	   
	    'twice {'x $, 'y $} :: add,
      	    'four_times {'x @twice, 'y @x} :: add,
	      /* We may refer to the value of one of the previous fields
	         using its name prefixed by "@" */
            'square {'x $, 'y $} :: mult}
	);

/* We communicate with a kernel program through "pipes", sometimes
   called FIFOs (first-in-first-out). There are two kings of FIFOs,
   for inputs and for outputs. They can be declared in the following
   way: */

unat get_unat = <input>; 
(test -> {}) print_unat = <output>;

/* By default, the top level (starting) relation is called "main" */

main =  get_unat :: [('o -> {}), test :: print_unat :: main];

/*********************************************************** 

  Our program reads a "unat" (e.g., 'i'i'o;) from input pipe
  "get_unat" and write the result of the test, i.e.,

  {'x 'i 'i 'o {}, 
   'twice 'i 'i 'i 'i 'o {}, 
   'four_times 'i 'i 'i 'i 'i 'i 'i 'i 'o {}, 
   'square 'i 'i 'i 'i 'o {}};

  to the output pipe "print_unat". Under UNIX, those pipes 
  are just ordinary files (of the same names) located in 
  the working directory.

**************************************************************/ 



