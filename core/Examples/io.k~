/* An example of Kernel input/output:

   In the interpreter, any relation declaration with definition
   <input> or <output> is considered "external relation
   declaration"; an <input> relation has to be a constant, e.g.,

   ({} -> MyType) rel = <input>;

   and it is implemented as a read from file "rel" in the current
   working directory: the file must exists!

   Otherwise, we assume that "rel" is an assynchronous "send" (image
   type being "unit"), e.g.,

   (MyType -> {}) rel = <output>;

   which is implemented as a write to file "rel" in the current
   working directory (the file will be created or overwritten).

   External synchronous "read-writes" are not implemented in the
   interpreter! The call to a so called "In_Out_channel" results in
   the exception "Eval.In_Out_channel".     */

<type> char <is> ['a,'b,'c,'eof]; 
       /* for simplicity, our char type consists of three letters and a
         "End-Of-File" marquer */

/* These are two "external functions": */
char getchar = <input>;            /* reads a "char" from file "getchar" */
(char -> {}) putchar = <output>;   /* writes a "char" to file "putchar" */

/* we define a variation of "putchar", which returns its argument */
(char -> char) logchar = [('eof -> 'eof), 
			  (... -> $ :: putchar :: $)];

/* we define new type, the list of chars, and a relation "read_char_list" 
   which will read the whole content of "getchar" till 'eof */
<type> char_list <is> ['eof, 'a char_list, 'b char_list, 'c char_list];
char_list read_char_list =
	  getchar :: logchar :: 
               [ ('eof -> 'eof), 
	         ('a -> 'a read_char_list),
	         ('b -> 'b read_char_list),
	         ('c -> 'c read_char_list)];

/* gets a list of char through "getchar" of the length equal to the length of
   its argument (only a's) (or shorter if getchar generates 'eof) */

(char_list -> char_list) get_char_list_2 =
	   [ ('eof -> 'eof),
	     ($x 'a ... -> getchar :: 
                [('eof -> 'eof),
	         ('a -> 'a [$x.a :: get_char_list_2]),
	         ('b -> 'b [$x.a :: get_char_list_2]),
	         ('c -> 'c [$x.a :: get_char_list_2]) ]	
		 )];

/* unary encoded numbers using "char_list" */
char_list _0 = 'eof;
char_list _1 = 'a _0;
char_list _2 = 'a _1;
char_list _3 = 'a _2;


/* the following relation concatenates two char_lists */
({'x char_list, 'y char_list} -> char_list) 
     concat_char_list = 
     [ ( {'x 'eof, ...} -> $.y),
       (... ->  [ 'a [{'x $.x.a, 'y $.y} :: concat_char_list], 
                  'b [{'x $.x.b, 'y $.y} :: concat_char_list],
	  	  'c [{'x $.x.c, 'y $.y} :: concat_char_list]
		 ])];


/* the test program which reads "getchar". It returns three field product: 
   'one - this subfield consists of three field product again:
   	'fm - first match of 2, 1, and 3 initial words of "getchar"
	'lm - longest match of the same
	'sm - shortest match of the same three initial words
   'two - the rest of the content of "getchar" file
   'all - the whole "getchar" file. 
 */ 
{'one 
   {'fm char_list,
    'lm char_list,
 '   sm char_list }, 
 'two char_list, 
 'all char_list}  main =
      {'one {'fm
		[ _2 :: get_char_list_2,
		  _1 :: get_char_list_2,
		  _3 :: get_char_list_2],
	     'lm
		[ _2 :: get_char_list_2,
		  _1 :: get_char_list_2,
		  _3 :: get_char_list_2 
		  <longest> getchar ],
	     'sm
		[  _2 :: get_char_list_2,
		   _1 :: get_char_list_2,
		   _3 :: get_char_list_2 
		  <shortest>  getchar ]}, 
       	 'two read_char_list, 
       	 'all {'x @one.lm, 'y @two} :: concat_char_list
	 <serialize> getchar  
	 };


