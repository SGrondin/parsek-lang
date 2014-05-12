<type> unat <is> ['o, 'i unat ];

unat zero = 'o;
unat one = 'i'o; 
unat two ='i'i'o;
unat three = 'i'i'i'o;
unat four = 'i three;

<type> upair <is> {'x unat, 'y unat};

//; Addition is defined recursively, as the union (merge) of relations
(upair -> unat) add =
	[ 
	  
          ($arg {'x 'o , 'y ...} -> $arg.y),
	
          ({'x 'i ..., ...} -> 'i [{'x $.x.i, 'y $.y} :: add] )

 ];

({'x unat, 'y unat} -> unat) mult =
	[
          ({'x 'o , ...} -> 'o),
          ( ... -> {'x [{'x $.x.i , 'y $.y} :: mult], 'y $.y}) :: add
        ];

<type> test <is> {'x unat, 'twice unat, 'four_times unat, 'square unat};

(unat -> test)
test = ( ... ->
           {'x $, 	   
	    'twice {'x $, 'y $} :: add,
      	    'four_times {'x @twice, 'y @x} :: add,
	      /*; We may refer to the value of one of the previous fields
	        ; using its name prefixed by "@" */
            'square {'x $, 'y $} :: mult}
	);


unat get_unat = <input>; 
(test -> {}) print_unat = <output>;

<type> a <is> ['a, 'b ];

a get_a = <input>; 
(a -> {}) put_a = <output>;

({} -> a) get_a_ = 'a {};


(unat -> unat) factorial = 
  [('o -> 'i'o)
  ,(... -> {'x $, 'y $.i | factorial} | mult)
  ]
;

(a -> a) inc_a = 
  [('a {} -> 'b {})
  ,('b {} -> 'a {})
  ];

<code> xya <is> {'x a, 'y a};

(a -> {}) f = ('a {} -> {});
(a -> {}) g = 
  [f
  ,inc_a | g
  ]
;

<code> boolean <is> ['true, 'false];
<code> boolean_tree <is> ['leaf, 'node];
<code> node <is> {'l boolean_tree, 'v boolean, 'r boolean_tree};

<code> cross <is> ['x cross_1, 'y cross_2];
<code> cross_1 <is> ['x cross_2];
<code> cross_2 <is> ['y cross_1];

{'a unat} h =
  {'a zero
  }
;

(unat -> unat) succ = (... -> 'i $);

<code> natpair <is> {'x unat, 'y unat};

(natpair -> natpair) succboth = (... ->
    {'x $.x | succ
    ,'y $.y | succ
    }
  )
;

(natpair -> natpair) succboth2 = (... ->
    {'x $.x | succ
    ,'y $.y | (... -> $ | succ)
    }
  )
;

(natpair -> natpair) succboth3 = 
  [[]
  ,[[[[]]]]
  ,[]
  ,(... ->
    {'x $.x | succ
    ,'y $.y | (... -> $ | succ)
    }
   )
  ,[[]]
  ,[[[]]]
  ]
;

(unat -> unat) pred = (... -> $.i);

(unat -> unat) predtrunc =
  [pred
  ,(... -> 'o)
  ]
;

<code> nattriple <is> {'x unat, 'y unat, 'z unat};
<code> dnattriple <is> {'x dnat, 'y dnat, 'z dnat};

<code> char <is> ['a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s,'t,'u,'v,'w,'x,'y,'z,'A,'B,'C,'D,'E,'F,'G,'H,'I,'J,'K,'L,'M,'N,'O,'P,'Q,'R,'S,'T,'U,'V,'W,'X,'Y,'Z,'0,'1,'2,'3,'4,'5,'6,'7,'8,'9];
<code> string <is> ['e, 'o {'c char, 'n string}];

-- breaks here:
({} -> {}) dnat_to_string <is>
  [('o -> 'o {'c '0, 'n 'e})
  ]
;

<code> decimal <is> ['0,'1,'2,'3,'4,'5,'6,'7,'8,'9 ];
<code> dnat <is> ['d {'d decimal, 'n dnat}, 'o ];

dnat zerod = 'o;
dnat zero2 = 'd {'d '0, 'n 'o};
dnat zero3 = 'd {'d '0, 'n zero2};
dnat oned = 'd {'d '1, 'n 'o};

(dnat -> dnat) succ_dnat =
  [('o              -> 'd {'d '1, 'n 'o})
  ,('d {'d '0, ...} -> 'd {'d '1, 'n $.d.n})
  ,('d {'d '1, ...} -> 'd {'d '2, 'n $.d.n})
  ,('d {'d '2, ...} -> 'd {'d '3, 'n $.d.n})
  ,('d {'d '3, ...} -> 'd {'d '4, 'n $.d.n})
  ,('d {'d '4, ...} -> 'd {'d '5, 'n $.d.n})
  ,('d {'d '5, ...} -> 'd {'d '6, 'n $.d.n})
  ,('d {'d '6, ...} -> 'd {'d '7, 'n $.d.n})
  ,('d {'d '7, ...} -> 'd {'d '8, 'n $.d.n})
  ,('d {'d '8, ...} -> 'd {'d '9, 'n $.d.n})
  ,('d {'d '9, ...} -> 'd {'d '0, 'n $.d.n | succ_dnat})
  ]
;

({'u unat, 'd dnat} -> {'u unat, 'd dnat}) unat_to_dnat_ =
  [({'u 'o,  'd ...} -> {'u 'o, 'd $.d})
  ,({'u ..., 'd ...} -> {'u $.u.i, 'd $.d | succ_dnat} | unat_to_dnat_)
  ]
;

(unat -> dnat) unat_to_dnat =
  (... -> {'u $, 'd 'o} | unat_to_dnat_ | (... -> $.d));

--({} -> a) main = get_a | inc_a;
--({} -> natpair) main = {'x 'i zero, 'y zero} | succboth | succboth2 | succboth3;
--({} -> natpair) main = {'x zero|predtrunc, 'y one|predtrunc}
--({} -> nattriple) main = {'x zero|predtrunc, 'y one|predtrunc, 'z two|predtrunc};
({} -> dnattriple) main =
  {'x zero | predtrunc | unat_to_dnat
  ,'y one  | predtrunc | unat_to_dnat
  ,'z two  | predtrunc | unat_to_dnat
  }
;
