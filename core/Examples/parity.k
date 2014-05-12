
/********************** from the "intro" doc *******************/
<type> PARITY <is> ['even, 'odd];
<type> BIT <is> ['0, '1];

['eof, 'b BIT] parity_in = <input>;
['eof, 'b BIT] input = parity_in;

PARITY ones = input :: [('eof -> 'even),
       	                ('b '0 -> ones),
			('b '1 -> ones) :: inc];

PARITY zeros = input :: [('eof -> 'even),
       	                 ('b '0 -> zeros) :: inc,
			 ('b '1 -> zeros)];

(PARITY -> PARITY) inc = 
   [ ('even -> 'odd), ('odd -> 'even) ];

({'x PARITY,'y PARITY} -> PARITY) add =
   [ ($x {'x 'even,'y PARITY} -> $x.y),
     ($x {'y PARITY, 'x 'odd} -> $x.y :: inc) ];

{'zeros PARITY, 'ones PARITY, 'all PARITY} main = 
   {'zeros zeros, 'ones ones, 'all {'x @ones,'y @zeros}::add};


