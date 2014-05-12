-- It reads "BIT"s from file "skipzeros_in" and it counts it modulo 2

<type> BIT <is> ['0,'1];
BIT skipzeros_in = <input>;
BIT getbit = skipzeros_in;
<type> PARITY <is> ['even, 'odd];
(PARITY -> PARITY) inc = [('odd -> 'even),('even -> 'odd)];
({} -> {}) getzero = getbit :: ('0 -> {});
({} -> {}) getone = getbit :: ('1 -> {});
({} -> {}) getEOF = [ getbit :: (... -> 'a), 'b 
       	   	      <longest> skipzeros_in
       	       	    ] :: (['a,'b] -> $.b); 

({} -> PARITY) skipzeros = 
   [ 'even, getzero::skipzeros::inc <longest> skipzeros_in ];

({} -> PARITY) ones = 
     [ getzero::ones, getone::ones::inc, getEOF::'even ];
({} -> PARITY) zeros = 
     [ getzero::zeros::inc, getone::zeros, getEOF::'even ];
({} -> {'zeros PARITY, 'ones PARITY}) both = 
     { 'zeros zeros, 'ones ones };

({'x PARITY,'y PARITY} -> PARITY) add =
   [ ({'x 'even,'y PARITY} -> $.y),
     ({'x 'odd,'y PARITY} -> $.y :: inc) ];

({} -> {'zeros PARITY, 'ones PARITY, 'all PARITY}) all = 
   {'zeros zeros, 'ones ones, 'all {'x @ones,'y @zeros}::add};

{'zeros PARITY, 'ones PARITY, 'all PARITY} main = all;
