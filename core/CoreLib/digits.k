<code> digit <is> ['0,'1,'2,'3,'4,'5,'6,'7,'8,'9];
(digit -> digit) simple-next <is> [ ('0 -> '1),('1 -> '2),('2 -> '3), 
   ('3 -> '4),('4 -> '5),('5 -> '6),('6 -> '7),('7 -> '8),('8 -> '9)];
(digit -> digit) next <is> [ ('9 -> '0), simple-next ];
(digit -> digit) previous <is> next | next | next | next | next | next | next | next | next;


({'x digit, 'y digit} -> {}) x=y <is>
  [ ({'x'9,'y'9} -> {}),
    (... -> {'x $.x | simple-next, 'y $.y | simple-next}) | x=y ];


({'x digit,'y digit} -> {}) x<=y <is> 
   [ x=y, (... -> {'x $.x | simple-next, 'y $.y}) | x<=y];


({'x digit,'y digit} -> {}) x<y <is>
   (... -> {'x $.x | simple-next, 'y $.y}) | x<=y;

({'x digit, 'y digit} -> {'d digit, 'c digit}) x+y <is>
   [({'y '0, ...} -> {'d $.x, 'c '0}),
    ({'x '9,...} -> {'d $.y | previous, 'c '1}),
    (... -> {'x $.x|simple-next, 'y $.y|previous}) | x+y]; 

({'x digit, 'y digit} -> {'d digit, 'c digit}) x-y <is>
   [({'y '0, ...} -> {'d $.x, 'c '0}),
    ({'x '0, ...} -> {'d [{'x '9, 'y $.y|previous} | x-y].d, 'c '1}),
    (... -> {'x $.x|previous, 'y $.y|previous})|x-y ]; 

({'x digit, 'y digit} -> digit) +g <is> x+y.d;
({'x digit, 'y digit} -> digit) +c <is> x+y.c;
({'x digit, 'y digit} -> digit) -g <is> x-y.d;
({'x digit, 'y digit} -> digit) -c <is> x-y.c;
({'x digit, 'y digit} -> {'d digit, 'c digit}) x*y <is>
   (... -> {'x $.x, 'y $.y, 'c '0}) :: x*y_wc;
({'x digit, 'y digit, 'c digit} -> {'d digit, 'c digit}) x*y_wc <is>
   [({'x '0,...} -> {'d '0, 'c '0}),
    (... ->
      {'a {'x $.x | previous, 'y $.y, 'c $.c} | x*y_wc,
       'b {'x @a.d, 'y $.y} | x+y,
       'c {'d @b.d, 'c {'x @b.c, 'y @a.c} | +g} } ).c ];
({'x digit, 'y digit, 'z digit} -> {'d digit, 'c digit}) x+y+z <is>
   ( ... -> 
     {'a {'x $.x, 'y $.y } | x+y, 
      'b {'x @a.d, 'y $.z} | x+y, 
      'c {'d @b.d, 'c {'x @a.c, 'y @b.c} | +g}}) . c;

# {'x x,'y y,'z z} | x-y-z = {'d d, 'c c} iff d - 10*c = x - y - z
({'x digit, 'y digit, 'z digit} -> {'d digit, 'c digit}) x-y-z <is>
   ( ... -> 
     {'a {'x $.x, 'y $.y } | x-y, 
      'b {'x @a.d, 'y $.z} | x-y, 
      'c {'d @b.d, 'c {'x @a.c, 'y @b.c} | +g}}) . c;



