-- gray encoding
-- requires "bool.k" 

<code> gnat <is> ['0 gnat, '1 gnat, '_];

gnat 0 <is> '_;
gnat 1 <is> 0 | succ; 
gnat 2 <is> 1 | succ; 
gnat 3 <is> 2 | succ; 
gnat 4 <is> 3 | succ; 
gnat 5 <is> 4 | succ; 
gnat 6 <is> 5 | succ; 
gnat 7 <is> 6 | succ; 
gnat 8 <is> 7 | succ; 
gnat 9 <is> 8 | succ; 
gnat 10 <is> 9 | succ; 
gnat 11 <is> 10 | succ; 


(gnat -> gnat) normalize <is> [
      ('0 ... -> $.0) | normalize,
      (... -> $)
];

(gnat -> gnat) succ <is> [
      ('_ -> '1'_),
      ('0'_ -> '1'_),
      ($x '0 ... -> $x.0 | max | [
      	  ('true -> '1 $x.0),
	  ('false -> '0 [$x.0 | succ])]),
      ($x '1 ... -> $x.1 | min | [
      	  ('true -> '1 $x),
	  ('false -> '1 [$x.1 | prec])])
];

(gnat -> bool) max <is> [
      ('_ -> 'true),
      ('1 ... -> $.1 | min),
      ('0 ... -> 'false)];

(gnat -> bool) min <is> normalize | 
      [ ('_ -> 'true),
        ('1 ... -> 'false) ];

(gnat -> gnat) prec <is> [
      ('1'_ -> '0'_),
      ('0 ... -> '0 [$.0 | prec]),
      ($x '1 ... -> $x.1 | max | [
      	  ('true -> '0 [$x.1]),
	  ('false -> '1 [$x.1 | succ])])
];

({'x gnat, 'y gnat} -> gnat) x+y <is> [
     ({'x '_, ...} -> $.y),
     (... -> {'x $.x | prec | normalize, 'y $.y | succ}) | x+y
];

({'x gnat, 'y gnat} -> {'x gnat, 'y gnat}) pad <is> [
     ({'x '_,'y '_} -> $),
     ({'x '_, ...} -> {'x '0'_, 'y $.y}) | pad,
     ({'y '_, ...} -> {'x $.x, 'y '0'_}) | pad,
     ({'x '0 ..., 'y '0 ...} -> {'x $.x.0, 'y $.y.0} | pad) 
       | (... -> {'x '0 $.x, 'y '0 $.y}),
     ({'x '0 ..., 'y '1 ...} -> {'x $.x.0, 'y $.y.1} | pad) 
       | (... -> {'x '0 $.x, 'y '1 $.y}),
     ({'x '1 ..., 'y '0 ...} -> {'x $.x.1, 'y $.y.0} | pad) 
       | (... -> {'x '1 $.x, 'y '0 $.y}),
     ({'x '1 ..., 'y '1 ...} -> {'x $.x.1, 'y $.y.1} | pad) 
       | (... -> {'x '1 $.x, 'y '1 $.y})];


(gnat -> gnat) reverse <is> [
      ('_ -> '_),
      ('0 ... -> [$.0 | reverse]) | append0,
      ('1 ... -> [$.1 | reverse]) | append1
];

(gnat -> gnat) append0 <is> [
      ('_ -> '0'_),
      ('0 ... -> '0 [$.0 | append0] ),
      ('1 ... -> '1 [$.1 | append0] )];

(gnat -> gnat) append1 <is> [
      ('_ -> '1'_),
      ('0 ... -> '0 [$.0 | append1] ),
      ('1 ... -> '1 [$.1 | append1] )];


({'x gnat, 'y gnat} -> {'x gnat, 'y gnat}) align <is>
  (... -> {'x $.x | reverse, 'y $.y | reverse}) | pad |
  (... -> {'x $.x | reverse, 'y $.y | reverse});
 
({'x gnat, 'y gnat} -> bool) x<=y <is> [
     ({'x '_, 'y '_} -> 'true),
     ({'x '0 ..., 'y '1 ...} -> 'true),
     ({'x '1 ..., 'y '0 ...} -> 'false),
     ({'x '1 ..., 'y '1 ...} -> {'x $.x.1, 'y $.y.1}) | x>=y,
     ({'x '0 ..., 'y '0 ...} -> {'x $.x.0, 'y $.y.0}) | x<=y
];
({'x gnat, 'y gnat} -> bool) x>=y <is> [
     ({'x '_, 'y '_} -> 'true),
     ({'x '1 ..., 'y '0 ...} -> 'true),
     ({'x '0 ..., 'y '1 ...} -> 'false),
     ({'x '1 ..., 'y '1 ...} -> {'x $.x.1, 'y $.y.1}) | x<=y,
     ({'x '0 ..., 'y '0 ...} -> {'x $.x.0, 'y $.y.0}) | x>=y
];
     

     
-- {'x gnat, 'y gnat} main <is> {'x 3, 'y 10} | align;

-- bool main <is> {'x 3, 'y 3} | align | x<=y;
