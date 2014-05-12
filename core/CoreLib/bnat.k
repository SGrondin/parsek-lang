-- Standard (most significant bit first) binary encoding of natural
-- numbers plus the operations.  
 
<code> bnat <is> ['_, '0 bnat, '1 bnat];

(bnat -> bnat) remove_leading_zeros <is>
      [ 
        ('0 ... -> $.0) | remove_leading_zeros,
	(... -> $) 
      ];

(bnat -> bnat) _append_zero <is>
     [ 
        ('_ -> '0'_),
	('1 ... -> '1 [$.1 | _append_zero]),
	('0 ... -> '0 [$.0 | _append_zero])
     ];

(bnat -> bnat) _append_one <is>
     [ 
        ('_ -> '1'_),
	('1 ... -> '1 [$.1 | _append_one]),
	('0 ... -> '0 [$.0 | _append_one])
     ];

(bnat -> bnat) _reverse_bnat <is> 
     [ 
        ('_ -> '_),
	('0 ... -> $.0 | _reverse_bnat | _append_zero),
	('1 ... -> $.1 | _reverse_bnat | _append_one)
     ];


({'x bnat, 'y bnat, 'c ['0,'1]} -> bnat) _add2wc <is>
/* least significat bit first addition with carry */
     [
      ({'x '_, 'c '0, ...} -> $.y ),
      ({'y '_, 'c '0, ...} -> $.x ),
      ({'x '_, 'y '_, 'c '1, ...} -> '1'_ ),
      ({'x '_, 'y '0 ..., 'c '1 } -> '1 $.y.0 ),
      ({'y '_, 'x '0 ..., 'c '1 } -> '1 $.x.0 ),
      ({'x '_, 'y '1 ..., 'c '1 } -> '0 [{'x '_, 'y $.y.1, 'c '1} | _add2wc]),
      ({'y '_, 'x '1 ..., 'c '1 } -> '0 [{'x $.x.1, 'y '_, 'c '1} | _add2wc] ),
      ({'x '0 ..., 'y '0 ..., 'c '0} -> 
      	   '0 [{'x $.x.0, 'y $.y.0, 'c '0} | _add2wc]),
      ({'x '0 ..., 'y '0 ..., 'c '1} -> 
      	   '1 [{'x $.x.0, 'y $.y.0, 'c '0} | _add2wc]),
      ({'x '0 ..., 'y '1 ..., 'c '0} -> 
      	   '1 [{'x $.x.0, 'y $.y.1, 'c '0} | _add2wc]),
      ({'x '0 ..., 'y '1 ..., 'c '1} -> 
      	   '0 [{'x $.x.0, 'y $.y.1, 'c '1} | _add2wc]),
      ({'x '1 ..., 'y '0 ..., 'c '0} -> 
      	   '1 [{'x $.x.1, 'y $.y.0, 'c '0} | _add2wc]),
      ({'x '1 ..., 'y '0 ..., 'c '1} -> 
      	   '0 [{'x $.x.1, 'y $.y.0, 'c '1} | _add2wc]),
      ({'x '1 ..., 'y '1 ..., 'c '0} -> 
      	   '0 [{'x $.x.1, 'y $.y.1, 'c '1} | _add2wc]),
      ({'x '1 ..., 'y '1 ..., 'c '1} -> 
      	   '1 [{'x $.x.1, 'y $.y.1, 'c '1} | _add2wc]) 
];


({'x bnat, 'y bnat} -> bnat) plus <is>
   (... -> 
     {'x $.x | remove_leading_zeros | _reverse_bnat, 
      'y $.y | remove_leading_zeros | _reverse_bnat, 
      'c '0} 
     | _add2wc 
     | _reverse_bnat );


({'x bnat, 'y bnat, 'c ['0,'1]} -> bnat) _sub2wc <is>
/* least significat bit first substraction with carry */
     [
      ({'y '_, 'c '0, ... } -> $.x ),
      ({'y '_, 'x '1 ..., 'c '1 } -> '0 $.x.1 ),
      ({'y '_, 'x '0 ..., 'c '1 } -> 
      	   '1 [{'x $.x.0, 'y '_, 'c '1} | _sub2wc] ),
      ({'x '0 ..., 'y '0 ..., 'c '0} -> 
      	   '0 [{'x $.x.0, 'y $.y.0, 'c '0} | _sub2wc]),
      ({'x '0 ..., 'y '0 ..., 'c '1} -> 
      	   '1 [{'x $.x.0, 'y $.y.0, 'c '1} | _sub2wc]),
      ({'x '0 ..., 'y '1 ..., 'c '0} -> 
      	   '1 [{'x $.x.0, 'y $.y.1, 'c '1} | _sub2wc]),
      ({'x '0 ..., 'y '1 ..., 'c '1} -> 
      	   '0 [{'x $.x.0, 'y $.y.1, 'c '0} | _sub2wc]),
      ({'x '1 ..., 'y '0 ..., 'c '0} -> 
      	   '1 [{'x $.x.1, 'y $.y.0, 'c '0} | _sub2wc]),
      ({'x '1 ..., 'y '0 ..., 'c '1} -> 
      	   '0 [{'x $.x.1, 'y $.y.0, 'c '0} | _sub2wc]),
      ({'x '1 ..., 'y '1 ..., 'c '0} -> 
      	   '0 [{'x $.x.1, 'y $.y.1, 'c '0} | _sub2wc]),
      ({'x '1 ..., 'y '1 ..., 'c '1} -> 
      	   '1 [{'x $.x.1, 'y $.y.1, 'c '1} | _sub2wc]) 
];


({'x bnat, 'y bnat} -> bnat) minus =
   (... -> 
     {'x $.x | remove_leading_zeros | _reverse_bnat, 
      'y $.y | remove_leading_zeros | _reverse_bnat, 
      'c '0} 
     | _sub2wc 
     | _reverse_bnat | remove_leading_zeros);


(bnat -> bnat) incr = (... -> {'x $, 'y '1'_} | plus);
(bnat -> bnat) decr = (... -> {'x $, 'y '1'_} | minus);


({'result bnat, 'multipl bnat, 'times bnat} -> bnat ) _sm =
/* This function "smart mult" calculates 
   	result + (multipl * (times :: reverse_nat))
*/ 
    [
     ({'times '_, ...} -> $.result),
     ({'times '0 ..., ...} -> {'result $.result, 
     	      	      	       'multipl $.multipl | _append_zero,
			       'times $.times.0} | _sm),
     ({'times '1 ..., ...} -> 
     	      {'result {'x $.result, 'y $.multipl} | plus,
     	       'multipl $.multipl | _append_zero,
	       'times $.times.1} | _sm)
    ];

({'x bnat, 'y bnat} -> bnat) times =
  (... -> {'result '_, 
           'multipl $.x | remove_leading_zeros, 
	   'times $.y | remove_leading_zeros | _reverse_bnat} 
	  | _sm );

/*
 bnat main = {'x '1'1'1'_, 'y '1'0'_} | times;
*/