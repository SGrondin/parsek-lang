-- unary encoding of natural number with
-- addition (add) and multiplication (mult) 

/* recursive type definition */
<code> unat <is> ['_ {}, '1 unat ];

unat 0 <is> '_;
unat 1 <is> '1'_;
unat 2 <is> '1'1'_;
unat 3 <is> '1 2;

({'x unat, 'y unat} -> unat) add <is>
	[ 
          ({'x '_ , 'y unat} -> $.y),
          ({'x '1 ..., ...} -> '1 [{'x $.x.1, 'y $.y} | add] )
 ];

({'x unat, 'y unat} -> unat) mult =
	[
          ({'x '_ , ...} -> '_),
          ( ... -> {'x [{'x $.x.1 , 'y $.y} :: mult], 'y $.y}) :: add
        ];




