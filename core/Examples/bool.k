-- We define the Boolean type and we evaluate the expression  
--    not ( 'true or 'false )
-- which should give 'false.
 
<code> bool <is> ['true {}, 'false {}];

<code> nat <is> ['_ {} ,'1 nat];

({} -> nat) 0 <is> '_ ;
({} -> nat) 1 <is> '1'_;
nat 2 <is> '1'1'_;
nat 3 <is> '1'1'1'_;
nat 10 <is> '1'1'1'1'1
            '1'1'1'1'1
	    '_;

<code> pair <is> {'x nat, 'y nat};

({'x nat, 'y nat} -> nat) x+y <is> 
  [
   ({'x '_, 'y nat} -> $.y),
   ({'x '1 ..., ...} ->
       '1 [
            {'x $.x.1, 'y $.y}  |  x+y
          ])
  ];

({'x nat, 'y nat} -> nat) x*y <is>
[
  ({'x '_, ...} -> '_),
  ({'x '1 ..., ...} -> 
     {'x {'x $.x.1, 'y $.y} | x*y,
      'y $.y}) | x+y
];

nat main = {'x 10, 'y 10} | x*y;
