<code> nat <is> ['0, '1 nat];      -- (recursive) variant type

-- examples:
({} -> nat) 0 <is> '0;
({} -> nat) 1 <is> '1'0;
nat 10 <is> '1'1'1'1'1'1'1'1'1'1'0;

<code> pair <is> {'x nat, 'y nat};  -- record type

(pair -> {}) ==  <is> [
      ({'x '0, 'y '0} -> {}),
      (... -> {'x $.x.1, 'y $.y.1}) | ==  
     ];

(nat -> nat) ++ <is> [
  ('1'1'1'1 ... -> $),
  ('0, '1'0, '1'1'0, '1'1'1'0 -> '1 $)];

// nat main = 0 | ++ | ++ | ++| ++ | ++ | ++;


-- examples:
pair 0_and_1 <is> {'x 0, 'y 1};
pair 1_10 <is> {'y 10, 'x '1'0};  

(pair -> pair) swap <is> (... -> {'x $.y, 'y $.x});

(nat -> pair) double = ($arg nat -> {'x $arg, 'y $arg});
// or  (nat -> {'x $, 'y $});
// or  (... -> {'x $, 'y $});

nat toto <is> <input>;      -- FIFOs
({} -> nat) get_nat <is> toto; 
(nat -> {}) put_nat <is> <output>;

(pair -> nat ) f1 <is> ({'x nat, 'y '0 } -> $.x);
(pair -> nat ) f2 <is>  ({'x '1 nat, ...} -> {'x $.x.1, 'y $.y.1}) | x-y;

({'x nat, 'y nat} -> nat) x-y <is> [ f1, f2 ];

(pair -> nat) y-x <is> swap | x-y;

pair get_pair <is>  {'x get_nat, 'y get_nat  <serialize> toto};


pair get_bla <is> {'x get_nat, 'y [ get_nat | (... -> get_nat) | (... -> get_nat) ]};


-- pair main <is> get_bla;

skip_a_nat <is> get_nat | (nat -> {});


(nat -> nat) find <is>  [
         (... -> {'x $, 'y get_nat} | == | '0),
         ($x ... -> get_nat | (... -> '1 [$x | find]))
];     


-- nat main <is> '1'1'1'1'1'0 | find;


nat main <is> 
[ 
 {'0  get_bla.y, 'titi 0}.0,
 skip_a_nat|toto 
]
;

/*
// {} main ...
nat main =
 [
   get_bla.x, 

   skip_a_nat | get_nat,
  
  {'y get_nat, 'x  '1'1'1'1'1 @y} | x-y,

  skip_a_nat | get_pair | '1 [y-x, x-y]  

   <shortest> get_nat
  -- <longest> get_nat
 ] :: (... -> $ | put_nat | $);

pair main7 <is> get_bla :: (... -> get_bla);


*/