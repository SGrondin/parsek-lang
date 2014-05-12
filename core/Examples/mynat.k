<type> bit <is> ['0,'1];
<type> nat <is> ['_, '0 nat, '1 nat];
<type> bool <is> ['true, 'false];

/* few natural numbers in our encoding.
   It is not the usual binary encoding !*/

nat zero =         '_;
nat one  =       '0'_;
nat two  =       '1'_;
nat three =    '0'0'_;
nat four  =    '0'1'_;
nat five  =    '1'0'_;
nat six   =    '1'1'_;

nat eleven = '1'0'0'_;

nat fifteen ='0'0'0'0'_;

/*
 -- the formula is: #(b[n] b[n-1] ... b[2] b[1]) = 
 --                (\sum_{i=1}^{n} b[i]*2^{i-1}) + 2^n -1
 -- where n is the length and b[i] is the value (o or 1) of the symbol at
 -- position i. E.g #(0101) = 1*2^0 + 0*2^1 + 1*2^2 + 0*2^3 + 2^4 -1 = 20 
*/

(nat -> nat) succ =
     [
       ('_ -> '0'_),
       ('0 ... -> [ '1 [$.0 :: ones_to_zeros],
       	            '0 [$.0 :: succ]]),
       '0 ones_to_zeros,
       ('1 ... -> '1 [$.1 :: succ])
     ];      


(nat -> nat) prec =
     [
       ('0 ... -> [$.0 :: zeros_to_ones, '0 [$.0 :: prec] ]),
       ('1 ... -> ['0 [$.1 :: zeros_to_ones], '1 [$.1 :: prec]])
     ];      

({'x nat, 'y nat} -> nat) add1 =
     [ ({'x '_, ...} -> $.y),
       (... -> {'x $.x :: prec, 'y $.y :: succ} :: add1)
];

(nat -> nat) nat_to_bin = [ ('_ -> '_), '1 prec ];
(nat -> nat) skip_zeros = [ ('0 ... -> $.0) :: skip_zeros, (... -> $) ];
(nat -> nat) bin_to_nat = [ skip_zeros.1 :: succ, (... -> '_) ];  

{'x nat, 'bin_to_nat nat, 'nat_to_bin nat} 
    main = {'x eleven, 'bin_to_nat @x :: bin_to_nat, 'nat_to_bin @x :: nat_to_bin};

(nat -> nat) ones_to_zeros =
     -- If argument is a list of zeros, it returns the same length 
     -- list but of ones. Otherwise undefined.
     [ ('_ -> '_), '0 (... -> $.1 :: ones_to_zeros) ];

(nat -> nat) zeros_to_ones=
     -- If argument is a list of ones, it returns the same length 
     -- list but of zeros. Otherwise undefined.
     [ ('_ -> '_), '1 (... -> $.0 :: zeros_to_ones) ];

<type> unat <is> ['1 unat, '_];

({'x unat, 'y unat} -> unat) add0 =
     [
      ({'x '_, ...} -> $.y),
      (... -> {'x $.x.1, 'y '1 $.y} :: add0)
];

(nat -> nat) append_0 = 
     [('_ -> '0'_), 
      ('0 ... -> '0 [$.0 :: append_0]),
      ('1 ... -> '1 [$.1 :: append_0])
     ];

(nat -> nat) append_1 = 
     [('_ -> '1'_), 
      ('0 ... -> '0 [$.0 :: append_1]),
      ('1 ... -> '1 [$.1 :: append_1])
     ];

(nat -> nat) reverse = 
   [('_ -> '_), 
    ('0 ... -> $.0 :: reverse :: append_0),
    ('1 ... -> $.1 :: reverse :: append_1)
   ];

(nat -> nat) fill_r =
     [ ('_ -> '0'_),
       ('1 ... -> '0 [$.1 :: fill_r]),
       ('0 ... -> '1 $.0)
       ];

({'x nat, 'y nat, 'c ['0,'1]} -> nat) add_r =
     [({'x '_, 'y '_, 'c '0} -> '0'_),
      ({'x '_, 'y '_, 'c '1} -> '1'_),

      ({'x '0 ..., 'y '_, 'c '0} -> '1 $.x.0),
      ({'x '0 ..., 'y '_, 'c '1} -> '0 [$.x.0 :: fill_r]),
      ({'x '1 ..., 'y '_, 'c '0} -> '0 [$.x.1 :: fill_r]),
      ({'x '1 ..., 'y '_, 'c '1} -> '1 [$.x.1 :: fill_r]),

      ({'x '0 ..., 'y '0 ..., 'c '0} ->
                '0 [{'x $.x.0, 'y $.y.0, 'c '0}::add_r]),
      ({'x '0 ..., 'y '0 ..., 'c '1} ->
                '1 [{'x $.x.0, 'y $.y.0, 'c '0}::add_r]),
      ({'x '0 ..., 'y '1 ..., 'c '0} ->
                '1 [{'x $.x.0, 'y $.y.1, 'c '0}::add_r]),
      ({'x '0 ..., 'y '1 ..., 'c '1} ->
                '0 [{'x $.x.0, 'y $.y.1, 'c '1}::add_r]),
      ({'x '1 ..., 'y '1 ..., 'c '0} ->
                '0 [{'x $.x.1, 'y $.y.1, 'c '1}::add_r]),
      ({'x '1 ..., 'y '1 ..., 'c '1} ->
                '1 [{'x $.x.1, 'y $.y.1, 'c '1}::add_r]),
      
      ( ...  -> {'x $.y, 'y $.x, 'c $.c}::add_r)
];

({'x nat, 'y nat} -> nat) add2 =
    [
      ({'x '_, ...} -> $.y),
      ({'y '_, ...} -> $.x),
      (... -> {'x $.x :: reverse, 'y $.y :: reverse, 'c '0}) 
      	      :: add_r :: reverse :: prec
    ];


/*
{'add1 nat, 'add2 nat} main = 
      {'x '0'1'0'1'0'0'_, 
       'y '1'0'1'0'0'1'1'1'1'1'1'0'0'1'0'1'0'0'0'1'1'1'_} :: 
       (... -> {'add1 $ :: add1, 'add2 $ :: add2});
*/
      

