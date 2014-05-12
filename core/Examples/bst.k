-- Binary Search Tree over natural numbers in binary encoding
-- It reads from file "bst_in" and writes to file "bst_out"
--   File "bst_in" contains a list of natural numbers in our
-- encoding. The resulting file, "bst_out" should list all those
-- numbers in the increasing order.

['add nat, 'print] bst_in = <input>;
(nat -> {}) bst_out = <output>;

<type> nat <is> ['_, '0 nat, '1 nat ];
<type> bst <is> ['leaf, 'node {'key nat, 'l bst, 'r bst} ];

(nat -> nat) append_one = 
     [('_ -> '1'_),
      ('1 nat -> '1 [$.1 :: append_one]),
      ('0 nat -> '0 [$.0 :: append_one])
      ];

(nat -> nat) append_zero =
     [('_ -> '0'_),
      ('1 nat -> '1 [$.1 :: append_zero]),
      ('0 nat -> '0 [$.0 :: append_zero])
     ];

(nat -> nat) reverse = 
     [ ('_ -> '_),
       ('1 ... -> $.1 :: reverse :: append_one),
       ('0 ... -> $.0 :: reverse :: append_zero)
       ];

({'x nat, 'y nat} -> {'x nat, 'y nat}) adjust =
     [
      ({'x '_, 'y '_} -> $),
      ({'x '_, ...} 
      	   ->  {'x '0 $.x, 'y $.y}) :: adjust ,
      ({'y '_, ...} 
      	   ->  {'x $.x, 'y '0 $.y}) :: adjust ,
      (...
       -> 
        [{'x $.x.1, 'y $.y.1} :: adjust :: (... -> {'x '1 $.x, 'y '1 $.y}),
         {'x $.x.1, 'y $.y.0} :: adjust :: (... -> {'x '1 $.x, 'y '0 $.y}),
         {'x $.x.0, 'y $.y.1} :: adjust :: (... -> {'x '0 $.x, 'y '1 $.y}),
         {'x $.x.0, 'y $.y.0} :: adjust :: (... -> {'x '0 $.x, 'y '0 $.y})
        ]
      ) ];


({'x nat, 'y nat} -> {'x nat, 'y nat}) normalize =
     (... -> {'x $.x :: reverse, 'y $.y :: reverse}) 
     :: adjust 
     :: (... -> {'x $.x :: reverse, 'y $.y :: reverse });

({'x nat, 'y nat} -> {}) leq_normalized = [
  ({'x '_, ...}, {'x '0 ..., 'y '1 ...} -> {}),
  (... -> [{'x $.x.0, 'y $.y.0},{'x $.x.1, 'y $.y.1}]) :: leq_normalized 
];
  
({'x nat, 'y nat} -> ['true, 'false]) leq = 
     [ normalize :: leq_normalized :: 'true,
       (... -> 'false) ];


({'n nat, 't bst} -> bst) insert = [
  ({'t 'leaf, ...} -> 'node {'key $.n, 'l 'leaf, 'r 'leaf}),
  ({'t 'node ..., ...} ->
    {'x $.n, 'y $.t.node.key} :: leq ::
     [($b 'true -> 
       'node {'key $.t.node.key, 
              'l {'n $.n, 't $.t.node.l} :: insert, 
              'r $.t.node.r }),
      ($b 'false -> 
       'node {'key $.t.node.key, 
              'r {'n $.n, 't $.t.node.r} :: insert, 
              'l $.t.node.l })
    ]) 
];


({'n nat, 't bst} -> {'l bst, 'r bst}) split = [
  ({'t 'leaf, ...} -> {'l 'leaf, 'r 'leaf}),  
  (... ->
    {'x $.n, 'y $.t.node.key} :: leq ::
    [ ($_ 'true ->  
     {'n $.n, 't $.t.node.l} :: split :: 
       ($u ... -> {'l $u.l, 'r 'node {'key $.t.node.key, 
       	       	      	       	      'l $u.r, 
				      'r $.t.node.r}})),
      ($_ 'false ->
     {'n $.n, 't $.t.node.r} :: split :: 
       ($u ... -> {'r $u.r, 'l 'node {'key $.t.node.key, 
       	       	      	       	      'r $u.l, 
				      'l $.t.node.l}}))
   ])
];
     
({'n nat, 't bst} -> bst) insert_head = 
  (... -> $ :: split 
            :: ($u ... -> 'node {'key $.n, 'l $u.l, 'r $u.r}));


(bst -> {}) print_in_order = 
 [ ('leaf -> {}),
   ('node ... -> $.node.l ::  print_in_order :: 
           $.node.key :: bst_out ::    
	   $.node.r ::  print_in_order)];

(bst -> {}) loop = 
  ($tree bst -> 
    bst_in ::
     [
      ('add nat -> {'n $.add, 't $tree}) :: insert_head :: loop,
      ('print -> $tree) :: print_in_order
     ]);


main = 'leaf :: loop;
 


