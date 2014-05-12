-- our alphabet has four letters
<type> letter <is> ['a, 'b, 'c, 'd];

-- we define the ordered on the letters by defining relation "next"
(letter -> letter) next =  
   [ ('a -> 'b), -- next of 'a is 'b 
     ('b -> 'c), -- next of 'b is 'c
     ('c -> 'd)  -- and next of 'c is 'd
     ];

({'x letter, 'y letter} -> {}) equal =
  [ ({'x 'a, 'y 'a } -> {}), ({'x 'b, 'y 'b } -> {}), 
    ({'x 'c, 'y 'c } -> {}), ({'x 'd, 'y 'd } -> {}) ];
     
({'x letter, 'y letter} -> {}) greater =
     (... -> {'x $.x :: next, 'y $.y} :: [equal, greater] );

 main = {'x 'a, 'y 'c} :: [equal,greater];

-- <type> tree <is> ['empty, 'tree {'value, 'left tree, 'right tree }];

   
