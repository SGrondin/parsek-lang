<type> RGB <is> ['leaf, 'rgb {'color ['red,'green,'blue], 'left RGB, 'right RGB}];

(RGB -> {}) no_blue =
    [('leaf -> {}),
     ('rgb { 'color 'green, ...}, 'rgb { 'color 'red, ...} -> 
       {'a $.rgb.left :: no_blue, 'b $.rgb.right :: no_blue} )
       :: (... -> {})
   ];
	     
(RGB -> {}) at_most_one_blue =
   [('leaf -> {}),
    [ ('rgb { 'color 'blue, ...} -> 
       {'a $.rgb.left :: no_blue, 'b $.rgb.right :: no_blue} ),
      (... -> {'a $.rgb.left :: at_most_one_blue, 
             'b $.rgb.right :: at_most_one_blue}) 
    ] ::  (... -> {}) 
   ];

RGB read_rgb = <input> ;
	     
main = read_rgb :: (... -> {}) :: read_rgb ::  no_blue;
