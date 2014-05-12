<code> gnat <is> ['_, '0 gnat, '1 gnat];

gnat 1 <is> '1'_;
gnat 2 <is> 1|succ;
gnat 3 <is> 2|succ;
gnat 4 <is> 3|succ;

<code> bool <is> ['true,'false];
(gnat -> bool) max <is> [
      ('_ -> 'true),
      ('0 ... -> 'false),
      ('1 ... -> $.1 | min) ];

(gnat -> bool) min <is> [
      ('_ -> 'true),
      ('0 ... -> $.0 | min),
      ('1 ... -> 'false) ];

(gnat -> gnat) succ <is> [
      ('_ -> '1'_),
      ('0 ... -> [ $.0 | max | ('true -> {}) | ['1 $.0],
                   '0 [$.0 | succ] ]),
      ('1 ... -> [ $.1 | min | ('true -> {}) | ['1 $],
                   '1 [$.1 | prec] ])
];

(gnat -> gnat) prec <is> [
      ('1'_ -> '0'_),
      ('0 ... -> '0 [$.0 | prec]),
      ('1 ... -> [ $.1 | max | ('true -> {}) | ['0 $.1],
                   '1 [$.1 | succ] ])
];

({'x gnat, 'y gnat} -> gnat) x+y <is> (... ->
     [
      $.x | min | ('true -> {}) | $.y,
      {'x $.x | prec , 'y $.y | succ} | x+y
      ]
);

-- gnat main = {'x 2, 'y 2} | x+y;
-- gnat main = 4;

-- <code> l <is> ['-, '+ {'+ gnat,'- l}];

(gnat -> gnat) encode <is> [
      ('_ -> '_),
      (... -> $.1) | encode | succ];

(gnat -> l) enum <is> [
   ('_ -> '-),
   (... -> '+ {'+ $|encode, '- [$.0,$.1] | enum}
];

l main = '1'1'1'1'1'1'1'1'1'1'1'_ | enum;
 


