<code> bool <is> ['t,'f];

({'x bool, 'y bool} -> {'x bool, 'y bool}) rev <is> (... -> {'x $.y, 'y $.x});

{'x bool, 'y bool} main <is> 't | dup;



<code> C <is> ['x C, 'y ];

(C -> C) x_c <is> (... -> 'x $);

(C -> C) x_d <is> (... -> $.x);


C y_c <is> 'y;
(C -> {}) y_d <is> (... -> $.y);



