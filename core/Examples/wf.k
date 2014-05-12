<code> bnat <is> ['_, '1 bnat, '0 bnat ];

bnat 8 <is> '1'0'0'0'_;

(bnat -> bnat) normalize <is> [
      ('0 ... -> $.0) | normalize ,
      (... -> $) ];

// bnat main <is> '0'0'1'1'0'_ | normalize;

<code> bit <is> ['0,'1];


<code(X)> numb <is> ['nil, 'numb {'car X, 'cdr numb} ];
 
(numb -> numb) normalize_numb <is> 
    [
      ('nil -> 'nil),
      ('numb {'car '0, ...} -> $.numb.cdr) | normalize_numb,
      (... -> $) 
    ];

<code> pair <is> {'b bnat, 'n numb};

bnat bnat <is> <input>;

bnat  main <is> 
      {'x bnat, 'y bnat <serialize> bnat}.y;       
