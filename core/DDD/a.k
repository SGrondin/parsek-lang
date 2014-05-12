<code> bool <is> ['true {}, 'false {}];
<code> list <is> ['empty {}, 'list {'car bool, 'cdr list}];
<code> olist <is> 
       ['empty, 'list {'car ['false,'true], 'cdr list}];

<code> nat <is> ['_,'1 nat];


({} -> nat) 0 <is> '_;
({} -> nat) 1 <is> '1'_;
({} -> nat) 2 <is> '1 1;

<code> pair <is> {'x nat, 'y nat};

(nat -> bool) if_odd <is> [
     ('_ -> 'false),
     ('1'_ -> 'true),
     ('1'1 nat -> $.1.1 | if_odd)
   ];

(nat -> nat) succ <is> (... -> '1 $);

(pair -> nat) add <is>
     [
      ({'x '_, 'y nat} -> $.y),
      ({'x '1 nat, 'y nat} -> {'x $.x.1, 'y '1 $.y}) | add 
     ]; 


({} -> nat) main <is> {'x 2,'y 2} | add;


