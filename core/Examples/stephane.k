/* environment = TOTO

<type> kkk <is> bit;

(kkk -> {}) write = #TOTO;
({} -> kkk) read = #TOTO;

 .... */ 


<type> unat <is> ['_, 'x, '1 unat];

unat zero = '_;
unat un = '1'_;
unat deux = '1'1'_;
unat dix = '1'1'1'1'1'1'1'1'_;



({'x unat, 'y unat} -> unat) add =
     [
      ({'x '_, ...} -> $.y ),
      ({'x '1 ..., ...} -> '1 [{'y $.y, 'x $.x.1 } :: add])
     ];


(unat -> unat) x = [(... -> {'x $, 'y $}) :: add, ('x -> 'x)] ;


{'x unat, 'xx unat, 'xxx unat} main = 
    deux :: ($x ... -> {'x $x, 'xx 'x :: x, 'xxx @xx :: x :: x :: x}) ;


