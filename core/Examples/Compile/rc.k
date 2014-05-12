-- Read a stream of bits till 'eof and construct the list of it as result.

<type> bit <is> ['a, 'b];

<type> lbits <is> ['empty, 'list {'car bit, 'cdr lbits}];

['a,'b,'eof] RS = <input>;

lbits main = RS :: [('eof -> 'empty), 
      	            ('a -> 'list {'car 'a, 'cdr main}),
      	            ('b -> 'list {'car 'b, 'cdr main})];
