<type> bit <is> ['0,'1,'error];
bit list_in = <input>;
<type> lbit <is> ['0 lbit, '1 lbit, 'e]; 
lbit main = [list_in :: 
              [('0 -> '0 main),
               ('1 -> '1 main)],
              'e <longest> list_in] ;

