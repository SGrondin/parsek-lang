 <type> bit <is> ['0, '1];
 <type> 2bits <is> {'b0 bit, 'b1 bit};
<type> 2bits-bis <is> {'b1 ['0,'1], 'b0 ['1,'0]};

2bits-bis zero =  {'b0 '0, 'b1 '0};

2bits main = zero;