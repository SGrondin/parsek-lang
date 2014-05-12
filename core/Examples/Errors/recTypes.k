
<type> B1 <is> ['0, '1 B2];
<type> B2 <is> ['0, '1 B1];
<type> B3 <is> ['0, '1 B1];

B1 b1 <is> '0;
B2 b2 <is> '0;
B3 b3 <is> '0;

(B1 -> {}) b1Fn <is> (... -> {});

main = b2 :: b1Fn :: ({} -> b2) :: b1Fn;
 

