<type> bit <is> ['0,'1];

<type> bits <is> ['_, 'l {'b bit, 'n bits}];

<type> lbits <is> ['_,'0 lbits,'1 lbits];

(lbits -> bits) c = [
       ('_ -> '_),
       ('0 ... -> 'l {'b '0, 'n $.0 :: c}),
       ('1 ... -> 'l {'b '1, 'n $.1 :: c})
       ];

(lbits -> lbits) tail =
       (... -> [$.0,$.1]);

(lbits -> lbits) xor = 
       [  ('_ -> '_)
        , ('1 lbits -> '0 [$.1 :: xor])
        , ('0 lbits ->  '1 [$.0 :: xor])
];

--<type> obit <is> ['nil, ];



bits main = '0'0'1'0'_ :: c;