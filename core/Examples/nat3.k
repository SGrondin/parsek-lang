<code> nat <is> ['_, 'a nat, 'b nat, 'c nat];

nat 0 <is> '_;
nat 1 <is> 'a'_;
nat 2 <is> 'b'_;
nat 3 <is> 'c'_;
nat 4 <is> 'a'a'_;

(nat -> nat) +1 <is> [
    ('_ -> 'a'_),
    ('a ... -> 'b $.a),    
    ('b ... -> 'c $.b),    
    ('c ... -> 'a [$.c :: +1] )];

(nat -> nat) -1 <is> [
    ('a'_ -> '_),  
    ('a ... -> 'c [$.a :: -1]),    
    ('b ... -> 'a $.b),    
    ('c ... -> 'b $.c)];    

({'x nat, 'y nat} -> nat) + <is> [
    ({'y '_, ...} -> $.x),
    (... -> {'x $.x::+1, 'y $.y:: -1}) :: +
];

({'x nat, 'y nat} -> nat) * <is> [
    ({'y '_, ...} -> '_),
    (... -> {'x {'x $.x, 'y $.y:: -1} :: *, 'y $.x}) :: +
];


nat main = {'x 2, 'y 2} :: *;