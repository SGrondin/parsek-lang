<type> nat <is> ['0, '1 nat];      -- (recursive) variant type

<type> nat2 <is> ['0, '1 {'f nat, 's ['a, 'b]}];

(pair -> {}) ==  <is> [
      ({'x '0, 'y '0} -> {}),
      (... -> {'x $.x.1, 'y $.y.1} | == ) ];

(nat -> nat) ++ <is> [
  ('0, '1'0, '1'1'0, '1'1'1'0 -> '1 $)];

<type> pair <is> {'x nat, 'y nat};  -- record type

({} -> nat) get_nat = <input>;      -- FIFOs

pair get_bla = {'x get_nat, 'y get_nat | (... -> get_nat) };

(nat -> nat) find <is>  [
         (... -> {'x $, 'y get_nat} :: == :: '0)
     ,  ($x ... -> get_nat | (... -> '1 [$x :: find]))
];     

main = {};


