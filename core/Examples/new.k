<type> nat <is> ['_, '0 nat, '1 nat ];      -- (recursive) variant type
<type> pair <is> {'x nat, 'y nat};  -- record type
(pair -> pair) swap = (pair -> {'x $.y, 'y $.x});
({} -> nat) get_nat = <input>;      -- FIFO
(pair -> nat) x-y =
      (... -> {'x $.x :: reverse, 'y $.y :: reverse}) 
      :: reverse_x-y :: reverse :: remove_zeros;

(nat -> nat) remove_zeros = (... -> [$.0 :: remove_zeros, $]);

(pair -> nat) y-x = swap :: x-y;
pair get_pair = {'x get_nat, 'y get_nat <serialize> get_nat};
skip_get_nat = get_nat :: (... -> {});

(nat -> nat) reverse = [
     ('_ -> '_),
     ('1 nat -> $.1 :: reverse :: append_1),
     ('0 nat -> $.0 :: reverse :: append_0)];

(nat -> nat) append_0 = [
     ('_ -> '0 '_),
     ('1 ... -> '1 [$.1 :: append_0]),
     ('0 ... -> '0 [$.0 :: append_0])];

(nat -> nat) append_1 = [
     ('_ -> '1 '_),
     ('1 ... -> '1 [$.1 :: append_1]),
     ('0 ... -> '0 [$.0 :: append_1])];

({'x nat, 'y nat} -> nat) reverse_x-y = 
     (... -> {'x $.x, 'y $.y, 'c '0}) :: x-y-c;

({'x nat, 'y nat, 'c ['0, '1]} -> nat) x-y-c = [
     ({'x nat, 'y '_, 'c '0} -> $.x),
     ({'x nat, 'y '_, 'c '1} -> {'x $.x, 'y '1'_, 'c '0} :: x-y-c),
     ({'x '0 nat, 'y '0 nat, 'c '0} -> '0 [{'x $.x.0, 'y $.y.0, 'c '0} :: x-y-c]),
     ({'x '0 nat, 'y '0 nat, 'c '1} -> '1 [{'x $.x.0, 'y $.y.0, 'c '1} :: x-y-c]),
     ({'x '0 nat, 'y '1 nat, 'c '0} -> '1 [{'x $.x.0, 'y $.y.1, 'c '1} :: x-y-c]),
     ({'x '0 nat, 'y '1 nat, 'c '1} -> '0 [{'x $.x.0, 'y $.y.1, 'c '1} :: x-y-c]),
     ({'x '1 nat, 'y '0 nat, 'c '0} -> '1 [{'x $.x.1, 'y $.y.0, 'c '0} :: x-y-c]),
     ({'x '1 nat, 'y '0 nat, 'c '1} -> '0 [{'x $.x.1, 'y $.y.0, 'c '0} :: x-y-c]),
     ({'x '1 nat, 'y '1 nat, 'c '0} -> '0 [{'x $.x.1, 'y $.y.1, 'c '0} :: x-y-c]),
     ({'x '1 nat, 'y '1 nat, 'c '1} -> '1 [{'x $.x.1, 'y $.y.1, 'c '1} :: x-y-c])
];
     
     
nat main = {'x '1'1'0'_, 'y '1'0'0'_} :: x-y;

nat a_main =
 [
   skip_get_nat :: get_nat,
   {'y get_nat, 'x  '1'1 @y} :: x-y,
   skip_get_nat :: get_pair :: [y-x, x-y] 
##  <shortest> get_nat
   <longest> get_nat
 ];
