-- tests for "digits.k"

<code> dc <is> {'d digit, 'c digit};
{'x digit, 'y digit, 'x+y dc, 'x-y dc, 'x*y dc}
 main = 
 {'x '9, 
  'y '8,
  'x+y {'x @x, 'y @y} | x+y,
  'x-y {'x @x, 'y @y} | x-y,
  'x*y {'x @x, 'y @y} | x*y};

