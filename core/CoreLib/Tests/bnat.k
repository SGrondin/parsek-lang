-- tests
{'x bnat, 'y bnat, 'plus bnat, 'minus bnat, 'times bnat} main = 
 {'x '1'1'0'1'_, 'y '1'0'1'_,
  'plus {'x @x, 'y @y}|plus,
  'minus {'x @x, 'y @y}|minus,
  'times {'x @x, 'y @y}|times
 };

