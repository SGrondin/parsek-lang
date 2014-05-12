-- test

{'x bool, 'y bool, 'not_x bool, 'x_and_y bool, 'x_or_y bool} main =
  {'x 'true, 'y 'false,
   'not_x @x|not, 
   'x_and_y {'x @x, 'y @y} | and, 
   'x_or_y {'x @x, 'y @y} | or };



      

    
