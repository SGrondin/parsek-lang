<type> bool <is> ['true, 'false];  -- ['true {}, 'false {}];

bool t = 'true;                    -- ({} -> bool) t = 'true {}; 

(bool -> bool) not =
      [ ('true -> 'false),         -- ($x 'true -> 'false {})    
        ('false -> 'true) ];       -- ($x 'false -> 'true {})

({'x bool, 'y bool} -> bool ) or = 
   [ ({'x 'true,...} -> 'true),    -- ($x {'x 'true,...} -> 'true {})
     ({'x 'false,...} -> $.y) ];   -- ($x {'x 'false,...} -> $x.y)   	

bool main = {'x t, 'y t :: not} :: or; 
   --({} -> bool) main = {'x t, 'y t :: not} :: or;
