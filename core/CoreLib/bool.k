-- The Boolean type
 
<code> bool <is> ['true,'false];
<code> para <is> {'x bool, 'y bool};

(bool -> bool) not <is> 
      [
       ('true -> 'false), 
       ('false -> 'true)
      ];

(para -> bool) or <is>
      [({'x 'false, 'y 'false} -> 'false),
       (... -> 'true)];

({'x bool, 'y bool} -> bool) and <is>
      [({'x 'true,'y 'true} -> 'true),
       (... -> 'false)];

-- bool main = {'x 'false, 'y 'true | not} | or | not;


      

    
