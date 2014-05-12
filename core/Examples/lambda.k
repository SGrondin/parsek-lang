-- Manipulating lambda terms

<type> Var <is> ['a Var, 'b Var, '_];
<type> Bool <is> ['true, 'false];
<type> Lambda <is> ['var Var, 
                    'app {'f Lambda, 'a Lambda}, 
                    'abs {'x Var, 'e Lambda}];
Lambda lambda_in = <input>;

<type> ListVars <is> ['_, 'vars {'one Var, 'others ListVars}];


--------------------------------
({'x Var, 'y Var} -> Bool)
x_y_are_equal =
--------------------------------
[ ({'x '_, 'y '_} -> 'true),
  ({'x 'a ..., 'y 'a ...}, {'x 'b ..., 'y 'b ...}
    -> {'x [$.x.a,$.x.b], 'y [$.y.a,$.y.b]}):: x_y_are_equal, 
  (... -> 'false)
];


-----------------------------------
({'x Var, 'Y ListVars} -> ListVars) 
remove_x_from_Y =
-----------------------------------
  [ ({'Y '_, ...} -> '_),
    (... -> {'x $.x, 'y $.Y.vars.one} 
    	 :: x_y_are_equal 
	 :: [('true -> {}) :: {'x $.x, 'Y $.Y.vars.others} :: remove_x_from_Y,
	     ('false ->{}) :: 'vars {'one $.Y.vars.one, 
                    'others {'x $.x, 'Y $.Y.vars.others} :: remove_x_from_Y}])
];


-----------------------------------
({'x Var, 'Y ListVars} -> ListVars) 
add_x_to_Y = 
-----------------------------------
     (... -> $ :: remove_x_from_Y 
           :: ($tail ... -> 'vars {'one $.x, 'others $tail}));


-----------------------------------     
({'X ListVars, 'Y ListVars} -> ListVars)
merge_X_and_Y =
-----------------------------------
  [ ({'X '_, ...} -> $.Y),
    ({'X 'vars ..., ...} -> {'X $.X.vars.others, 'Y {'x $.X.vars.one, 'Y $.Y} :: add_x_to_Y})
    ::  merge_X_and_Y 

];


----------------------------------
(Lambda -> ListVars) 
freeVars =
----------------------------------
  [
   ('var ... -> {'x $.var, 'Y '_} :: add_x_to_Y),
   ('abs ... -> {'x $.abs.x, 'Y $.abs.e :: freeVars}) :: remove_x_from_Y,
   (... -> {'X $.app.f :: freeVars, 'Y  $.app.a :: freeVars}) :: merge_X_and_Y 
];


------------------------------
(Lambda -> Bool) 
normalp =
------------------------------
  [
   ('app {'f 'abs ..., 'a ...} -> 'false),
   ('var ... -> 'true),
   ('abs ... -> $.abs.e :: normalp),
   ( ... -> [$.app.f :: normalp :: ('false -> 'false),
             $.app.a :: normalp])];

------------------------------
-- Bool main = {'x 'b'a'_, 'y 'b'a'_} :: x_y_are_equal;
ListVars main = lambda_in :: freeVars;
