<type> bool <is> ['oui, 'non];
<type> nat <is> ['0, 's nat];
<type> pair <is> {'x nat, 'y nat};
<type> arbre <is> ['_, 'a {'n nat, 'g arbre, 'd arbre}];


(pair -> bool) x-ge-y? = 
      [ ({'y '0, ...} -> 'oui),
        ({'x '0, ...} -> 'non),
        ( ... -> {'x $.x.s, 'y $.y.s} :: x-ge-y?)];

(pair -> nat) add =
      [ ({'x '0, ...} -> $.y),
        (... -> 's [{'x $.x.s, 'y $.y} :: add])];

(pair -> nat) mult =
      [ ({'x '0, ...} -> '0),
        (... -> {'x $.y, 'y {'x $.x.s, 'y $.y} :: mult}) :: add
      ] ;
(arbre -> nat) le-plus-a-gauche =
      [ ('a{'g'_,...} -> $.a.n),
        (... -> $.a.g) :: le-plus-a-gauche
      ];

(arbre -> nat) le-plus-a-droite =
      [ ('a {'d'_, ...} -> $.a.n ),
        (... -> $.a.d) :: le-plus-a-droite
      ];
  

(arbre -> bool) abr? = 
      [ ('_,'a{'g'_,'d'_, ...}  -> 'oui),
        ('a{'g'_, ...} -> 
	   {'y $.a.n, 'x $.a.d :: le-plus-a-gauche}) :: x-ge-y?,
        ('a{'d'_, ...} -> 
	   {'x $.a.n, 'y $.a.g :: le-plus-a-droite}) :: x-ge-y?,
        (... -> 
	   {'d {'y $.a.n, 'x $.a.d :: le-plus-a-gauche} :: x-ge-y?,
	    'g {'x $.a.n, 'y $.a.g :: le-plus-a-droite} :: x-ge-y?})  
           :: [({'d 'oui, 'g 'oui} -> 'oui), (... -> 'non)] 
       ];

({'n nat, 'abr arbre} -> arbre) inserer-n-dans-abr =
      [ ({'abr '_,...} -> 'a{'n $.n,'g'_,'d'_})
 ,
        (... ->
	   {'x $.n, 'y $.abr.a.n} :: x-ge-y? :: 
	   [ ('oui -> {}) :: 
                'a{'g $.abr.a.g, 
                   'n $.abr.a.n, 
                   'd {'abr $.abr.a.d, 'n $.n} :: inserer-n-dans-abr} ,
             ('non -> {})::
                'a{'d $.abr.a.d, 
                   'n $.abr.a.n, 
                   'g {'abr $.abr.a.g, 'n $.n} :: inserer-n-dans-abr}
           ]

)

];
	    

pair pair = {'x 's's'0, 'y 's's's'0};
arbre arbre = 'a{'n's's'0, 'g'a{'n's's'0,'g'_,'d'_}, 'd'a{'n's'0,'g'_,'d'_}}; 
arbre a2 = 'a {'d 'a {'n 's '0 {}, 'g '_ {}, 'd '_ {}}, 
    'n 's 's '0 {}, 
    'g 'a {'d '_ {}, 
           'n 's 's '0 {}, 
           'g 'a {'n '0 {}, 'g '_ {}, 'd '_ {}}}};


-- bool main = pair :: x-ge-y?;
-- nat main = pair :: mult;
-- nat main = arbre :: le-plus-a-droite;
-- bool main = arbre :: abr?;

-- arbre main = {'abr arbre, 'n 's's's's'0} :: inserer-n-dans-abr;
bool main = {'abr arbre, 'n 's's's's'0} :: inserer-n-dans-abr :: abr?;
