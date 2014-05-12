<type> bool <is> ['oui, 'non];
<type> nat <is> ['0, '1 nat];
<type> pair <is> {'x nat, 'y nat};
<type> arbre <is> ['_, 'a {'n nat, 'g arbre, 'd arbre}];

nat 0 <is> '0;
nat 1 <is> '1 0;
nat 2 <is> '1 1;
nat 3 <is> '1 2;
nat 5 <is> '1'1 3;

               -------- x-ge-y? --------
(pair -> bool) x-ge-y? =
 [ ({'y '0, ...} -> 'oui),
   ({'x '0, ...} -> 'non),
   ( ... -> {'x $.x.1, 'y $.y.1} :: x-ge-y?)];
               ---------- add --------
(pair -> nat) add =
 [ ({'x '0, ...} -> $.y),
   (... -> '1 [{'x $.x.1, 'y $.y} :: add])];
               ---------- mult --------
(pair -> nat) mult =
 [ ({'x '0, ...} -> '0),
   (... -> {'x $.y, 'y {'x $.x.1, 'y $.y} :: mult}) :: add];
               ---------- abr? --------
-- pour définir abr? nous introduisons un nouveau type:
<type> liste <is> ['_, 'l {'head nat, 'tail liste}];
---- concat: concaténation de deux listes
({'x liste, 'y liste} -> liste) concat =
 [ ({'x '_, ...} -> $.y),
   (... -> 'l {'head $.x.l.head,
               'tail {'x $.x.l.tail, 'y $.y} :: concat})];
---- en-ordre: vérifie si la liste est en ordre croissant
(liste -> bool) en-ordre? =
 [('_ -> 'oui),
  ('l {'tail '_, ...} -> 'oui),
  (... -> {'x $.l.tail.l.head, 'y $.l.head} :: x-ge-y?
          :: [($b 'oui -> $.l.tail) :: en-ordre?,
              ('non ->'non) ] )];
---- infixe: produit une liste (infixe) des  éléments d'un arbre
(arbre -> liste) infixe =
 [('_ -> '_),
  (... -> {'x $.a.g :: infixe,
           'y 'l{'head $.a.n, 'tail $.a.d::infixe}} ) :: concat ];
-- on vérifie si un arbre est un "ABR" (ArBre de Recherche)
(arbre -> bool) abr? = infixe :: en-ordre?;

(nat -> arbre) a <is>  (... -> 'a {'n $, 'g'_, 'd'_});

liste  main <is> ['a {'n 3,'g 2|a, 'd 5|a}] | infixe ; 

