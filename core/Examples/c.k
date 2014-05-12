<code> bool <is> ['oui, 'non];
<code> nat <is> ['0, '1 nat];
<code> pair <is> {'x nat, 'y nat};
<code> arbre <is> ['_, 'a {'n nat, 'g arbre, 'd arbre}];

nat 0 <is> '0;
nat 1 <is> '1'0;
nat 2 <is> '1 1;
nat 3 <is> '1 2;
               -------- succ --------
(nat -> nat) succ <is> (... -> '1 $);

              ---------- pred -------
(nat -> nat) pred <is> ('1 ... -> $.1);


               -------- x-ge-y? --------
(pair -> bool) x-ge-y? <is>
 [ ({'y '0, ...} -> 'oui),
   ({'x '0, ...} -> 'non),
   ( ... -> {'x $.x.1, 'y $.y.1}  |  x-ge-y?)];
               ---------- add --------
(pair -> nat) add <is>
 [ ({'x '0, ...} -> $.y),
   (... -> '1 [{'x $.x.1, 'y $.y}  |  add])];
               ---------- mult --------
(pair -> nat) mult <is>
 [ ({'x '0, ...} -> '0),
   (... -> {'x $.y, 'y {'x $.x.1, 'y $.y}  |  mult})  |  add];
               ---------- abr? --------
-- pour définir abr? nous introduisons un nouveau type:
<code> liste <is> ['_, 'l {'head nat, 'tail liste}];
---- concat: concaténation de deux listes
({'x liste, 'y liste} -> liste) concat <is>
 [ ({'x '_, ...} -> $.y),
   (... -> 'l {'head $.x.l.head,
               'tail {'x $.x.l.tail, 'y $.y}  |  concat})];
---- en-ordre: vérifie si la liste est en ordre croissant
(liste -> bool) en-ordre? <is>
 [('_ -> 'oui),
  ('l {'tail '_, ...} -> 'oui),
  (... -> {'x $.l.tail.l.head, 'y $.l.head}  |  x-ge-y?
           |  [($b 'oui -> $.l.tail)  |  en-ordre?,
              ('non ->'non) ] )];
---- infixe: produit une liste (infixe) des  éléments d'un arbre
(arbre -> liste) infixe <is>
 [('_ -> '_),
  (... -> {'x $.a.g  |  infixe,
           'y 'l{'head $.a.n, 'tail $.a.d | infixe}} )  |  concat ];
-- on vérifie si un arbre est un "ABR" (ArBre de Recherche)
(arbre -> bool) abr? <is> infixe  |  en-ordre?;

nat main = 3 | pred;