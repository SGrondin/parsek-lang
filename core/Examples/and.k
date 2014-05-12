<code> bool <is> ['true, 'false];
<code> pair <is> {'x bool, 'y bool};

(pair -> bool) and <is> [
  ({'x 'false, ...} -> 'false),
  ({'x 'true,...} -> $.y) ];

bool main <is> {'x'true,'y'true} | and;
 