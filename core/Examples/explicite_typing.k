-- How to do explicite typing?

<type> bool <is> ['true, 'false];
<type> point <is> {'a bool,'b bool};


bool main = {'a 'true,'b 'false :: (bool -> $)}.a;
-- bool main = {'a 'true,'b 'false}.a;
--   correctly yields "Uncaught exception: Typing.Unresolved_type"!


