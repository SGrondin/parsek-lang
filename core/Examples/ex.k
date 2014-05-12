<type> bit <is> ['0,'1];
<type> list <is> ['_, 'list {'head bit, 'tail list}];

list liste-vide = '_;

list list-un-elem = 'list {'head '0, 'tail '_};

(list -> bit) second =
 (... -> $.list.tail.list.head);

bit main = 
  'list{'head'0,'tail'list{'head'1,'tail'_}} :: second;

<type> nat <is> ['_, '0 nat, '1 nat];

nat 0 =     '0'_;
nat 1 =     '1'_;
nat 2 =   '1'0'_;
nat 4 = '1'0'0'_;
