<type> digit <is> ['0,'1,'2,'3,'4,'5,'6,'7,'8,'9];
<type> digit_list <is> ['empty, 'list {'digit digit, 'cont digit_list}];

--------------------------------------------------------------------------

(digit -> digit) next_digit =
       [('0 ->'1),('1 ->'2),('2 ->'3),('3 ->'4),('4 ->'5),
        ('5 ->'6),('6 ->'7),('7 ->'8),('8 ->'9)];

({'x digit, 'y digit} -> {}) equal_digits? =
     ({'x '0, 'y '0},{'x '1, 'y '1},{'x '2, 'y '2},{'x '3, 'y '3},{'x '4, 'y '4},
      {'x '5, 'y '5},{'x '6, 'y '6},{'x '7, 'y '7},{'x '8, 'y '8},{'x '9, 'y '9} 
      -> {});

({'x digit, 'y digit} -> {}) y_next_to_x? =
     (... -> {'x $.x::next_digit, 'y $.y}) :: equal_digits?
;

({'x digit, 'try digit} -> digit) find_previous = (... ->
     [ {'x $.try, 'y $.x} ::  y_next_to_x? :: $.try,
       {'x $.x, 'try $.try :: next_digit} :: find_previous ]);

(digit -> digit) previous_digit =
        (... -> {'x $, 'try '0}) :: find_previous;

---------------------------------------------------------

(digit_list -> digit_list) remove_leading_zeros =
     [('list {'digit '0, ...} -> $.list.cont) :: remove_leading_zeros,
      (... -> $)] <by> FIRST_MATCH;

({'d digit, 'l digit_list} -> digit_list) append_d_to_l =
     [ ({'l 'empty, ...} -> 'list{'digit $.d, 'cont 'empty}),
       (... -> 'list{'digit $.l.list.digit, 
                     'cont [{'d $.d, 'l $.l.list.cont} ::  append_d_to_l] })
     ] <by> FIRST_MATCH;

		     
(digit_list -> digit_list) reverse_digit_list =
	[ ('empty -> 'empty),
          (... -> {'d $.list.digit, 
	           'l $.list.cont :: reverse_digit_list}
          ) :: append_d_to_l
	] <by> FIRST_MATCH; 

---------------------------------------------------------------------------

(digit_list -> digit_list) inc_reverse_digit_list =
	    [('empty -> 'list{'digit'1,'cont'empty}),
	     ('list{'digit '9,...} -> 
              'list{'digit'0, 'cont $.list.cont :: inc_reverse_digit_list}),
	     (... -> 'list{'digit $.list.digit :: next_digit,
                           'cont $.list.cont})
	    ] <by> FIRST_MATCH;

(digit_list -> digit_list) inc_digit_list =
	    remove_leading_zeros :: reverse_digit_list ::
	    inc_reverse_digit_list :: reverse_digit_list;


digit_list main =
{'d '9, 'l      
'list{'digit'9,'cont'list{'digit'9,'cont'empty}}
} :: append_d_to_l :: inc_digit_list ;

