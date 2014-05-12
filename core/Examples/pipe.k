-- An example of using named pipes under unix 
-- It could be done over the network!
/* To run, create two pipes:

  > mkfifo pipe_in pipe_out

  Then open three terminals and in each one do:

     term1> k < pipe.k 
     term2> cat pipe_in
     term3> cat > pipe_out

  Lastly, in terminal term3 start wrinting bits in KernelCore syntax,
  eg:
     term3> '1;
     term3> '0;'1;'0; 
     term3> '1;'1;'1; 
     term3> ...

   You should see the same bits popping up in terminal term2. 

*/

<type> bit <is> ['0,'1];

({} -> ['empty, 'b bit]) pipe_out = <input>; 
(['empty, 'b bit] -> {}) pipe_in = <output>; 

main = pipe_out :: pipe_in :: main;