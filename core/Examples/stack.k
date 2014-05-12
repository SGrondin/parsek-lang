/* use with pipe.k which will manage "pipe_in and "pipe_out" pipes:
   
   Run with:

     > ./k < stack.k & ./k < pipe.k 

   The bits from file "stack_in" will be copied into "stack_out" in the
   reverse order.
 
 */
-- types: ------------------------------------
<type> bit <is> ['0,'1];
--  fifos: -----------------------------------
['0,'1,'_] stack_in = <input>;
(['0,'1,'_] -> {}) stack_out = <output>;
({} -> ['empty, 'b bit]) pipe_in = <input>; 
(['empty, 'b bit] -> {}) pipe_out = <output>; 
----------------------------------------------
(['empty, 'b bit] -> {}) out =
    [('empty -> '_),
     ('b '0  -> '0) ,
     ('b '1  -> '1)] :: stack_out;

({} -> ['empty, 'b bit]) in = 
    stack_in :: [('0 -> 'b '0),('1 -> 'b '1), ('_ -> 'empty)]; 

read_into_buf =[
	in.empty :: 'empty :: pipe_out,
	in :: pipe_out :: read_into_buf
       ];	  

(bit -> {}) send_out_last =
    (... -> [
    	 pipe_in.empty :: ['b $] :: out :: 'empty :: pipe_out,
	 ['b $] :: pipe_out :: pipe_in.b :: send_out_last 
     ]);


run_loop =
	 [ pipe_in.empty :: 'empty :: out,
	   pipe_in.b  :: send_out_last :: run_loop
];	

reverse = 
	read_into_buf ::
	run_loop;


main = reverse;

