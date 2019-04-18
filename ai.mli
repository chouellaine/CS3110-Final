open State

(**  
   The functionality necessay for playing against a computer opponent
*)

(* [minimax st depth curr_path sugg_mvs] is the best evaluation for the 
   current player and the move sequence recommended given the game specified by 
   state [st]. The minimax algorithm searches up to [depth] moves ahead. 
*)
val minimax : t -> int -> (float * ((int*int) list) list)

(* [get_sugg_mv st depth] is the suggested move given the minimax function 
   evaluated at [depth] moves ahead from the current state [st]
*)
val get_sugg_mv : t -> int -> (int*int) list  

