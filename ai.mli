open State

(**  
   The functionality necessay for playing against a computer opponent
*)

(* [minimax st depth curr_path sugg_mvs] is the best evaluation for the 
   current player and the move sequence recommended given the game specified by 
   state [st]. The minimax algorithm searches up to [depth] moves ahead. 
*)
val minimax : t -> int -> ((int*int) list) list -> ((int*int) list) list -> 
  (float * ((int*int) list) list)
