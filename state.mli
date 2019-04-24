(** 
   Representation of checkers game state.

   This module represents the state of a checkers game as it is being played,
   including all of the pieces currrently on the board, their locations,   ,
   and functions that cause the state to change.
*)

(** [pp_move_lst mv_lst] pretty prints the move list [mv_lst] *)
val pp_move_lst : ((int * int) list) list -> unit

(** [new_game ()] is the state of a new game. All pieces are on the board in 
    their original positions and it is red's turn (turn = 1).  *)
val new_game : unit -> Game.t

(** [get_moves st] is a list of legal moves given the currrent state. *)
val get_all_moves : Game.t -> ((int * int) list) list 

(** [get_eval st points] gets the current number of black pieces minus the 
    current number of red pieces. Kings are worth more points than normal pieces. *)
val get_eval : Game.t -> float

(** [get_suicide_eval st points] is the opposite of get_eval. *)
val get_eval_suicide : Game.t -> float

(** The type representing the result of an attempted move. *)
type result = Legal of Game.t | Illegal | Win of Game.t*Game.color

(** [update_state st mv] is the state resulting from making the move [mv] 
    Requires: [mv] is a legal move
*)
val update_state : Game.t -> (int * int) list -> Game.t

(**ADD SEPCS *)
val checkWin: Game.t -> result 

(** [move st mv] is the result of attempting to make the move specified by [mv]
    If the move is legal, then the result is [Legal st'] where [st'] is the 
    new state after taking the move [mv] in the state [st]. Otherwise, the 
    result is [Illegal]
*)
val move : Game.t -> (int * int) list -> result

(** [print_board pieces] prints the board given by the list of pieces and their
    coordinates given by [pieces]  *)
val print_board : Game.piece list -> unit