(** 
   Representation of checkers game state.

   This module represents the state of a checkers game as it is being played,
   including all of the pieces currrently on the board, their locations,   ,
   and functions that cause the state to change.
*)

type color = 
  | Black
  | Red

type piece = 
  | R of (int * int) 
  | B of (int * int) 
  | RK of (int * int) 
  | BK of (int * int)

(** The abstract type of values representing the game state. *)
type t = {
  pieces: piece list;
  turn: int; 
}

(** [new_game ()] is the state of a new game. All pieces are on the board in 
    their original positions and it is red's turn (turn = 1).  *)
val new_game : unit -> t

(** [get_moves st] is a list of legal moves given the currrent state. *)
val get_moves : t -> piece * ((int * int) list) list 

(** [set_score st points] gets the current number of black pieces minus the 
    current number of red pieces. *)
val get_score : t -> int

(** The type representing the result of an attempted move. *)
type result = Legal of t | Illegal

(** [move st mv] is the result of attempting to make the move specified by [mv]
    If the move is legal, then the result is [Legal st'] where [st'] is the 
    new state after taking the move [mv] in the state [st]. Otherwise, the 
    result is [Illegal]
*)
val move : t -> piece * ((int * int) list) -> result

(* [check_win st] is None if the game is still being played, Some 0 if Red 
   wins, and Some 1 if Black wins *)
val check_win : t -> int option

(** [print_board pieces] prints the board given by the list of pieces and their
    coordinates given by [pieces]  *)
val print_board : piece list -> unit