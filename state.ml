type piece = 
  | R of (int * int) 
  | B of (int * int) 
  | RK of (int * int) 
  | BK of (int * int)

type t = {
  pieces: piece list;
  turn: int; 
}

let new_game () = 
  {
    pieces = [
      R (1, 1); 
      (* TODO: etc. *)
    ];
    turn = 1; 
  }

(** [get_moves st] is a list of legal moves given the currrent state. *)
let get_moves = 
  failwith("unimplemented")

(** [set_score st points] gets the current number of red pieces minus the 
    current number of black pieces. *)
let get_score st = 
  let rec helper acc = function
    | [] -> acc
    | h::t -> 
      match h with 
      | RK _ | R _ -> helper (acc + 1) t
      | BK _ | B _ -> helper (acc - 1) t
  in helper 0 st.pieces


(** The type representing the result of an attempted move. *)
type result = Legal of t | Illegal

(** [move st mv] is the result of attempting to make the move specified by [mv]
    If the move is legal, then the result is [Legal st'] where [st'] is the 
    new state after taking the move [mv] in the state [st]. Otherwise, the 
    result is [Illegal]
*)
let move st mv = 
  failwith("unimplemented")


