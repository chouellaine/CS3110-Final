(**
   Parsing of player commands.
*)

(* fst action = piece to be moved, snd action = end location of moved piece*)
type action = (int*int) * (int*int)

type command = 
  | Start
  | Quit 
  | Score 
  | Draw
  | Moves
  | Accept
  | Reject
  | Move of action

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

