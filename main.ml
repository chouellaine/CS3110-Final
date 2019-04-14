open State
open Command

(** [helper_string str] prints [str] with a new line. *)
let helper_string str =  
  print_endline (String.concat "" (str :: "\n" :: []))

(** [pp_board p_lst] pretty-prints the checkers board given all of the pieces 
    that are currently in play [p_list]. *)
let pp_board p_lst =
  failwith("unimplemented")


let check_end_game_condition st = 
  failwith("unimplemented")


(** [play_game ()] begins and a game of checkers and plays through it by

    - Printing the initial position of the board with pieces in their 
      starting positions and prompting the user for a move

    - Making moves if a legal move command is made. The computer makes 
      a move in response and prompts the user for another move. 

    - Beginning a new game if the user issues a new game command

    - Ending the game when a win condition occurs

    - Terminating the process if the quit command is issued. 

    - Prompting the user again if an illegal move is made or an unrecognized 
      command is issued
*)
let rec play_game () = 
  failwith("unimplemented")

(** [helper_init p] initializes the initial state of a new game against 
    player or AI [p]. 
    [p] = 0 if player vs player and 1 if player vs AI *)
let helper_init p =  
  failwith("unimplemented")


(** [main ()] prints the prompt for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our checkers game.\n");
  play_game ()

(* Execute the game engine. *)
let () = main ()
