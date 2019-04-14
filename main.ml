open State
open Command

(**  Menu Level 1 accepts commands: "Start", "Quit" 
     Display:  "Welcome to blah, type Start/Quit, etc"

     Menu Level 2 accepts commands: "Player" , "AI", "Quit"
     Display: "Player vs Player or Player vs AI,etc "

     Menu Level 3 accepts commands: "Quit", "move","jump","offer draw",
     "accept draw","reject draw," "score"
     Display: the board game, Your commands are "move" ,"jump","offer draw", etc

     Commands: "Quit", "Start","Player", "AI", "jump (x1,y1) to (x2,y2)", 
     "move (x1,y1) to (x2,y2)", "offer draw", "accept draw"," reject draw"*)

type menu = int 

(** [helper_string str] prints [str] with a new line. *)
let helper_string str =  
  print_endline (String.concat "" (str :: "\n" :: []))


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

    - Ending game if a draw has been agreed upon 

    - Prompting the user again if an illegal move is made or an unrecognized 
      command is issued
*)
let rec play_game s = 
  match parse (read_line ()) with 
  | Moves -> (print get_moves s) play_game s
  | _ -> failwith "something died"

(** [helper_init p] initializes the initial state of a new game against 
    player or AI [p]. 
    [p] = 0 if player vs player and 1 if player vs AI *)
let helper_init p =  
  failwith("unimplemented")

let rec menu_2 a= 
  match a with 
  | exception Malformed  -> helper_string "Invalid Command. Try again.\n"; menu_2 (parse(read_line()));
  | exception Empty -> helper_string "Empty Command. Try again.\n" ; menu_2 (parse(read_line())); 
  | Opponent p -> p 
  | Quit -> Pervasives.exit 0
  | _ -> failwith "BUG!!!" 

let menu_3 a = 
  failwith("unimp")

let rec menu_1 a = 
  match a with 
  | exception Malformed  -> helper_string "Invalid Command. Try again.\n"; menu_1 (parse(read_line()));
  | exception Empty -> helper_string "Empty Command. Try again.\n" ; menu_1 (parse(read_line())); 
  | Start -> ()
  | Quit -> Pervasives.exit 0
  | _ -> failwith "BUG!!!" 

(** [main ()] prints the prompt for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to CHECKER ! \n 
    Please type 'Start' or 'Quit' to move forward. \n > \n");
  menu_1 (parse(read_line())); 
  ANSITerminal.(print_string [red]
                  "\n\n Player vs Player or Player vs AI? 
                    Please enter 'player'  or 'AI' to move forward. \n > \n");
  let opp =  menu_2 (parse(read_line())) in 
  if opp = Player then 
    menu_3 else failwith "AI version not implemented"

(* Execute the game engine. *)
let () = main ()
