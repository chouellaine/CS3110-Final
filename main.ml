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

(**DO SPECS *)
let parse_thunk () = parse(read_line())

(** [helper_string str] prints [str] with a new line. *)
let helper_string str =  
  ANSITerminal.(print_string [red] 
  ("\n \n " ^ str ^ " \n > \n"))

let check_end_game_condition st = 
  failwith("unimplemented check_end_game_condition")


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
  match parse_thunk() with 
  | Moves -> pp_move_lst (get_all_moves s) ; 
  helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; play_game s
  | Move m -> 
  begin match move s m with 
      | Legal s' -> print_board s'.pieces; 
      helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; play_game s'
      | Illegal -> helper_string "Illegal move. Try again.\n"; play_game s
    end
  | exception Malformed -> helper_string "Invalid Command. Try again.\n"; play_game s
  | exception Empty -> helper_string "Empty Command. Try again.\n"; play_game s
  | Draw -> helper_string "A draw has been offered. Do you accept or reject?\n";
    accept_or_reject s;
  | Quit -> helper_string "Peace out homie.\n"; Pervasives.exit 0
  | _ -> failwith "something died"

and accept_or_reject s =
  match parse_thunk() with
  | Accept -> helper_string "Draw accepted. The game has been drawn.\n";
  | Reject -> helper_string "Draw rejected.\n"; play_game s
  | exception Malformed -> helper_string "Invalid Command. Try again.\n"; accept_or_reject s
  | exception Empty -> helper_string "Empty Command. Try again.\n"; accept_or_reject s
  |Start| Quit| Score| Draw| Moves| Opponent _|Move _ 
    -> helper_string "You must accept or reject the draw"; accept_or_reject s

let rec menu_2 a= 
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n"; menu_2 (parse_thunk);
    | exception Empty -> helper_string "Empty Command. Try again.\n" ; menu_2 (parse_thunk); 
    | Opponent p -> p
    | Quit -> helper_string "Quitting Game. \n"; Pervasives.exit 0
    | Score | Draw | Moves | Accept | Reject | Start | Move _ 
      -> helper_string "Invalid Command. Try again.\n"; menu_2 (parse_thunk);
  end

let rec menu_1 a = 
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n"; 
    menu_1 (parse_thunk);
    | exception Empty -> helper_string "Empty Command. Try again.\n" ; 
    menu_1 (parse_thunk); 
    | Start -> ()
    | Quit ->  helper_string "Quitting Game. \n"; Pervasives.exit 0
    | Score | Draw | Moves | Accept | Reject | Opponent _ | Move _ 
      -> helper_string "Invalid Command. Try again.\n"; 
      menu_1 (parse_thunk);
  end 

(** [main ()] prints the prompt for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to CHECKER ! \n 
    Please enter 'Start' or 'Quit' to move forward. \n > \n");
  menu_1 (parse_thunk); 
  ANSITerminal.(print_string [red]
                  "\n\n Player vs Player or Player vs AI? \n 
                  Please enter 'player' or 'AI' to move forward. \n > \n");
  let opp =  menu_2 (parse_thunk) in 
  if opp = Player then (
    print_board (new_game ()).pieces;
    (helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; 
    play_game (new_game ()))) else failwith "AI version not implemented"

(* Execute the game engine. *)
let () = main ()
