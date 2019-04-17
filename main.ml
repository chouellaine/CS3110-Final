open State
open Command
open Sockets

(*   Game Infrastucture:

     Menu Level 1 accepts commands: "Start", "Quit" 
     Display:  "Welcome to Checker, type Start/Quit, etc"

     Menu Level 2 accepts commands: "Player" , "AI", "Quit"
     Display: "Player vs Player or Player vs AI,etc "

     Menu Level 3 accepts commands: "Rematch", "Quit"
     Display: "Rematch or Quit?" 

     Commands: "Quit", "Start","Player", "AI", 
     "move a1 to b2", "draw", "accept"," reject","rematch"*)

exception Restart 

let parse_thunk () = parse(read_line())

(** [helper_string str] prints [str] with a new line. *)
let helper_string str =  
  ANSITerminal.(print_string [red] 
                  ("\n \n " ^ str ^ " \n > \n"))

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
      | Win c when c = Black -> 
        helper_string "Game Over. Black Wins! \n  Quit or Rematch? \n"; ()
      | Win c when c = Red -> 
        helper_string "Game Over. Red Wins! \n  Quit or Rematch? \n"; ()
      | Win _ -> failwith "BUG in play_game, Win match!"
    end
  | exception Malformed -> helper_string "Invalid Command. Try again.\n"; play_game s
  | exception Empty -> helper_string "Empty Command. Try again.\n"; play_game s
  | Score -> print_int (get_score s); 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; play_game s
  | Draw -> helper_string "A draw has been offered. Do you accept or reject?\n";
    accept_or_reject s;
  | Quit -> helper_string "Peace out homie.\n"; Pervasives.exit 0
  | _ -> failwith "something died"

and accept_or_reject s =
  match parse_thunk() with
  | Accept -> helper_string "Draw accepted. The game has been drawn.\n Quit or Rematch? \n"; 
    () 
  | Reject -> helper_string "Draw rejected. It is still your turn. \n"; play_game s
  | exception Malformed -> helper_string "Invalid Command. Try again.\n"; accept_or_reject s
  | exception Empty -> helper_string "Empty Command. Try again.\n"; accept_or_reject s
  | Start| Quit| Score| Draw| Moves| Opponent _ |Move _ |Rematch| SameDiff _|HostClient _
    -> helper_string "You must accept or reject the draw"; accept_or_reject s

let host () = 
  init_with_socket_host ()

let client () = 
  init_with_socket_client ()

let rec menu_5 a=
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again. IN MENU 4 \n"; menu_5 parse_thunk
    | exception Empty -> helper_string "Empty Command. Try again.\n" ; menu_5 parse_thunk
    | Quit -> helper_string "Quitting Game. \n"; Pervasives.exit 0
    | HostClient a -> a
    | Score | Draw | Moves | Accept | Reject | Start | Move _ | Opponent _ | Rematch|SameDiff _
      -> helper_string "Invalid Command. Try again.\n"; menu_5 parse_thunk
  end

let rec menu_4 a=
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again. IN MENU 4 \n"; menu_4 parse_thunk
    | exception Empty -> helper_string "Empty Command. Try again.\n" ; menu_4 parse_thunk
    | Quit -> helper_string "Quitting Game. \n"; Pervasives.exit 0
    | SameDiff a -> a
    | Score | Draw | Moves | Accept | Reject | Start | Move _ | Opponent _ | Rematch|HostClient _
      -> helper_string "Invalid Command. Try again.\n"; menu_4 parse_thunk
  end

(**[menu_3] runs the game at Level Menu 3.*)
let rec menu_3 a=
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again. IN MENU 3 \n"; menu_3 parse_thunk
    | exception Empty -> helper_string "Empty Command. Try again.\n" ; menu_3 parse_thunk
    | Quit -> helper_string "Quitting Game. \n"; Pervasives.exit 0
    | Rematch -> raise Restart  
    | Score | Draw | Moves | Accept | Reject | Start | Move _ | Opponent _ | SameDiff _|HostClient _
      -> helper_string "Invalid Command. Try again.\n"; menu_3 parse_thunk
  end

(**[menu_2] runs the game at Level Menu 2.*)
let rec menu_2 a= 
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n"; menu_2 (parse_thunk);
    | exception Empty -> helper_string "Empty Command. Try again.\n" ; menu_2 (parse_thunk); 
    | Opponent p -> p
    | Quit -> helper_string "Quitting Game. \n"; Pervasives.exit 0
    | Score | Draw | Moves | Accept | Reject | Start | Move _ |Rematch| SameDiff _|HostClient _
      -> helper_string "Invalid Command. Try again.\n"; menu_2 (parse_thunk);
  end

(**[menu_1] runs the game at Level Menu 1.*)
let rec menu_1 a = 
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n"; 
      menu_1 (parse_thunk);
    | exception Empty -> helper_string "Empty Command. Try again.\n" ; 
      menu_1 (parse_thunk); 
    | Start -> ()
    | Quit ->  helper_string "Quitting Game. \n"; Pervasives.exit 0
    | Score | Draw | Moves | Accept | Reject | Opponent _ | Move _ |Rematch| SameDiff _|HostClient _
      -> helper_string "Invalid Command. Try again.\n"; 
      menu_1 (parse_thunk);
  end 

let play_game() = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to CHECKERS! \n 
    Please enter 'Start' or 'Quit' to move forward. \n > \n");
  menu_1 (parse_thunk); 
  ANSITerminal.(print_string [red]
                  "\n\n Player vs Player or Player vs AI? \n 
                  Please enter 'player' or 'AI' to move forward. \n > \n");
  let opp =  menu_2 (parse_thunk) in 
  if opp = Player then (
    ANSITerminal.(print_string [red] "Do you want to play on the same or different machines?\n");
    let same_diff = menu_4 (parse_thunk) in
    if same_diff = Same then (
      print_board (new_game ()).pieces;
      (helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; 
       play_game (new_game ())))
    else 
      ANSITerminal.(print_string [red] "Do you want to be the host or client?\n");
    let host_client = menu_5 (parse_thunk) in
    if host_client = Host then host () else client ()
  )
  else failwith "AI version not implemented";
  menu_3(parse_thunk) 

(** [main ()] prints the prompt for the game to play, then starts it. *)
let main () =
  try play_game()
  with Restart -> play_game()

(* Execute the game engine. *)
let () = main()
