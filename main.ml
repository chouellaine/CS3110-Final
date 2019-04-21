open State
open Command
open Sockets
open Unix
open Ai

type turn = Wait | Go
type player = AI | Player | Host | Client

(*   Game Infrastucture:

     Menu Level 1 accepts commands: "Start", "Quit" 
     Display:  "Welcome to Checker, type Start/Quit, etc"

     Menu Level 2 accepts commands: "Player" , "AI", "Quit"
     Display: "Player vs Player or Player vs AI,etc "

     Menu Level 3 accepts commands: "Rematch", "Quit"
     Display: "Rematch or Quit?" 

     Commands: "Quit", "Start","Player", "AI", 
     "move a1 to b2", "draw", "accept"," reject","rematch", "new game",
     "same", "different", "host", "client"

     "Restart": Starts over to Level 1 
     "Rematch" and "New Game": Starts a new game with the same settings *)

exception Restart 
exception Rematch_Host 
exception Rematch_Client 
exception Rematch_AI 
exception Rematch_Same 

let parse_thunk () = parse(read_line())

(** [helper_string str] prints [str] with a new line. *)
let helper_string str =  
  ANSITerminal.(print_string [red] ("\n \n " ^ str  ^ "\n"))

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
let rec play_game str st ai = 
  (*helper_string "in play_game";*)
  match str() with 
  | Moves -> pp_move_lst (get_all_moves st) ; 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"; 
    play_game (parse_thunk) st ai
  | Move m -> 
    begin match move st m with 
      | Legal st' when not ai -> print_board st'.pieces; 
        helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"; 
        play_game (parse_thunk) st' ai
      | Legal st' when ai -> let st'' = (update_state st' (get_sugg_mv st' 7)) 
        in print_board st''.pieces; 
        helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"; 
        play_game (parse_thunk) st'' ai
      | Legal st' -> failwith "ai was not set to true or false???" 
      | Illegal -> helper_string "Illegal move. Try again.\n>"; 
        play_game (parse_thunk) st ai 
      | Win c when c = Black -> 
        helper_string "Game Over. Black Wins! \n  Quit or Rematch?\n>"; ()
      | Win c when c = Red -> 
        helper_string "Game Over. Red Wins! \n  Quit or Rematch?\n>"; ()
      | Win _ -> failwith "BUG in play_game, Win match!"
    end
  | exception Malformed -> helper_string "Invalid Command. Try again.\n>"; 
    play_game (parse_thunk)st ai 
  | exception Empty -> helper_string "Empty Command. Try again.\n>"; 
    play_game (parse_thunk) st ai 
  | Score -> print_float (get_eval st); 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"; 
    play_game (parse_thunk) st ai
  | Draw -> helper_string "A draw has been offered. Do you accept or reject?\n>";
    accept_or_reject ai st;
  | Quit -> helper_string "Peace out homie."; Pervasives.exit 0
  | Rematch -> helper_string "Starting a new game."; 
    if ai then raise Rematch_AI else raise Rematch_Same 
  | StartOver -> helper_string"Restarting Checkers.";
    raise Restart
  | Opponent _ | Start| Accept | Reject |HostClient _ |SameDiff _ 
    -> helper_string "other cmd, Invalid Command. Try again.\n>"; play_game (parse_thunk) st ai

and accept_or_reject ai s =
  match parse_thunk() with
  | Accept -> 
    helper_string "Draw accepted. The game has been drawn.\n Quit or Rematch?\n>"; () 
  | Reject -> helper_string "Draw rejected. It is still your turn.\n>"; 
    play_game (parse_thunk) s false
  | exception Malformed -> helper_string "Invalid Command. Try again.\n>"; 
    accept_or_reject ai s
  | exception Empty -> helper_string "Empty Command. Try again.\n>"; 
    accept_or_reject ai s
  | StartOver -> raise Restart
  | Start| Quit| Score| Draw| Moves| Opponent _ 
  | Move _ |Rematch |SameDiff _| HostClient _ 
    -> helper_string "You must accept or reject the draw.\n>"; accept_or_reject ai s

let rec host_client_play f_list fd str st = 
  Pervasives.print_newline ();
  match parse str with 
  | Moves -> pp_move_lst (get_all_moves st); 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"; 
    host_client_play f_list fd (read_line ()) st
  | Move m -> 
    begin 
      match move st m with 
      | Legal st' -> let sent = send_substring fd str 0 (String.length str) [] in
        if sent < String.length str then 
          (print_string "The whole command did not send. There is probably a bug.")
        else (
          begin
            match f_list with 
            | Some lst -> write_children lst str
            | None -> ()
          end;
          print_board st'.pieces;
          Pervasives.print_newline ();
          update f_list fd (Bytes.to_string (receive fd)) st')
      | Illegal -> helper_string "Illegal move. Try again."; 
        host_client_play f_list fd (read_line ()) st
      | Win c when c = Black -> 
        helper_string "Game Over. Black Wins! \n  Quit or Rematch?\n>"; ()
      | Win c when c = Red -> 
        helper_string "Game Over. Red Wins! \n  Quit or Rematch?\n>"; ()
      | Win _ -> failwith "BUG in play_game, Win match!"
    end
  | exception Malformed -> helper_string "Invalid Command. Try again.\n>"; 
    host_client_play f_list fd (read_line ()) st
  | exception Empty -> helper_string "Empty Command. Try again."; 
    host_client_play f_list fd (read_line ()) st
  | Score -> print_float (get_eval st); 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"; 
    host_client_play f_list fd (read_line ()) st
  | Draw -> helper_string "A draw has been offered. Do you accept or reject?\n>";
    hc_accept_or_reject f_list fd st;
  | Quit -> helper_string "Peace out homie.\n"; Pervasives.exit 0
  | StartOver -> raise Restart
  | Opponent _ | Start| Accept | Reject | HostClient _ | SameDiff _  |Rematch
    -> helper_string "Invalid Command. Try again.\n>"; 
    host_client_play f_list fd (read_line ()) st

(* TODO: Make compatible with host-client *)
and hc_accept_or_reject f_list fd s =
  match parse_thunk() with
  | Accept -> 
    helper_string "Draw accepted. The game has been drawn.\n Quit or Rematch?\n>"; () 
  | Reject -> helper_string "Draw rejected. It is still your turn.\n>"; 
    host_client_play f_list fd (read_line ()) s
  | exception Malformed -> helper_string "Invalid Command. Try again.\n>";
    hc_accept_or_reject f_list fd s
  | exception Empty -> helper_string "Empty Command. Try again.\n>"; 
    hc_accept_or_reject f_list fd s
  | StartOver -> raise Restart
  | Start| Quit| Score| Draw| Moves| Opponent _ 
  | Move _ |Rematch| SameDiff _| HostClient _ 
    -> helper_string "You must accept or reject the draw.\n>"; 
    hc_accept_or_reject f_list fd s

and update f_list fd str st = 
  begin
    match parse str with
    | Move m ->
      begin
        match move st m with
        | Legal st' -> 
          begin
            match f_list with 
            | Some lst -> write_children lst str
            | None -> ()
          end;
          print_board st'.pieces; 
          Pervasives.print_newline (); 
          helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>";
          host_client_play f_list fd (read_line ()) st'
        | _ -> failwith "move should be legal if it gets to [update]"
      end
    | _ -> failwith "command should be a move if it gets to [update]"
  end

let rec spec_play fd str st = 
  begin
    match parse str with
    | Move m ->
      begin
        match move st m with
        | Legal st' -> 
          print_board st'.pieces; 
          Pervasives.print_newline (); 
          spec_play fd (Bytes.to_string (spec_receive fd)) st'
        | _ -> failwith "move should be legal if it gets to [update]"
      end
    | _ -> failwith "command should be a move if it gets to [update]"
  end


(**[menu_3] runs the game at Level Menu 3.*)
let rec menu_3 a ai=
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n>"; 
      menu_3 parse_thunk ai
    | exception Empty -> helper_string "Empty Command. Try again.\n>" ;
      menu_3 parse_thunk ai
    | Quit -> helper_string "Quitting Game."; Pervasives.exit 0
    | StartOver -> raise Restart; 
    | Rematch -> if ai = AI then raise Rematch_AI 
      else if ai = Player then raise  Rematch_Same
      else if ai = Host then raise Rematch_Host
      else if ai = Client then raise Rematch_Client
    | Score | Draw | Moves | Accept | Reject | Start 
    | Move _ | Opponent _ | SameDiff _|HostClient _
      -> helper_string "Invalid Command. Try again.\n>"; 
      menu_3 parse_thunk ai
  end

let host () = 
  let fd = socket PF_INET SOCK_STREAM 0 in
  let conn_fd,sockaddr = listen_accept fd in
  let f_list = init_spectators fd 4 in
  print_board (new_game ()).pieces;
  helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>";
  Pervasives.print_newline ();
  host_client_play (Some f_list) conn_fd (read_line ()) (new_game ()); menu_3 parse_thunk Host

let client () = 
  let fd = socket PF_INET SOCK_STREAM 0 in
  conn_client fd;
  update None fd (Bytes.to_string (receive fd)) (new_game ()); menu_3 parse_thunk Client

let spectator () = 
  let fd = socket PF_INET SOCK_STREAM 0 in
  conn_spec fd;
  spec_play fd (Bytes.to_string (spec_receive fd)) (new_game ())

let rec menu_5 a=
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n>";
      menu_5 parse_thunk
    | exception Empty -> helper_string "Empty Command. Try again.\n>" ; menu_5 parse_thunk
    | Quit -> helper_string "Quitting Game."; Pervasives.exit 0
    | HostClient a -> a
    | StartOver -> raise Restart
    | Score | Draw | Moves | Accept | Reject 
    | Start | Move _ | Opponent _ | Rematch|SameDiff _ 
      -> helper_string "Invalid Command. Try again.\n>"; menu_5 parse_thunk
  end

let rec menu_4 a=
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n>";
      menu_4 parse_thunk
    | exception Empty -> helper_string "Empty Command. Try again.\n>" ; 
      menu_4 parse_thunk
    | Quit -> helper_string "Quitting Game. \n"; Pervasives.exit 0
    | SameDiff a -> a
    | StartOver -> raise Restart
    | Score | Draw | Moves | Accept | Reject 
    | Start | Move _ | Opponent _ | Rematch |HostClient _ 
      -> helper_string "Invalid Command. Try again.\n>"; menu_4 parse_thunk
  end

(**[menu_2] runs the game at Level Menu 2.*)
let rec menu_2 a= 
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n>"; 
      menu_2 (parse_thunk);
    | exception Empty -> helper_string "Empty Command. Try again.\n>" ;
      menu_2 (parse_thunk); 
    | Opponent p -> p
    | StartOver -> raise Restart
    | Quit -> helper_string "Quitting Game. \n"; Pervasives.exit 0
    | Score | Draw | Moves | Accept | Reject 
    | Start | Move _ |Rematch| SameDiff _|HostClient _ 
      -> helper_string "Invalid Command. Try again.\n>"; menu_2 (parse_thunk);
  end

let play_player_helper()= 
  begin
    print_board (new_game ()).pieces;
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"; 
    play_game (parse_thunk) (new_game ()) false; menu_3 parse_thunk Player 
  end 

let play_ai_helper() = 
  begin 
    print_board (new_game ()).pieces;
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"; 
    play_game (parse_thunk) (new_game ()) true; menu_3 parse_thunk AI 
  end 

let play_player() = 
  try play_player_helper() with Rematch_Same -> 
    helper_string "Starting new game.";play_player_helper()

let play_ai() = 
  try play_ai_helper() with Rematch_AI -> 
    helper_string "Starting new game."; play_ai_helper() 

let play_client() = 
  try client () with Rematch_Client -> 
    helper_string "Starting new game.";client()

let play_host() = 
  try host () with Rematch_Host -> 
    helper_string "Starting new game."; host() 

(**[menu_1] runs the game at Level Menu 1.*)
let rec menu_1 a = 
  begin
    match a() with 
    | exception Malformed  -> helper_string "Invalid Command. Try again.\n>"; 
      menu_1 (parse_thunk);
    | exception Empty -> helper_string "Empty Command. Try again.\n>" ; 
      menu_1 (parse_thunk); 
    | Start -> ()
    | Quit ->  helper_string "Quitting Game."; Pervasives.exit 0
    | Score | Draw | Moves | Accept | Reject | Opponent _ 
    | Move _ |Rematch| SameDiff _|HostClient _ | StartOver 
      -> helper_string "Invalid Command. Try again.\n>"; menu_1 (parse_thunk);
  end 

let start_menu_helper() = 
  helper_string "Welcome to CHECKERS! \n 
    Please enter 'Start' or 'Quit' to move forward.\n>";
  menu_1 (parse_thunk);
  helper_string "Player vs Player or Player vs AI, or do you want to spectate? \n
    Please enter 'player', 'AI' or 'spectate' to move forward.\n>";
  let opp =  menu_2 (parse_thunk) in 
  match opp with 
  | Player  -> (
      helper_string "Do you want to play on the same or different machines?\n>";
      let same_diff = menu_4 (parse_thunk) in
      if same_diff = Same then play_player()
      else (
        helper_string "Do you want to be the host or client?\n>";
        let host_client = menu_5 (parse_thunk) in
        if host_client = Host then 
          try host () with Rematch_Host -> 
            helper_string "Starting new game."; host() 
        else try client () with Rematch_Client -> 
          helper_string "Starting new game.";client()));
  | AI -> play_ai ()
  | Spectate -> spectator ()

let rec start_menu = function 
  | None -> begin try start_menu_helper() with Restart -> main() end 
  | Some t -> if t = Player then try play_player() with Restart -> main()
    else if t = AI then try play_ai() with Restart -> main()
    else if t = Host then try play_host() with Restart -> main()
    else if t = Client then try play_client() with Restart -> main()

and  main () =
  try start_menu None
  with 
  | Restart -> start_menu None 
  | Rematch_Same -> start_menu (Some Player)
  | Rematch_AI -> start_menu (Some AI)
  | Rematch_Host -> start_menu (Some Host) 
  | Rematch_Client -> start_menu (Some Client)

(* Execute the game engine. *)
let () = main()
