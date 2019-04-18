open State
open Command
open Sockets
open Unix
open Ai

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
let rec play_game str st ai = 
  match str with 
  | Moves -> pp_move_lst (get_all_moves st) ; 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; play_game (parse_thunk ()) st ai
  | Move m -> 
    begin match move st m with 
      | Legal st' when not ai -> print_board st'.pieces; 
        helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; play_game (parse_thunk ()) st' ai
      | Legal st' when ai -> let st'' = (update_state st' (get_sugg_mv st' 7)) 
        in print_board st''.pieces; 
        helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; 
        play_game (parse_thunk ()) st'' ai
      | Illegal -> helper_string "Illegal move. Try again.\n"; play_game (parse_thunk ()) st ai 
      | Win c when c = Black -> 
        helper_string "Game Over. Black Wins! \n  Quit or Rematch? \n"; ()
      | Win c when c = Red -> 
        helper_string "Game Over. Red Wins! \n  Quit or Rematch? \n"; ()
      | Win _ -> failwith "BUG in play_game, Win match!"
    end
  | exception Malformed -> helper_string "Invalid Command. Try again.\n"; play_game (parse_thunk ())st ai 
  | exception Empty -> helper_string "Empty Command. Try again.\n"; play_game (parse_thunk ()) st ai 
  | Score -> print_float (get_eval st); 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; play_game (parse_thunk ()) st ai
  | Draw -> helper_string "A draw has been offered. Do you accept or reject?\n";
    accept_or_reject st;
  | Quit -> helper_string "Peace out homie.\n"; Pervasives.exit 0
  | Rematch -> helper_string "Cannnot rematch. Must quit or ask for draw. \n"; play_game (parse_thunk ()) st ai
  | Opponent _ | Start| Accept | Reject|HostClient _|SameDiff _ 
    -> helper_string "Invalid Command. Try again.\n"; play_game (parse_thunk ()) st ai

and accept_or_reject s =
  match parse_thunk() with
  | Accept -> helper_string "Draw accepted. The game has been drawn.\n Quit or Rematch? \n"; 
    () 
  | Reject -> helper_string "Draw rejected. It is still your turn. \n"; play_game (parse_thunk ()) s false
  | exception Malformed -> helper_string "Invalid Command. Try again.\n"; accept_or_reject s
  | exception Empty -> helper_string "Empty Command. Try again.\n"; accept_or_reject s
  | Start| Quit| Score| Draw| Moves| Opponent _ | Move _ |Rematch| SameDiff _| HostClient _
    -> helper_string "You must accept or reject the draw"; accept_or_reject s

let receive fd =
  let msg = Bytes.create 64 in
  match Unix.recv fd msg 0 64 [] with
  | len -> Bytes.of_string (String.sub (Bytes.to_string msg) 0 len)

let rec host_client_play fd str st =
  match parse str with 
  | Moves -> pp_move_lst (get_all_moves st) ; 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; 
    host_client_play fd (read_line ()) st
  | Move m -> 
    begin match move st m with 
      | Legal st' -> print_board st'.pieces; 
        helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; 
        let sent = send_substring fd str 0 (String.length str) [] in
        if sent < String.length str then (print_string "The whole command did not send. There is probably a bug")
        else (host_client_play fd (read_line ()) st);
      | Illegal -> helper_string "Illegal move. Try again.\n"; play_game (parse_thunk ()) st false
      | Win c when c = Black -> 
        helper_string "Game Over. Black Wins! \n  Quit or Rematch? \n"; ()
      | Win c when c = Red -> 
        helper_string "Game Over. Red Wins! \n  Quit or Rematch? \n"; ()
      | Win _ -> failwith "BUG in play_game, Win match!"
    end
  | exception Malformed -> helper_string "Invalid Command. Try again.\n"; play_game (parse_thunk ()) st false
  | exception Empty -> helper_string "Empty Command. Try again.\n"; play_game (parse_thunk ()) st false
  | Score -> print_float (get_eval st); 
    helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; play_game (parse_thunk ()) st false 
  | Draw -> helper_string "A draw has been offered. Do you accept or reject?\n";
    accept_or_reject st;
  | Quit -> helper_string "Peace out homie.\n"; Pervasives.exit 0
  | Rematch -> helper_string "Cannnot rematch. Must quit or ask for draw. \n"; play_game (parse_thunk ()) st false
  | Opponent _ | Start| Accept | Reject | HostClient _ | SameDiff _ 
    -> helper_string "Invalid Command. Try again.\n"; play_game (parse_thunk ()) st false 

let listen_accept fd =
  listen fd 1;
  echo "Please give your IP Address and this port number to your opponent: \n";
  echo "Your IP Address: ";
  let code = 
    (if env () = Apple then system 
         "ifconfig en0 | grep broadcast | grep -o 'inet\ [0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*' | grep -o '[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*'"
     else system "ifconfig eth0 | grep broadcast | grep -o 'inet\ [0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*' | grep -o '[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*'")
  in
  echo "";
  if code = WEXITED 0 then (echo ("Port number: \n" ^ string_of_int (find_port fd) ^ "\n");)
  else (echo "We couldn't find your IP Address. You will have to find it manually.\n");
  echo "Waiting for opponent to connect...";
  let (conn_fd, sockaddr) = Unix.accept fd in
  host_client_play conn_fd (Bytes.to_string (receive conn_fd)) (new_game ())

let conn fd = 
  print_string "Please enter your opponent's IP Address:\n";
  let ip = read_line () in
  print_string "Please enter the port number to connect to on your opponents machine:\n";
  let port = read_line () in
  let conn_addr = Unix.ADDR_INET(Unix.inet_addr_of_string ip,int_of_string port) in
  Unix.connect fd conn_addr;
  host_client_play fd (read_line ()) (new_game ())

let init_with_socket_host () =
  listen_accept (socket PF_INET SOCK_STREAM 0)

let init_with_socket_client () =
  conn (socket PF_INET SOCK_STREAM 0)

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
      -> helper_string "Invalid Command. Try again. IN MENU 5\n"; menu_5 parse_thunk
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
      -> helper_string "Invalid Command. Try again. MENU 3, other cmd\n"; menu_3 parse_thunk
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

let start_menu() = 
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
       play_game (parse_thunk ()) (new_game ()))) false 
    else (
      ANSITerminal.(print_string [red] "Do you want to be the host or client?\n");
      let host_client = menu_5 (parse_thunk) in
      if host_client = Host then host () else client ()
    ))
  else print_board (new_game ()).pieces;
  helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'"; 
  play_game (parse_thunk ()) (new_game ()) true
(* menu_3(parse_thunk)  *)

(** [main ()] prints the prompt for the game to play, then starts it. *)
let main () =
  try start_menu ()
  with Restart -> start_menu()

(* Execute the game engine. *)
let () = main()
