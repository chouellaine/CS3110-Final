open State
open Command
open Sockets
open Unix
open Ai
open Game 
(*Easy = 3 | Medium = 6 | Hard = 9 | AlphaZero  = 12*)
(*  Game Infrastucture:

    playWatch(): Play Game or Watch Game
    Commands: "Play", "Watch","Quit"

    gameType(): Suicide Checkers or Regular Checkers
    Commands: "Suicide", "Quit", "Regular"

    loadNew(): Load Game or New Game? 
    Commands: "Load", "New", "Quit"

    load(): Load a json game file 
    Commands:  [x].json, "Quit" 

    playerAI(): Player vs Player or Player vs AI?
    Commands: "player", "AI","quit"

    gameLevel(): Level of AI from 1-5 
    Commands: [1-5], "quit"

    machineEnv(): Same or Different Machine? 
    Commands: "same","different","quit"

    hostClient(): Host or Client?
    Commands: "Host","Client"

    playGame():  Play Game/Make a Move 
    Commands: "move a1 to b2", "draw", "moves", "rules",
    "restart", "rematch","quit","save"

    rematchRestart(): Rematch, Restart, or Quit?
    Commands: "Rematch", "Quit", "Restart"

    draw(): Accept or Reject Draw 
    Commands: "Accept","Reject"

    Side note: All commands are case-insensitive.
    "Restart": Starts over to playWatch() 
    "Rematch" and "New Game": Starts a new game with the same settings *)

(** [helper_string str] prints [str] with a new line. *)
let helper_string str =  
  ANSITerminal.(print_string [red] ("\n" ^ str  ^ "\n \n"))

(** [playGame ()] begins and a game of checkers and plays through it by

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
let menu_str q opt = 
  let rec helper acc = function
    | [] -> acc
    | h::t-> helper (acc^"> "^h^"           ") t
  in let str = helper "" opt in helper_string (q^"\n"^str)

let quit_str() = 
  helper_string "Peace Out, Homie."

let invalid_str = function
  |None -> helper_string "Invalid Command. Try Again."
  |Some x -> helper_string ("Invalid Command. Try Again."^ x)

let move_str() = 
  helper_string "It is your turn, enter a move. Ex: 'move e1 to a2'\n>"

let newgame_str() = 
  helper_string "Starting New Game"

let empty_str() = 
  helper_string "Empty Command. Try again.\n>"

let get_level = function 
  | Easy -> 3 
  | Medium -> 6 
  | Hard -> 9
  | AlphaZero -> 12

(** Prompts user for commands in [opt]*)
let rec matchCommand opt = 
  try 
    let input = parse(read_line()) in 
    begin
      if List.mem input opt then input 
      else begin invalid_str None; matchCommand opt end 
    end 
  with
  |Malformed -> invalid_str None; matchCommand opt
  |Empty -> empty_str(); matchCommand opt 

let sendMsg st str = 
  match st.connection with 
  | None -> failwith "connection info not found in sendMove"
  | Some x -> 
    let fd = snd x in 
    let sent = send_substring fd str 0 (String.length str) [] in 
    if sent < String.length str then 
      (failwith "The whole command did not send. There is probably a bug.";)

let getPlayer st = 
  match st.opp with 
  |Client -> Host 
  | Host -> Client 
  | _ -> failwith "getPlayer"

let save t s = to_json t s; helper_string " Game Saved Successfully"

let quit t = 
  match t with 
  |None -> quit_str(); Pervasives.exit 0
  |Some x -> 
    menu_str " Save game before quitting?" ["Yes";"No"];
    match (matchCommand [Yes;No]) with 
    | Yes -> 
      helper_string "What do you want to name your save file?";
      save x (read_line ()); quit_str (); Pervasives.exit 0
    | No -> quit_str(); Pervasives.exit 0
    | _ -> failwith "failed in quit"

let sendQuit st = sendMsg st "quit"; quit (Some st)

let getScore = function 
  |Suicide -> get_eval_suicide 
  |Regular -> get_eval 

let rec load() = 
  Unix.chdir "saves";
  match (read_line()^".json") with 
  | exception End_of_file -> failwith "failed in load"
  | file -> match Yojson.Basic.from_file file with 
    | exception _ -> helper_string "File Error, try again"; load()
    | j ->  from_json j 

let rec playNetwork st = 
  if st.moves_without_capture = 39 
  then forceDraw st gameOverNetwork
  else begin move_str();
    let str= read_line() in 
    match parse str with 
    | Board -> print_board st.pieces; playNetwork st
    | Moves -> pp_move_lst (get_all_moves st); playNetwork st
    | Score -> st |> getScore st.game |> print_float; playNetwork st
    | Draw -> sendReq st Draw
    | StartOver -> sendMsg st "quit"; main() 
    | Quit -> sendQuit st
    | Save -> helper_string "What do you want to name your save file?";
      save st (read_line ()); quit_str (); Pervasives.exit 0;
    | Move m -> sendMove st m str
    | Rematch -> helper_string "Must request a draw before rematching."; playNetwork st
    | exception Malformed -> invalid_str None; playNetwork st
    | exception Empty -> empty_str(); playNetwork st
    | Opponent _ | Start | Watch | Level _ | No  | Accept | Reject
    | HostClient _ | Env _ | Load | Play | GameType _ | New | Yes 
      -> invalid_str None; playNetwork st
  end 

and recvMove st = 
  let msg = match st.connection with 
    | None -> failwith "connection info not found in recvMove"
    | Some x -> snd x |> client_receive |> Bytes.to_string  in
  match parse msg with 
  | Accept -> matchRequest st Accept
  | Reject -> matchRequest st Reject
  | Draw -> respReq st Draw
  | Rematch -> respReq st Rematch 
  | Quit -> helper_string "Your Opponent has left the game."; quitRestart st
  | Move m -> respMove st m msg 
  | exception Malformed -> failwith "received malformed command in recvMove"
  | exception Empty -> failwith "received empty command in recvMove"
  | Opponent _ | Start | Watch | Level _ | No | Score | Moves |StartOver
  | HostClient _ | Env _ | Load | Play | GameType _ | New | Yes | Save | Board
    ->  failwith "received invalid command in recvMove"

and sendMove st m msg = 
  match move st m with 
  | Legal st' -> updateSpec st' msg; print_board st'.pieces; 
    sendMsg st' msg; recvMove st'
  | Illegal -> helper_string "Illegal move. Try again.\n>"; playNetwork st 
  | _ -> failwith "failed in sendMove"

and respMove st m msg = 
  match move st m with 
  | Legal st' -> updateSpec st' msg; print_board st'.pieces; playNetwork st'
  | Illegal -> failwith "Opp sent an illegal move in respMove"
  | _ -> failwith "failed in respMove"

and matchRequest st r = 
  let p = getPlayer st in  
  let st' = {st with request = None} in 
  match st.request,r with 
  | None, _ -> failwith "received invalid command from opponent in matchRequest"
  | Some (a, Draw), Accept when a=p -> helper_string "Draw Accepted"; 
    gameOverNetwork st';
  | Some (a, Draw), Reject when a=p-> helper_string "Draw Rejected";
    playNetwork st' 
  | Some (a, Rematch),Accept when a=p ->helper_string "Rematch Accepted";
    newNetworkGame st' 
  | Some (a, Rematch), Reject when a=p-> helper_string "Rematch Rejected"; 
    quitRestart st'
  | _ -> failwith "failed in matchRequest"

and sendReq st req = 
  let p = getPlayer st in  
  match st.request with 
  | None -> 
    begin match req with 
      | Rematch -> helper_string "You have requested a rematch."; 
        let st' = {st with request = Some (p,Rematch)} in 
        sendMsg st' "rematch"; recvMove st' 
      | Draw -> helper_string "You have requested a draw."; 
        let st' = {st with request = Some (p,Draw)} in 
        sendMsg st' "draw"; recvMove st' 
    end 
  | Some (a,Rematch) when a <> p && req = Rematch -> 
    helper_string "Rematch Accepted"; newNetworkGame st
  | Some (a,Draw) when a <> p && req = Draw -> 
    helper_string "Draw Accepted"; gameOverNetwork st
  | _ -> failwith "failed in sendReq"

and respReq st req = 
  let p = getPlayer st in  
  match st.request with 
  | None -> let st' = {st with request = Some (st.opp,req)} in acceptReject st' req
  | Some (a,Rematch) when a = p && req = Rematch ->  
    helper_string "Rematch Accepted"; newNetworkGame st
  | Some (a,Draw) when a = p && req = Draw -> 
    helper_string "Draw Accepted"; gameOverNetwork st
  | _ -> failwith "failed in respReq"

and acceptReject st req = 
  let str = 
    begin 
      match req with  
      | Rematch -> "Rematch" 
      | Draw -> "Draw"
    end in menu_str(" Your opponent offers a "^str^".") ["Accept";"Reject"];
  let st' = {st with request = None} in 
  match (matchCommand [Accept; Reject]) with 
  | Accept -> helper_string (str^" Accepted"); sendMsg st' "accept"; 
    begin match req with 
      | Rematch -> newNetworkGame st' 
      | Draw -> gameOverNetwork st'
    end 
  | Reject -> helper_string (str^" Rejected"); sendMsg st' "reject"; 
    begin match req with 
      | Rematch -> helper_string "Rematch Rejected"; quitRestart st'
      | Draw -> print_board st'.pieces; recvMove st' 
    end 
  | _ -> failwith "failed in acceptReject"

and newNetworkGame st =
  newgame_str(); let defaultGame = new_game() in 
  let initGame = {defaultGame with opp = st.opp; connection = st.connection;
                                   game=st.game; } in 
  print_board initGame.pieces;
  match st.opp with 
  | Client -> playNetwork initGame
  | Host -> recvMove initGame
  | _ -> failwith "failed in newNetworkGame"

and gameOverNetwork st =   
  menu_str" Rematch, Restart, or Quit?" ["Rematch";"Restart";"Quit"];
  match (matchCommand [Rematch;StartOver;Quit]) with 
  | Rematch -> sendReq st Rematch
  | StartOver -> sendMsg st "quit"; main()
  | Quit -> sendQuit st
  | _ -> failwith "failed in gameOver"

and quitRestart st = 
  menu_str" Restart, or Quit?" ["Restart";"Quit"];
  match (matchCommand [StartOver;Quit]) with 
  | StartOver -> sendMsg st "quit"; main()
  | Quit -> sendQuit st
  | _ -> failwith "failed in gameOver"

and moveLocal st m = 
  match move st m with 
  | Legal st' -> print_board st'.pieces; 
    begin match st.opp with 
      |Player -> playLocal st'
      |AI i ->  let st'' = 
                  st.game |> getScore |> get_sugg_mv st' (get_level i)
                  |> update_state st' in print_board st''.pieces;
        begin 
          match checkWin st'' with 
          | Win (st',c) when c = Black -> print_board st'.pieces;
            helper_string "Game Over. Black Wins!"; gameOver st
          | Win (st',c) when c = Red -> print_board st'.pieces;
            helper_string "Game Over. Red Wins!"; gameOver st
          | Win _ -> failwith "BUG in playGame, Win match!"
          | Legal t -> playLocal t
          | Illegal -> failwith "failed in playGame, AI made illegal move"
        end  
      | _ -> failwith "failed in moveLocal"
    end 
  | Illegal -> helper_string "Illegal move. Try again.\n>";  playLocal st 
  | Win (st',c) when c = Black -> print_board st'.pieces;
    helper_string "Game Over. Black Wins!"; gameOver st
  | Win (st',c) when c = Red -> print_board st'.pieces;
    helper_string "Game Over. Red Wins!"; gameOver st
  | Win _ -> failwith "BUG in playGame, Win match!"

and playLocal st = 
  if st.moves_without_capture = 39 
  then forceDraw st gameOver
  else begin move_str();
    match parse(read_line()) with 
    | Board -> print_board st.pieces; playLocal st
    | Moves -> pp_move_lst (get_all_moves st); playLocal st
    | Score -> st |> getScore st.game |> print_float; playLocal st
    | Save ->  helper_string "What do you want to name your save file?";
      save st (read_line ()); quit_str (); Pervasives.exit 0;
    | Draw -> helper_string "Draw Requested"; draw st
    | StartOver -> helper_string "Restarting Checkers."; main()
    | Quit -> quit(Some st)
    | Rematch -> rematchLocal st 
    | Move m -> moveLocal st m
    | exception Malformed -> invalid_str None; playLocal st 
    | exception Empty -> empty_str(); playLocal st 
    | Opponent _ | Start | Accept | Reject | Watch 
    | HostClient _ | Env _  |Load | Play | GameType _ | Level _ |New |Yes |No
      -> invalid_str None; playLocal st 
  end 

and rematchLocal st = 
  helper_string "Rematching";
  newgame_str(); let defaultGame = new_game() in 
  let initGame = 
    {defaultGame with opp = st.opp; game=st.game;} in 
  print_board initGame.pieces;
  playLocal initGame 

and draw st =
  match st.opp with 
  | Host | Client -> failwith "failed in draw, Client/Host"
  | AI _ -> helper_string " Draw Accepted"; gameOver st 
  | Player -> 
    menu_str(" Your opponent offers a draw.") ["Accept";"Reject"];
    match (matchCommand [Accept; Reject]) with 
    | Accept -> helper_string "Draw Accepted"; gameOver st 
    | Reject -> helper_string"Draw Rejected. It is still your turn."; playLocal st 
    | _ -> failwith "failed in draw"

and forceDraw st f= 
  helper_string "40 moves were made without progression by either side. 
  The game is a draw."; f st

and gameOver st = 
  menu_str" Rematch, Restart, or Quit?" ["Rematch";"Restart";"Quit"];
  match (matchCommand [Rematch; StartOver;Quit]) with 
  | Rematch -> rematchLocal st
  | StartOver -> helper_string "Restarting Checkers."; main()
  | Quit -> quit (Some st)
  | _ -> failwith "failed in gameOver"

and updateSpec st str = 
  match st.connection with 
  |Some c -> 
    begin match fst c with 
      | Some lst -> write_children lst str
      | None -> ()
    end 
  | None -> failwith "failed in updateSpec, connection not found"

and spec_play fd str st = 
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

and hostGame f_list conn_fd g = 
  let defaultGame = new_game() in 
  let initGame = {defaultGame with connection = Some ((Some f_list),conn_fd);
                                   game = g; opp = Client} in 
  print_board initGame.pieces; playNetwork initGame

and clientGame fd = 
  let getGame = Bytes.to_string (spec_receive fd) in 
  let gtype = Game.to_game getGame in 
  let defaultGame = new_game() in 
  let initGame = {defaultGame with connection = Some (None,fd);
                                   game = gtype; opp = Host} in
  print_board initGame.pieces; recvMove initGame

and specGame fd =
  let getGame = Bytes.to_string (spec_receive fd) in 
  print_string getGame;
  let gtype = Game.to_game getGame in 
  let defaultGame = new_game() in 
  let initGame = {defaultGame with connection = Some (None,fd);
                                   game = gtype; opp = Player} in
  let recv = Bytes.to_string (client_receive fd) in 
  spec_play fd recv initGame

and sendGame fd g = 
  let g_str = game_to_str g in 
  match send_substring fd g_str 0 (String.length g_str) [] with 
  | exception Unix_error _ -> sendGame fd g
  | x -> if x < String.length g_str then 
      (helper_string "Game Type msg failed to send to client") 
    else ()

and host g = 
  helper_string "Starting new game.";
  let fd = socket PF_INET SOCK_STREAM 0 in
  let conn_fd,sockaddr = listen_accept fd 4 in
  let f_list = init_spectators fd 4 in
  sendGame conn_fd g; 
  write_children f_list (game_to_str g);
  hostGame f_list conn_fd g

and client () = 
  helper_string "Starting new game.";
  let fd = socket PF_INET SOCK_STREAM 0 in
  conn_client fd; clientGame fd

and spectator () = 
  let fd = socket PF_INET SOCK_STREAM 0 in
  conn_spec fd;
  spec_play fd (Bytes.to_string (spec_receive fd)) (new_game ())

and playPlayer g= 
  helper_string "Starting new game.";
  let defaultGame = new_game() in 
  let initGame = {defaultGame with opp = Player; game = g} in 
  print_board initGame.pieces; playLocal initGame

and playAI a g= 
  helper_string "Starting new game.";
  let defaultGame = new_game() in 
  let initGame = {defaultGame with opp = AI a; game = g} in 
  print_board initGame.pieces; playLocal initGame

and hostClient g =
  menu_str" Do you want to be the host or client?" ["Host";"Client"];
  match (matchCommand [HostClient Host; HostClient Client;Quit]) with 
  | HostClient a -> 
    begin 
      match a with 
      |Host -> host g
      |Client -> client()
    end 
  | Quit -> quit None
  | _ -> failwith "failed in hostClient"

and machineEnv g = 
  menu_str" Do you want to play on the same or different machines?" ["Same";"Different"];
  match (matchCommand [Env Same; Env Different;Quit]) with 
  | Env a -> 
    begin
      match a with 
      |Same -> playPlayer g
      |Different -> hostClient g
    end  
  | Quit -> quit None
  | _ -> failwith "failed in machineEnv"

and gameLevel g  = 
  menu_str " Choose an AI difficulty: " ["Easy";"Medium";"Hard";"AlphaZero"];
  match (matchCommand [ Level Easy; Level Medium; Level Hard; Level AlphaZero]) with 
  | Level a -> playAI a g
  | Quit -> quit None
  | _ -> failwith "failed in gameLevel"

and playerAI g = 
  menu_str " Player vs Player or Player vs AI?" ["Player";"AI"];
  match (matchCommand [ Opponent Player; Opponent AI ; Quit ]) with 
  | Opponent a -> 
    begin
      match a with
      | AI -> gameLevel g
      | Player -> machineEnv g
    end   
  | Quit -> quit None
  | _ -> failwith "failed in playerAI"

and gameType() = 
  menu_str " Suicide Checkers or Regular Checkers?" ["Suicide";"Regular"];
  match (matchCommand [ GameType Suicide; GameType Regular ; Quit ]) with 
  | GameType g -> playerAI g
  | Quit -> quit None
  | _ -> failwith "failed in gameType"

and loadNew() = 
  menu_str " Load Game or New Game?" ["Load Game";"New Game"];
  match (matchCommand [ Load; New ; Quit ]) with 
  | Load ->  helper_string "Enter game file to load:"; 
    let rec list_files dh =
      begin
        match Unix.readdir dh with 
        | exception End_of_file -> ()
        | s when String.length s <= 5 -> list_files dh
        | s -> Pervasives.print_endline (String.sub s 0 (String.length s - 5)); 
          list_files dh
      end in
    let dh = Unix.opendir "saves" in
    list_files dh;
    print_string "\n";
    Unix.closedir dh;
    let st = load() in print_board st.pieces; Unix.chdir ".."; playLocal st
  | New -> gameType() 
  | Quit -> quit None
  |  _ -> failwith "failed in loadNew"

and playWatch() = 
  menu_str " Do you want to play a game or watch a game?" ["Play";"Watch"];
  match (matchCommand [ Play; Watch ; Quit]) with 
  | Play -> loadNew() 
  | Watch -> spectator()
  | Quit -> quit None
  | _ -> failwith "failed in playWatch"

and main () =
  ANSITerminal.resize 150 50;
  helper_string " Welcome to AJAE";
  playWatch()

(* Execute the game engine. *)
let () = main() 
