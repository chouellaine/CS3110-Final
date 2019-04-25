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

let getScore = function 
  |Suicide -> get_eval_suicide 
  |Regular -> get_eval 

let rec playGame st = 
  move_str();
  let str = read_line() in 
  match parse str with 
  | Moves -> pp_move_lst (get_all_moves st); 
    playGame st
  | Move m -> 
    if st.moves_without_capture = 39 
    then forceDraw st 
    else
      begin match move st m with 
        | Legal st' -> print_board st'.pieces; 
          begin  match st.opp with 
            |Player -> playGame st'
            |AI i -> 
              let st'' = print_board st'.pieces; print_newline (); 
                st.game |> getScore |> get_sugg_mv st' (get_level i)
                |> update_state st' in print_board st''.pieces; 
              begin 
                match check_win st'' with 
                | Win (st',c) when c = Black -> print_board st'.pieces;
                  helper_string "Game Over. Black Wins!"; gameOver st
                | Win (st',c) when c = Red -> print_board st'.pieces;
                  helper_string "Game Over. Red Wins!"; gameOver st
                | Win _ -> failwith "BUG in playGame, Win match!"
                | Legal t -> playGame t
                | Illegal -> failwith "failed in playGame, AI made illegal move"
              end 
            |Client | Host -> sendMove st' st str
          end 
        | Illegal -> helper_string "Illegal move. Try again.\n>"; 
          playGame st 
        | Win (st',c) when c = Black -> print_board st'.pieces;
          helper_string "Game Over. Black Wins!"; gameOver st
        | Win (st',c) when c = Red -> print_board st'.pieces;
          helper_string "Game Over. Red Wins!"; gameOver st
        | Win _ -> failwith "BUG in playGame, Win match!"
      end 
  | exception Malformed -> invalid_str None; playGame st 
  | exception Empty -> empty_str(); playGame st 
  | Score -> st|> getScore st.game |> print_float; playGame st 
  | Draw -> helper_string "Draw Requested"; 
    let f1 () = (draw st) in matchPlayer f1 st str st.opp
  | Quit -> helper_string "Quitting Game";
    let f1 () = (quit (Some st)) in matchPlayer f1 st str st.opp
  | Rematch -> begin match st.opp with 
      | AI _ | Player ->  newgame_str(); 
        let defaultGame = new_game() in 
        let initGame = 
          {defaultGame with opp = st.opp; 
                            connection = st.connection;
                            game=st.game; } in 
        print_board initGame.pieces;
        playGame initGame 
      | Client | Host -> helper_string "Must request a draw before rematching.";
        playGame st
    end 
  | StartOver -> helper_string "Starting Over";
    let f1() = helper_string "Restarting Checkers."; main() in 
    matchPlayer f1 st str st.opp 
  | Save -> 
    helper_string "What do you want to name your save file?";
    save st (read_line ()); quit_str (); Pervasives.exit 0;
  | Opponent _ | Start | Accept | Reject | Watch 
  | HostClient _ | Env _  |Load | Play | GameType _ | Level _ |New |Yes |No
    -> invalid_str None; playGame st 

and matchPlayer f1 st str  = function 
  |Player | AI _ -> f1() 
  |Client | Host -> sendMove st st str

and draw st =
  match st.opp with 
  |AI _ -> helper_string " Draw accepted"; gameOver st 
  |Player|Host|Client -> 
    menu_str(" Your opponent offers a draw.") ["Accept";"Reject"];
    match (matchCommand [Accept; Reject]) with 
    | Accept -> 
      begin match st.opp with 
        | Player ->  helper_string"Draw Accepted."; gameOver st 
        | Client | Host -> helper_string "You've accepted the draw.";
          sendMove st st "accept"
        | AI _ -> failwith "should never reach this point in accept, draw"
      end 
    | Reject -> begin match st.opp with 
        |Player -> helper_string"Draw Rejected. It is still your turn."; playGame st 
        |Client | Host -> helper_string "You've rejected the draw."; 
          sendMove st st "reject"
        |AI _ -> failwith "should never reach this point in reject, draw"
      end 
    | _ -> failwith "failed in draw"

and forceDraw st = 
  helper_string "40 moves were made without progression by either side. The game is a draw.";
  gameOver st

and save t s = to_json t s; helper_string" Game Saved Successfully";

and quit t = 
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

and gameOver st = 
  menu_str" Rematch, Restart, or Quit?" ["Rematch";"Restart";"Quit"];
  match (matchCommand [Rematch; StartOver;Quit]) with 
  | Rematch -> newgame_str(); let defaultGame = new_game() in 
    let initGame = {defaultGame with opp = st.opp; connection = st.connection;
                                     game=st.game; } in 
    print_board initGame.pieces;
    playGame initGame
  | StartOver -> main()
  | Quit -> quit (Some st)
  | _ -> failwith "failed in gameOver"

and update f_list fd str st  = 
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
          playGame st'
        | _ -> failwith "move should be legal if it gets to [update]"
      end
    | Draw -> draw st
    | Quit | StartOver -> restartServer st; 
    | Rematch -> rematchServer st; 
    | _ -> failwith "not a valid command to send in [update]"
  end

and restartServer st =
  menu_str " Your opponent has left the game" ["Restart";"Quit"];
  match (matchCommand [StartOver;Quit]) with 
  | StartOver -> main()
  | Quit -> quit (Some st)
  | _ -> failwith "failed in restartServer"

and rematchServer st = 
  menu_str " Your opponent has requested a Rematch" ["Accept";"Reject"];
  match (matchCommand [Accept;Reject]) with 
  | Accept -> helper_string "You've accepted the rematch. Starting a new game"; 
    begin match st.opp with 
      |Host -> 
        begin match st.connection with 
          | Some (_, fd) -> clientGame fd 
          | None -> failwith "missing connection info in rematchServer,hostNone"
        end 
      |Client -> 
        begin match st.connection with 
          | Some ((Some f_list),conn_fd) -> hostGame f_list conn_fd st.game 
          | None -> failwith "missing connection info in rematchServer,clientNone"
          | _  -> failwith "missing connection info in rematchServer,client"
        end 
      |_ -> failwith "shouldn't reach here in rematchServer"
    end 
  | Reject -> helper_string "You've rejected the rematch."; sendMove st st "quit";
  | _ -> failwith "failed in restartServer" 

and waitDraw f_list fd str st= 
  match parse str with 
  |Accept ->  helper_string "Your opponent has accepted the draw."; gameOver st
  |Reject -> helper_string "Your opponent has rejected the draw.";  playGame st
  |_ -> failwith "failed in waitDraw"

and waitRematch f_list fd str st = 
  match parse str with 
  |Accept -> helper_string "Your opponent has accepted the rematch."; gameOver st
  |Reject -> helper_string "Your opponent has rejected the rematch."; gameOver st
  |_ -> failwith "failed in waitDraw"

and updateState t' f_list fd str = 
  match parse str with 
  | Quit  -> quit (Some t')
  | Draw -> helper_string "You've requested a draw.";
    waitDraw f_list fd (Bytes.to_string (client_receive fd)) t'
  | Rematch ->  helper_string "You've requested a rematch.";
    waitRematch f_list fd (Bytes.to_string (client_receive fd)) t' 
  | StartOver -> main()
  | Move _ -> begin
      match f_list with 
      | Some lst -> write_children lst str
      | None -> ()
    end; 
    print_board t'.pieces;
    Pervasives.print_newline ();
    update f_list fd (Bytes.to_string (client_receive fd)) t'
  | _ -> failwith ("failed in updateMove")

and sendMove t' t str = 
  match t.connection with 
  | None -> failwith "connection info not found in sendMove"
  | Some x -> 
    let fd = snd x in 
    let f_list = fst x in 
    let sent = send_substring fd str 0 (String.length str) [] in 
    if sent < String.length str then 
      (failwith "The whole command did not send. There is probably a bug.";)
    else updateState t' f_list fd str

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
  print_board initGame.pieces;
  playGame initGame;

and clientGame fd = 
  let getGame = Bytes.to_string (spec_receive fd) in 
  let gtype = Game.to_game getGame in 
  let defaultGame = new_game() in 
  let initGame = {defaultGame with connection = Some (None,fd);
                                   game = gtype; opp = Host} in
  let recv = Bytes.to_string (client_receive fd) in
  update None fd recv (initGame);

and specGame fd =
  let getGame = Bytes.to_string (spec_receive fd) in 
  let gtype = Game.to_game getGame in 
  let defaultGame = new_game() in 
  let initGame = {defaultGame with connection = Some (None,fd);
                                   game = gtype; opp = Player} in
  let recv = Bytes.to_string (client_receive fd) in 
  spec_play fd recv initGame;

and sendGame fd g = 
  let g_str = game_to_str g in 
  match send_substring fd g_str 0 (String.length g_str) [] with 
  | exception Unix_error _ -> sendGame fd g
  | x -> if x < String.length g_str then 
      (helper_string "Game Type msg failed to send to client";)
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
  conn_client fd;
  clientGame fd

and spectator () = 
  let fd = socket PF_INET SOCK_STREAM 0 in
  conn_spec fd;
  specGame fd;

and playPlayer g= 
  helper_string "Starting new game.";
  let defaultGame = new_game() in 
  let initGame = {defaultGame with opp = Player; game = g} in 
  print_board initGame.pieces;
  playGame initGame

and playAI a g= 
  helper_string "Starting new game.";
  let defaultGame = new_game() in 
  let initGame = {defaultGame with opp = AI a; game = g} in 
  print_board initGame.pieces;
  playGame initGame

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

and  machineEnv g = 
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

and load() = 
  Unix.chdir "saves";
  match (read_line()^".json") with 
  | exception End_of_file -> ()
  | file -> match Yojson.Basic.from_file file with 
    | exception _ -> helper_string "File Error, try again"; load()
    | j -> let st = from_json j in print_board st.pieces; Unix.chdir "..";
      playGame st

and loadNew() = 
  menu_str " Load Game or New Game?" ["Load Game";"New Game"];
  match (matchCommand [ Load; New ; Quit ]) with 
  | Load ->  helper_string "Enter game file to load:"; 
    let rec list_files dh =
      begin
        match Unix.readdir dh with 
        | exception End_of_file -> ()
        | s when String.length s <= 5 -> list_files dh
        | s -> Pervasives.print_endline (String.sub s 0 (String.length s - 5)); list_files dh
      end in
    let dh = Unix.opendir "saves" in
    list_files dh;
    print_string "\n";
    Unix.closedir dh;
    load()
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
