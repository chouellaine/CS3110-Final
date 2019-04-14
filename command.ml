(*  Note: 
     Menu Level 1 accepts commands: "Start", "Quit" 
     Display:  "Welcome to blah, type Start/Quit, etc"

     Menu Level 2 accepts commands: "Player" , "AI", "Quit"
     Display: "Player vs Player or Player vs AI,etc "

     Menu Level 3 accepts commands: "Quit", "move","jump","offer draw",
     "accept draw","reject draw," "score"
     Display: the board game, Your commands are "move" ,"jump","offer draw", etc

     Commands: "Quit", "Start","Player", "AI", "jump (x1,y1) to (x2,y2)", 
     "move (x1,y1) to (x2,y2)", "offer draw", "accept draw"," reject draw"

     DISCUSS: are we allowing users to ask for valid moves? 

     TODO:  
     check command validity: 
    - check if player jumps over all possible pieces 
    - check if desired destination is empty 
    - check if piece at can be crowned
    - if not a king, check if piece is moving diagonally (left/right) forward one spot 
       AND if there are no available forward jumps to make 
    - if king, check if piece is moving diagonally (left/right) forward/backward one spot
       AND if there are no avialble forward/backward jumps to make 
    - "move" command can also be interpreted as a "jump" command, but 
       "jump" command MUST only be jumping over opponent's pieces. 
    - end game after 50 moves
    - if player vs player, and one person  "offer draw," other player must either 
       "accept draw" or "reject draw"
    - if player vs AI, and player "offer draw", AI always accepts draw (discuss with team)

     Update move history (number of moves, score?) <-- potentially needed for AI? 
     Tell state to print the updated board state 
*) 

type action = (int*int) list 
type ptype = Player | AI 

type command = 
  | Start
  | Quit 
  | Score 
  | Draw
  | Moves
  | Accept
  | Reject 
  | Opponent of ptype
  | Move of action

exception Empty

exception Malformed

(** [move_parse is_to lst newlst] raises [Malformed] if the word "to" does not 
    appear between board coordinates or if "to" appears at the end of the 
    command. *)
let rec move_parse is_to lst newlst =
  match lst with
  | [] -> newlst
  | h::t when h = "" -> move_parse is_to t newlst
  | h :: [] when h = "to" -> raise Malformed
  | h :: t when h = "to" 
    -> if is_to then move_parse false t newlst else raise Malformed
  | h :: t when h <> "to"
    -> if is_to then raise Malformed else move_parse true t (h::newlst)
  | _ -> failwith "shouldn't get here"

(** [parse_rec lst newlst] is the [lst] with all empty strings and substring "to" removed.
    The substring "to" is case-insensitive. *)
let parse_rec lst newlst =
  match lst with
  | [] -> newlst  
  | h :: [] -> [h]
  | h :: t when h <> "move" ->  raise Malformed
  | h :: t when h = "move" -> move_parse false t ["move"]
  | _ -> failwith "shouldn't get here"

(** [convert_coord] is [c] in (x,y) coordinate format
    Raises Malform if [c] is not within the board range. *)
let convert_coord c = 
  if String.length c > 2 then raise Malformed else 
    match (Char.code (Char.lowercase_ascii c.[0]) ,Char.code (Char.lowercase_ascii c.[1])) with 
    | x,y when x > 96 && x < 105 && y > 48 && y < 57 -> (x mod 96,y mod 48)
    | _,_ -> raise Malformed

(** [read_cmd lst] is the appropriate [command] from the the user input.
    Raises [Malformed] if the input is malformed. *) 
let read_cmd = function
  | [] -> raise Malformed 
  | h :: [] when String.lowercase_ascii h = "start" -> Start
  | h :: t when String.lowercase_ascii h = "start" -> raise Malformed
  | h :: [] when String.lowercase_ascii h = "quit" -> Quit
  | h :: t when String.lowercase_ascii h = "quit" -> raise Malformed
  | h :: [] when String.lowercase_ascii h =  "score" -> Score
  | h :: t when String.lowercase_ascii h = "score" -> raise Malformed
  | h :: [] when String.lowercase_ascii h = "draw" -> Draw
  | h :: t when String.lowercase_ascii h = "draw" -> raise Malformed
  | h :: [] when String.lowercase_ascii h = "moves" -> Moves
  | h :: t when String.lowercase_ascii h = "moves" -> raise Malformed
  | h :: [] when String.lowercase_ascii h = "accept" -> Accept
  | h :: t when String.lowercase_ascii h = "accept" -> raise Malformed
  | h :: [] when String.lowercase_ascii h = "reject" -> Reject
  | h :: t when String.lowercase_ascii h = "reject" -> raise Malformed
  | h :: [] when String.lowercase_ascii h = "player" -> Opponent (Player)
  | h :: t when String.lowercase_ascii h = "player" -> raise Malformed
  | h :: [] when String.lowercase_ascii h = "ai" -> Opponent (AI)
  | h :: t when String.lowercase_ascii h = "ai" -> raise Malformed
  | h :: [] when String.lowercase_ascii h = "move" -> raise Malformed 
  | h :: _ :: [] when String.lowercase_ascii h = "move" -> raise Malformed
  | h :: t  when String.lowercase_ascii h = "move" -> Move (List.map convert_coord t)
  | h :: [] when String.lowercase_ascii h = "jump" -> raise Malformed 
  | h :: _ :: [] when String.lowercase_ascii h = "jump" -> raise Malformed
  | h :: t  when String.lowercase_ascii h = "jump" -> Move (List.map convert_coord t)
  | _ :: _ -> raise Malformed 

(** [parse str] parses player's command into a command. 
    Raise Empty if str is [] and Malformed if str is an invalid command. *)
let parse str =
  let split = String.split_on_char ' ' str in 
  let lst = List.rev (parse_rec split []) in 
  if lst = [] then raise Empty 
  else read_cmd lst 