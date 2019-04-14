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

type command = 
  | Start
  | Quit 
  | Score 
  | Draw
  | Moves
  | Accept
  | Reject
  | Move of action

exception Empty

exception Malformed

(** UPDATE 
    [parse_rec] is the [lst] with all empty strings removed. *)
let rec parse_rec lst newlst = 
  match lst with
  | [] -> newlst  
  | h :: t when h = "" ->  parse_rec t newlst
  | h :: t -> parse_rec t (h :: newlst) 

(** UPDATE
    [read_cmd lst] is the appropriate [command] from the the user input.
    Raises [Malformed] if the input is malformed. *) 
let read_cmd lst =  
  failwith("unimplemented")



(** [parse] SPECS *)
let parse str =
  let split = String.split_on_char ' ' str in 
  let lst = List.rev (parse_rec split []) in 
  if lst = [] then raise Empty 
  else read_cmd lst 