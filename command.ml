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
  | Rematch 

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
let rec parse_rec lst newlst =
  match lst with
  | [] -> newlst  
  | h :: [] when h = "" -> newlst
  | h :: t when h = "" -> parse_rec t newlst
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
  | h :: [] when String.lowercase_ascii h = "rematch" -> Rematch
  | h :: t when String.lowercase_ascii h = "rematch" -> raise Malformed 
  | _ :: _ -> raise Malformed 


let parse str =
  let split = String.split_on_char ' ' (String.trim str) in 
  let lst = List.rev (parse_rec split []) in 
  if lst = [] then raise Empty 
  else read_cmd lst