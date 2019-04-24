open Yojson.Basic.Util
open Command

type color = 
  | Black (*white on ANSI *)
  | Red (*magenta on ANSI*)

type piece = 
  | P of (color * (int * int)) 
  | K of (color * (int * int)) 

type player = AI of diff | Player | Host | Client

type connection = ((Unix.file_descr*Unix.file_descr) list option) * Unix.file_descr

type t = {
  game: gtype;
  pieces: piece list;
  turn: int; 
  moves_without_capture: int; 
  opp: player; (* if opp = host, then user is client
                  elif opp = client then user is host 
                  else user is a player playing against AI *)
  connection: connection option 
}

exception UnknownMove  

let to_game x = 
  let str = String.(trim (lowercase_ascii x)) in 
  if str = "suicide" then Suicide 
  else if str = "regular" then Regular 
  else failwith "unknown game type"

let to_player x  = 
  let str = String.lowercase_ascii x in 
  if str = "easy ai" then AI Easy
  else if str = "medium ai" then AI Medium
  else if str = "hard ai" then AI Hard 
  else if str = "alphazero ai" then AI AlphaZero 
  else if str = "host" then Client 
  else if str = "client" then Host 
  else if str = "player" then Player 
  else failwith "unknown opponent type"

(** [set_from_list lst] is [lst] with all duplicates removed. *)
let set_from_list lst =
  List.fold_left (fun acc el -> if (List.mem el acc) then acc else el :: acc) [] lst

(* if p then piece type is K of (color * (int * int)) 
   else piece type is P of (color * (int * int)) *)
let to_coord color p c = 
  match c |> to_string |> convert_coord with 
  | exception Malformed -> raise UnknownMove
  | coord -> if p then K (color,coord) else P(color,coord) 

(** [room_of_json room] is the room that [room] represents. 
    Requires: [room] is a valid JSON room representation. *)
let piece_of_json color c =  
  let kings = c |> member "kings" |> to_list |>  List.map (to_coord color true) in 
  let pieces = c |> member "pieces" |> to_list |> List.map (to_coord color false) in 
  kings@pieces 

let from_json json = 
  let red = json |> member "red" |> piece_of_json Red in 
  let black = json |> member "black" |> piece_of_json Black in 
  {
    game = json|> member "game" |> to_string |> to_game;
    pieces = red@black |> set_from_list;
    turn = json |> member "turn" |> to_int;
    moves_without_capture = json|>member "moves"|>to_int;
    opp = json |> member "opp" |> to_string |> to_player;
    connection = None;
  } 

let from_coord c = 
  let x = Char.chr (96 + fst c) |> Char.escaped in 
  let y = string_of_int (snd c) in 
  x^y

let add el l = 
  if l = "" then ({|"|}^(from_coord el)^{|"|}^l) else 
    ({|"|}^(from_coord el)^{|"|}^","^l)

let rec pieces_of_state rp rk bp bk = function 
  |[]-> rp,rk,bp,bk
  |K (Red,c)::t -> pieces_of_state rp (add c rk) bp bk t
  |P (Red,c)::t -> pieces_of_state (add c rp) rk bp bk t
  |K (Black,c)::t -> pieces_of_state rp rk bp (add c bk) t
  |P (Black,c)::t -> pieces_of_state rp rk (add c bp) bk t

let player_to_str = function 
  |AI Easy -> "Easy AI" 
  |AI Medium -> "Hard AI"
  |AI Hard -> "Medium AI"
  |AI AlphaZero -> "AlphaZero AI"
  |Player -> "Player"
  |Client -> "Client"
  |Host -> "Host"

let game_to_str = function 
  |Regular -> "Regular"
  |Suicide -> "Suicide"

let quote str = 
  {|"|}^str^{|"|}

let to_json t f_name = 
  let turn = t.turn |> string_of_int in 
  let opp = player_to_str t.opp |> quote in 
  let moves = t.moves_without_capture |> string_of_int  in 
  let game = game_to_str t.game |> quote in 
  let rp,rk,bp,bk = pieces_of_state "" "" "" "" t.pieces in 
  let red = "{\"pieces\":["^rp^ "],\"kings\":["^rk^"]}," in 
  let black = "{\"pieces\":["^bp^ "],\"kings\":["^bk^"]}," in 
  let txt = "{\"red\":" ^red^ 
            {|"black":|} ^ black^  
            {|"turn":|} ^turn^ 
            {|,"opp":|} ^opp^ 
            {|,"moves":|} ^moves^ 
            {|,"game":|}^game^ "}" in 
  let json = Yojson.Basic.from_string txt in
  Unix.chdir "saves";
  Yojson.Basic.to_file (f_name^".json") json;
  Unix.chdir ".."

let get_pieces t = t.pieces 

let get_turn t = t.turn 
