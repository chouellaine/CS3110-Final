(* Note: You may introduce new code anywhere in this file. *) 
open Yojson.Basic.Util

type room_id = string
type key_name = string
type exit_name = string
type item_name = string
type locked = bool 
exception UnknownRoom of room_id
exception UnknownExit of exit_name
exception UnknownItem of item_name

type item = {i_name: item_name; i_points: int; i_type: string}

type exit = {name: exit_name; room_id: room_id; key_name: key_name; locked: locked;}

type room = {id: room_id; description: string; exits: exit list;
             points: int; items: item list}

type t = {rooms: room list; 
          start_room: room_id; 
          end_room: room_id;
          items_needed_to_win: item_name list;
          desired_item_type: string;
         }

(** [set_from_list lst] is [lst] with all duplicates removed. *)
let set_from_list lst =
  List.fold_left (fun acc el -> if (List.mem el acc) then acc else el :: acc) [] lst

let get_desired_item_type t = 
  t.desired_item_type

(** [exit_of_json exit] is the exit that [exit] represents. 
    Requires: [exit] is a valid JSON exit representation. *)
let exit_of_json exit = 
  let key = exit|> member "key" |> to_string in 
  {name = exit |> member "name" |> to_string;
   room_id = exit |> member "room id" |> to_string;
   key_name = key;
   locked = key <> ""}


(** [item_of_json item] is the item that [item] represents. 
    Requires: [item] is a valid JSON exit representation. *)
let item_of_json item = 
  {i_name = item |> member "name" |> to_string;
   i_points = item |> member "points" |> to_int;
   i_type = item |> member "type" |> to_string;}

(** [room_of_json room] is the room that [room] represents. 
    Requires: [room] is a valid JSON room representation. *)
let room_of_json room = 
  {id = room |> member "id" |> to_string;
   description = room |> member "description" |> to_string;
   exits = room |> member "exits" |> to_list |> List.map exit_of_json;
   points = room |> member "points" |> to_int; 
   items = room |> member "items" |> to_list |> List.map item_of_json}

let get_room_id room =
  room.id

let from_json json = 
  let first_rec = {
    rooms = json |> member "rooms" |> to_list |> List.map room_of_json; 
    start_room  = json |> member "start room" |> to_string;
    end_room = json |> member "end room" |> to_string;
    desired_item_type = json |> member "desired item type" |> to_string;
    items_needed_to_win = []
  } in 
  { first_rec with 
    items_needed_to_win = 
      let i_lst = 
        (List.fold_left (fun acc r -> acc @ r.items) [] first_rec.rooms) in 
      let desired_i_lst = 
        (List.filter (fun i -> i.i_type = first_rec.desired_item_type) i_lst) in 
      List.rev_map (fun i -> i.i_name) desired_i_lst
  }

let start_room adv =
  adv.start_room

let end_room adv =
  adv.end_room

let room_ids adv = 
  set_from_list (List.map (fun r -> r.id) adv.rooms)

let rec get_room id = function
  | [] -> raise (UnknownRoom id)
  | h :: t when h.id = id -> h
  | h :: t -> (get_room id t) 

let rec get_exit name = function
  | [] -> raise (UnknownExit name)
  | h :: t when h.name = name -> h
  | h :: t -> (get_exit name t) 

let rec get_item name = function
  | [] -> raise (UnknownItem name)
  | h :: t when h.i_name = name -> h
  | h :: t -> (get_item name t) 

let room_score adv room =
  (get_room room adv.rooms).points

let description adv room =
  (get_room room adv.rooms).description

let exits_name adv room = 
  set_from_list (List.map (fun e -> e.name) (get_room room adv.rooms).exits)

let exits adv room = 
  (get_room room adv.rooms).exits

let items adv room = 
  (get_room room adv.rooms).items

let item_names adv room = 
  (List.map (fun i -> i.i_name) (get_room room adv.rooms).items)

let next_room adv room ex =
  let r = (get_room room adv.rooms) in 
  (get_exit ex r.exits).room_id

let next_rooms adv room =
  let r = (get_room room adv.rooms) in 
  set_from_list (List.map (fun e -> e.room_id) r.exits)

let rec get_exit ex_name = function
  | [] -> raise (UnknownExit ex_name)
  | h :: t when h.name = ex_name -> h
  | h :: t -> (get_exit ex_name t) 

let get_key (ex_name:exit_name) (exits:exit list) = 
  let ex = get_exit ex_name exits in ex.key_name 

let get_locked (ex_name:exit_name) (exits:exit list) = 
  let ex = get_exit ex_name exits in ex.locked 

let exit_room_id (ex_name: exit_name) (exits:exit list)= 
  (get_exit ex_name exits).room_id
