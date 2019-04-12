(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(* You are free to add more code here. *)


(** The type of room identifiers. *)
type room_id = string

(** The type of exit names. *)
type exit_name = string

(** The type of item names *)
type item_name = string

(** The type of key names *)
type key_name = string

(** The type that represents whether or not an exit is locked *)
type locked = bool

(* The type of item representing an item with a name and point value. *)
type item = {i_name: item_name; i_points: int; i_type: string}

(* The type of exit representing an exit with a name and room id.
   locked is True if exit is locked, false otherwise *)
type exit = {name: exit_name; room_id: room_id; key_name: key_name; locked: locked}

(* The type of room representing room with an id, description, and list of exits. *)
type room = {id: room_id; description: string; exits: exit list;
             points: int; items: item list}

(** The abstract type of values representing adventures. *)
type t = {rooms: room list; 
          start_room: room_id; 
          end_room: room_id; 
          items_needed_to_win: item_name list;
          desired_item_type: string}

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

(** Raised when an unknown item is encountered *)
exception UnknownItem of item_name

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.json -> t

(** [start_room a] is the identifier of the starting room in adventure 
    [a]. *)
val start_room : t -> room_id

(** [end_room a] is the identifier of the end room in adventure 
    [a]. *)
val end_room : t -> room_id

(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val room_ids : t -> room_id list

(** [description a r] is the description of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val description : t -> room_id -> string

(** [room_score adv room] is the score of room [room] in adventure [adv]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val room_score : t -> room_id -> int

(** [get_room id lst] is the room with the id equal to [id]. 
    Raises [UnknownRoom id] if [id] is not a room identifier in [a]'s rooms. *)
val get_room : room_id -> room list -> room 

(** [get_exit name lst] is the exit with the name equal to [name]. 
    Raises [UnknownRoom name] if [name] is not an exit identifier in [r]'s exits. *)
val get_exit : exit_name -> exit list -> exit

(** [get_room_id] is the id of the [room]*)
val get_room_id : room -> room_id

(** [get_item name lst] is the item with the name equal to [name]. 
    Raises [UnknownItem name] if [name] is not an item identifier in [r]'s items. *)
val get_item : item_name -> item list -> item

(** [exits_name a r] is a set-like list of all exit names from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits_name: t -> room_id -> exit_name list 

(** [exits a r] is a set-like list of all exit from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits : t -> room_id -> exit list

(** [items adv room] returns a list of the items in a room *)
val items : t -> room_id -> item list

(** [item_names adv room] returns a list of the item's names in a room *)
val item_names : t -> room_id -> item_name list

(** [get_desired_item_type] is the desired item type to win. *)
val get_desired_item_type : t -> string

(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a].  
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val next_room : t -> room_id -> exit_name -> room_id

(** [next_rooms a r] is a set-like list of all rooms to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].*)
val next_rooms : t -> room_id -> room_id list

(** [get_key ex] is the name of the key required to unlock [exit] *)
val get_key : exit_name -> exit list -> key_name

(** [get_locked] is returns true if [exit] is locked and false if
    [exit] is not locked*)
val get_locked : exit_name -> exit list -> locked

(** [exit_room_id] is the room id of exit [ex_name]. 
    Raises UnknownExit if exit [ex_name] does not exist.*)
val exit_room_id : exit_name -> exit list -> room_id 