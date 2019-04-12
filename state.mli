(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing the game state. *)
(** The abstract type of values representing adventures. *)
type t = {current_room: Adventure.room_id;
          visited: Adventure.room_id list; 
          score: int; 
          inventory: Adventure.item list;
          unlocked_exits: Adventure.room_id list;
         }


(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Adventure.t -> t

(** [current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. *)
val current_room_id : t -> string

(** [get_score st] is the currents score of the state. *)
val get_score : t -> int

(** [set_score st points] changes the state's score to [points]. *)
val set_score : t -> int -> t

(** [update_score adv st] is the new score based on the items and rooms visited. *)
val update_score : Adventure.t -> t -> int

(** [visited st] is a set-like list of the room identifiers the adventurer has 
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm]. *)
val visited : t -> Adventure.item_name list

(** [get_inventory_items st] is the inventory containing a list of items. *)
val get_inventory_items : t -> Adventure.item list

(** [get_inventory_item_names st] is the list of names of items in the inventory. *)
val get_inventory_item_names : t -> Adventure.item_name list

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal | Locked

(** [remove_item i acc] is the list of items with item [i] removed. *)
val remove_item : 'a -> 'a list -> 'a list -> 'a list

(** [go exit adv st] is [r] if attempting to go through exit [exit] in state 
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the 
    adventurer's current room, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the room to which [exit] leads.  
    [r] is [Locked] if [exit] is locked. In all other cases, [r] is [Illegal]
    Effects: none.  [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result

(** [has_key ex adv st] checks if the player has the key for exit [ex] in 
    adventure [adv]*)
val has_key : Adventure.exit_name -> Adventure.t -> t -> bool

(** [unlock ex adv st] unlocks exit [ex] in 
    adventure [adv]. [r] is [Illegal] under the following conditions: 
    -  player doesn't have the key
    - exit [ex] doesn't require unlocking 

      and [Legal] otherwise.
      Effects: Change game state [st] to reflect action *)
val unlock : Adventure.exit_name -> Adventure.t -> t -> result

(** [lock ex adv st] unlocks exit [ex] in 
    adventure [adv]. [r] is [Illegal] under the following conditions: 
    -  player doesn't have the key
    - exit [ex] doesn't require locking 

      and [Legal] otherwise. 
      Effects: Change game state [st] to reflect action*)
val lock : Adventure.exit_name -> Adventure.t -> t -> result

(** [take item_name adv st] is [r] if attempting to take an item in the current 
    room in [st] and adventure [adv] results in [r].  If [item_name] is an item 
    in the adventurer's current room, then [r] is [Legal st'], where in [st'] the 
    adventurer's inventory now has that item.  Otherwise, 
    the result is [Illegal]. 
    Effects: none. *)
val take : Adventure.item_name -> Adventure.t -> t -> result

(** [drop item_name adv st] is [r] if attempting to drop an item in the 
    adventurer's inventory.  If [item_name] is an item 
    in the adventurer's inventory, then [r] is [Legal st'], where in [st'] the 
    adventurer's inventory now doesn't have that item.  Otherwise, 
    the result is [Illegal]. 
    Effects: none. *)
val drop : Adventure.item_name -> Adventure.t -> t -> result


(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)