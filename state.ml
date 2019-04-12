(* Note: You may introduce new code anywhere in this file. *) 
open Adventure

type t = {current_room: room_id;
          visited: room_id list; 
          score: int; 
          inventory: item list;
          unlocked_exits: room_id list;
         }

let init_state adv =
  {current_room = start_room adv; 
   visited = [start_room adv]; 
   score = 0; 
   inventory = [];
   unlocked_exits = []}

let current_room_id st =
  st.current_room

let visited st =
  st.visited

let get_score st = 
  st.score

let set_score st points = 
  {st with score = points}

let get_inventory_items st = 
  st.inventory

let get_inventory_item_names st = 
  List.map (fun i -> i.i_name) st.inventory

(** [add_score adv new_room st] is the score of the new_room.
    If the room has been visited, the score should be 0. *)
let add_score adv new_room st = 
  if List.mem new_room (st.visited) then 
    0 
  else  
    room_score adv new_room 

let update_score adv st =
  List.fold_left (fun acc i -> i.i_points + acc) 0 (items adv adv.end_room)
  + List.fold_left (fun acc r_id -> (get_room r_id adv.rooms).points + acc) 0 (st.visited)

let rec remove_item i acc = function 
  | [] -> acc
  | h::t -> if h = i then acc @ t
    else remove_item i (h::acc) t 


type result = Legal of t | Illegal | Locked

let go ex adv st =
  try 
    let exits = st.current_room |> exits adv in 
    let ex_id = exit_room_id ex exits in 
    if not (get_locked ex exits) || 
       get_locked ex exits && List.mem ex_id (st.unlocked_exits) then 
      if List.mem ex (exits_name adv st.current_room) then
        let new_room = next_room adv st.current_room ex in
        Legal {st with current_room = new_room; 
                       visited = if (List.mem new_room st.visited) 
                         then st.visited else new_room :: st.visited;
                       score = st.score + (add_score adv new_room st);}
      else Illegal
    else Locked
  with 
  |UnknownExit ex -> Illegal 
  |__ -> Illegal


let has_key ex adv st = 
  let exits = st.current_room |> exits adv in   
  List.mem (get_key ex exits) (get_inventory_item_names st)

let unlock ex adv st = 
  try 
    let exits = st.current_room |> exits adv in  
    let ex_id = exit_room_id ex exits in 
    if List.mem ex_id st.unlocked_exits then Illegal 
    else if get_locked ex exits && has_key ex adv st then 
      Legal {st with unlocked_exits = ex_id::st.unlocked_exits}
    else Illegal
  with 
  |__-> Illegal 


let lock ex adv st = 
  try 
    let exits = st.current_room |> exits adv in  
    let ex_id = exit_room_id ex exits in 
    if not (get_locked ex exits) then Illegal 
    else if not (List.mem ex_id st.unlocked_exits) then Illegal
    else if has_key ex adv st then 
      Legal {st with unlocked_exits = (remove_item ex_id [] st.unlocked_exits)}
    else Illegal
  with 
  |__ -> Illegal 

let take item_name adv st = 
  try 
    let item_list = items adv st.current_room in 
    if List.mem item_name (item_names adv st.current_room) then 
      let item = get_item item_name item_list in 
      Legal {st with inventory = item::st.inventory} 
    else Illegal 
  with 
  | __ -> Illegal 

(**[remove_item i acc lst] removes the first occurrence item [i] from [lst] 
   and returns the new list
   Requires: [acc ]is [[]]
*)
let drop item_name adv st = 
  try 
    if List.mem item_name (get_inventory_item_names st) then 
      let item = get_item item_name st.inventory in 
      Legal {st with inventory = remove_item item [] (get_inventory_items st)}
    else 
      Illegal 
  with 
  | __ -> Illegal 