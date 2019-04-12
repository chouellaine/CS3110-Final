open Adventure 
open State
open Command

(** [helper_string str] prints [str] with a new line. *)
let helper_string str =  
  print_endline (String.concat "" (str :: "\n" :: []))

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ ", ") t'
    in loop 0 "" lst
  in pp_elts lst

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = s 

let check_win_condition adv = 
  let end_items = (items adv adv.end_room) in 
  let item_name_list = 
    List.filter (fun i -> i.i_type = (get_desired_item_type adv)) end_items in 
  List.length item_name_list = List.length adv.items_needed_to_win


(**[modify_rm_lst room_id new_item_list room_list] is a modified 
   [room_list] with the room specified by [room_id] replaced with the same room 
   but given a modified item list [new_item_list] *)
let modify_rm_lst room_id new_item_list room_list = 
  let rec helper r_id i_list acc = function 
    | [] -> acc
    | h::t -> if get_room_id h = r_id 
      then helper r_id i_list ({h with items = i_list}::acc) t
      else helper r_id i_list (h::acc) t
  in 
  helper room_id new_item_list [] room_list

(** Helper that unlocks exit and allows player through exit  *)


(** [helper_play a s] plays the adventure [a] and performs actions
    based on user input in the context of the state [s].
    [helper_play] does the following:

    -Prints the description of the room, then prompts the user

    -If the user attempts to move illegally, the state does not
      change and the user is prompted again

    -If the user issues a legal go command, the user will exit
      to the corresponding room, and the state is updated

    -If the user issues the quit command, the process terminates
      and the game ends

    -If the user issues the score command, prints current score

    -If the user issues a command that cannot be understood,
      the user is prompted again 
*)
let rec helper_play a s = 
  s |> current_room_id |> description a |> helper_string;
  let item  = s |> current_room_id |> item_names a in 
  if item <> [] then print_string "Items in this room are: ";
  item |> pp_list pp_string |> helper_string; 
  print_string  "> ";
  match parse (read_line ()) with 
  | exception Empty -> helper_play a s  
  | Quit -> helper_string "Game Oo-Vah"; Pervasives.exit 0
  | exception Malformed-> helper_string "Invalid Command"; helper_play a s 
  | Score -> s |>  get_score |> string_of_int |> helper_string; helper_play a s
  | Inventory -> s |> get_inventory_item_names |>  pp_list pp_string |> helper_string; 
    helper_play a s
  | Go cmd -> let r = go (String.concat " " cmd) a s in 
    begin
      match r with 
      | Legal t -> helper_play a t
      | Illegal -> helper_string "Invalid action"; helper_play a s 
      | Locked -> helper_string "It's locked"; helper_play a s
    end
  | Take cmd -> let item_name = (String.concat " " cmd) in 
    let r = take item_name a s in
    begin
      match r with 
      | Legal t -> let curr_room_id = current_room_id t in 
        let items_in_room = (get_room curr_room_id a.rooms).items in 
        let the_item = get_item item_name items_in_room in 
        let n_i_lst = (remove_item the_item [] items_in_room) in
        let new_a = {a with rooms = modify_rm_lst curr_room_id n_i_lst a.rooms} in
        helper_play new_a (set_score t (update_score new_a t))
      | Illegal -> helper_string "Invalid action"; helper_play a s
      | _ -> (helper_string "there's bug. Ask the devs to fix it"; 
              Pervasives.exit 0 )
    end
  | Drop cmd -> let item_name = (String.concat " " cmd) in 
    let r = drop item_name a s in
    let items_in_inventory = (get_inventory_items s) in 
    begin
      match r with 
      | Legal t -> let curr_room_id = current_room_id t in 
        let items_in_room = (get_room curr_room_id a.rooms).items in
        let the_item = get_item item_name items_in_inventory in 
        let n_i_lst = (the_item::items_in_room) in  
        let new_a = {a with rooms = modify_rm_lst curr_room_id n_i_lst a.rooms} in
        if (check_win_condition new_a) then 
          (helper_string "Yay you win!"; Pervasives.exit 0 )
        else helper_play new_a (set_score t (update_score new_a t))
      | Illegal -> helper_string "Invalid action."; helper_play a s
      | _ -> (helper_string "there's bug. Ask the devs to fix it"; 
              Pervasives.exit 0 )
    end
  | Lock cmd -> let r = lock (String.concat " " cmd) a s in
    begin
      match r with
      | Legal t -> helper_string "Click. It locked"; helper_play a t
      | Illegal -> helper_string "Invalid action"; helper_play a s
      | _ -> (helper_string "there's bug. Ask the devs to fix it"; 
              Pervasives.exit 0 )
    end
  | Unlock cmd -> let r = unlock (String.concat " " cmd) a s in
    begin
      match r with
      | Legal t -> helper_string "Click. It unlocked";helper_play a t
      | Illegal -> helper_string "Invalid action"; helper_play a s
      | _ -> (helper_string "there's bug. Ask the devs to fix it"; 
              Pervasives.exit 0 )
    end

(** [helper_init a] initializes the initial state of a new game with 
    Adventure [a]. *)
let helper_init a =  
  a |> init_state |> helper_play a

(** [helper_start ()] asks user for a file and plays a game. 
    Prompts for try again if file input is invalid. *)
let rec helper_start () = 
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> 
    match Yojson.Basic.from_file file_name with 
    | j -> helper_init (from_json j)
    | exception _ -> helper_string "File Error, try again"; 
      print_string "> "; helper_start () 

(** [main ()] prints the prompt for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  helper_start ()

(* Execute the game engine. *)
let () = main ()
