(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Go of object_phrase
  | Quit
  | Score
  | Take of object_phrase
  | Drop of object_phrase
  | Inventory
  | Lock of object_phrase
  | Unlock of object_phrase

exception Empty

exception Malformed

(** [parse_rec] is the [lst] with all empty strings removed. *)
let rec parse_rec lst newlst = 
  match lst with
  | [] -> newlst  
  | h :: t when h = "" ->  parse_rec t newlst
  | h :: t -> parse_rec t (h :: newlst) 

(** [read_cmd lst] is the appropriate [command] from the the user input.
    Raises [Malformed] if the input is malformed. *) 
let read_cmd = function 
  | [] -> raise Malformed 
  | h :: [] when h = "quit" -> Quit
  | h :: [] when h = "score" -> Score
  | h :: [] when h = "inventory" -> Inventory
  | h :: [] when h = "go" -> raise Malformed
  | h :: [] when h = "take" -> raise Malformed
  | h :: [] when h = "drop" -> raise Malformed
  | h :: [] when h = "lock" -> raise Malformed
  | h :: [] when h = "unlock" -> raise Malformed
  | h :: t  when h = "score" -> raise Malformed 
  | h :: t  when h = "quit" -> raise Malformed 
  | h :: t  when h = "go" -> Go t
  | h :: t  when h = "take" -> Take t
  | h :: t  when h = "drop" -> Drop t
  | h :: t  when h = "lock" -> Lock t
  | h :: t  when h = "unlock" -> Unlock t
  | _ :: _ -> raise Malformed 

let parse str =
  let split = String.split_on_char ' ' str in 
  let lst = List.rev (parse_rec split []) in 
  if lst = [] then raise Empty 
  else read_cmd lst 