type piece = 
  | R of (int * int) 
  | B of (int * int) 
  | RK of (int * int) 
  | BK of (int * int)

type t = {
  pieces: piece list;
  turn: int; 
}

let new_game () = 
  {
    pieces = [B (2,1);B (4,1);B (6,1);B (8,1);B (1,2);B (3,2);B (5,2);B (7,2);
              B (2,3);B (4,3);B (6,3);B (8,3);R (1,6);R (3,6);R (5,6);R (7,6);
              R (2,7);R (4,7);R (6,7);R (8,7);R (1,8);R (3,8);R (5,8);R (7,8)];
    turn = 1; 
  }

(** [get_moves st] is a list of legal moves given the currrent state. *)
let get_moves = 
  failwith("unimplemented")

(** [set_score st points] gets the current number of red pieces minus the 
    current number of black pieces. *)
let get_score st = 
  let rec helper acc = function
    | [] -> acc
    | h::t -> 
      match h with 
      | RK _ | R _ -> helper (acc + 1) t
      | BK _ | B _ -> helper (acc - 1) t
  in helper 0 st.pieces


(** The type representing the result of an attempted move. *)
type result = Legal of t | Illegal

(** [move st mv] is the result of attempting to make the move specified by [mv]
    If the move is legal, then the result is [Legal st'] where [st'] is the 
    new state after taking the move [mv] in the state [st]. Otherwise, the 
    result is [Illegal]
*)
let move st mv = 
  failwith("unimplemented")

let rec piece_at coord pieces = 
  match pieces with
  | [] -> None
  | (R c)::_ when c = coord -> Some (R c)
  | (B c)::_ when c = coord -> Some (B c)
  | (RK c)::_ when c = coord -> Some (RK c)
  | (BK c)::_ when c = coord -> Some (BK c)
  | _::t -> piece_at coord t

let print_row coords subrow piece=
  begin
    match (coords,subrow,piece) with
    | (x,y),_,None | _, 1, Some R (x,y) |_, 1, Some B (x,y) 
    | _, 5, Some R (x,y) | _, 5, Some B (x,y) 
    | _, 5, Some RK (x,y) | _, 5, Some BK (x,y) when (x+y) mod 2 = 0 
      -> ANSITerminal.print_string [Background Red] "          ";
    | (x,y),_,None | _, 1, Some R (x,y) | _, 1, Some B (x,y) 
    | _, 5, Some R (x,y) | _, 5, Some B (x,y)
    | _, 5, Some RK (x,y) | _, 5, Some BK (x,y) when (x+y) mod 2 = 1 
      -> ANSITerminal.print_string [Background Black] "          ";
    | _, 2, Some B c | _, 2, Some BK c (* should never be on a red square *)
      -> ANSITerminal.print_string [Background Black] "  /";
      ANSITerminal.print_string [Background White] "    ";
      ANSITerminal.print_string [Background Black] "\\  ";
    | _, 4, Some B c | _, 4, Some BK c 
      -> ANSITerminal.print_string [Background Black] "  \\";
      ANSITerminal.print_string [Background White] "    ";
      ANSITerminal.print_string [Background Black] "/  ";
    | _, 2, Some R c | _, 2, Some RK c
      -> ANSITerminal.print_string [Background Black;Foreground Red] "  /";
      ANSITerminal.print_string [Background Red] "    ";
      ANSITerminal.print_string [Background Black;Foreground Red] "\\  ";
    | _, 4, Some R c | _, 4, Some RK c
      -> ANSITerminal.print_string [Background Black;Foreground Red] "  \\";
      ANSITerminal.print_string [Background Red] "    ";
      ANSITerminal.print_string [Background Black;Foreground Red] "/  ";
    | _, 3, Some B c | _, 3, Some BK c 
      -> ANSITerminal.print_string [Background Black] "  ";
      ANSITerminal.print_string [Background White] "      ";
      ANSITerminal.print_string [Background Black] "  ";
    | _, 3, Some R c | _, 3, Some RK c
      -> ANSITerminal.print_string [Background Black] "  ";
      ANSITerminal.print_string [Background Red] "      ";
      ANSITerminal.print_string [Background Black] "  ";
      (*King's Crown*)
    | _, 1, Some BK (x,y)
      -> ANSITerminal.print_string [Background Black] "  \\";
      ANSITerminal.print_string [Background Black;Underlined] "/\\/\\";
      ANSITerminal.print_string [Background Black] "/  ";
    | _, 1, Some RK (x,y) when (x+y) mod 2 = 1
      -> ANSITerminal.print_string [Background Black;Foreground Red] "  \\";
      ANSITerminal.print_string [Background Black;Foreground Red;Underlined] "/\\/\\";
      ANSITerminal.print_string [Background Black;Foreground Red] "/  ";
    | _ -> failwith "idk"
  end



let print_board pieces = 
  for col=1 to 8 do
    for subrow=1 to 5 do
      for row=1 to 8 do
        print_row (col,row) subrow (piece_at (row,col) pieces);
      done;
      print_string "\n"
    done
  done

