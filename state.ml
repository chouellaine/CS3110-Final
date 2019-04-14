type color = 
  | Black
  | Red

type piece = 
  | P of (color * int * int) 
  | K of (color * int * int) 

type t = {
  pieces: piece list;
  turn: int; 
}

(** The type representing the result of an attempted move. *)
type result = Legal of t | Illegal


let new_game () = 
  {
    pieces = [P (Red,1,1);P (Red,3,1);P (Red,5,1);P (Red,7,1);P (Red,2,2);
              P (Red,4,2);P (Red,6,2);P (Red,8,2);P (Red,1,3);P (Red,3,3);
              P (Red,5,3);P (Red,7,3);P (Red,2,8);P (Black,4,8);P (Black,6,8);
              P (Black,8,8);P (Black,1,7);P (Black,3,7);P (Black,5,7);
              P (Black,7,7);P (Black,2,6);P (Black,4,6);P (Black,6,6);
              P (Black,8,6)];
    turn = 1; 
  }

(** A move is valid if:
    - check if desired destination is empty 
    - if not a king, check if piece is moving diagonally (left/right) forward one spot 
       AND if there are no available forward jumps to make 
    - if king, check if piece is moving diagonally (left/right) forward/backward one spot
       AND if there are no availble forward/backward jumps to make 
    - check if player jumps over all possible pieces 
    - "move" command can also be interpreted as a "jump" command, but 
       "jump" command MUST only be jumping over opponent's pieces. *)
let get_moves = 
  failwith("unimplemented")

(** [set_score st points] gets the current number of black pieces minus the 
    current number of red pieces. *)
let get_score st = 
  let rec helper acc = function
    | [] -> acc
    | (color,_,_)::t -> 
      if color = Black then helper (acc + 1) t else  helper (acc - 1) t
  in helper 0 st.pieces

(** [piece_lst_helper st mv] is the tuple of the list of piece coordinates to be 
    removed after performing move [mv] on state [st] with list of piece 
    coordinates to be removed [acc] and the last coordinate in [mv].

    Requires: [mv] is a valid move. *)
let rec piece_lst_helper mv acc = 
  let first_coords = List.hd mv in
  match mv with 
  | [] 
  | h :: [] -> (first_coords::acc, h)
  | (x1,y1) :: (x2,y2) :: t -> 
    if abs (y2-y1) = 2 
      then piece_lst_helper (x2,y2)::t ((x2+x1)/2, (y1+y2)/2)::acc 
    else piece_lst_helper (x2,y2)::t acc 


(** [update_piece_list p_lst mv] is the new piece list after performing move [mv] 
    with piece list [piece_lst]. 
    Requires: [mv] is a valid move. *))
let update_piece_list st piece_lst mv = 
  let (remove, final) = piece_lst_helper mv []
  in 
  if mod st.turn 2 = 0 then R final else B final

  in new_piece :: updated_list

(** TODO 
    [move st mv] is the result of attempting to make the move(s) specified by [mv]
    If the move is legal, then the result is [Legal st'] where [st'] is the 
    new state after taking the move [mv] in the state [st]. Otherwise, the 
    result is [Illegal] 
    Other functionalities: 
    - check if piece at can be crowned. *)
let move st mv = 
  if List.mem mv (get_moves st) then 
    let st' = {pieces = update_piece_list st.pieces mv; turn = st.turn + 1}
    in Legal st'
  else Illegal 

(** ADD SPEC *)
let rec piece_at coord pieces = 
  match pieces with
  | [] -> None
  | (R c)::_ when c = coord -> Some (R c)
  | (B c)::_ when c = coord -> Some (B c)
  | (RK c)::_ when c = coord -> Some (RK c)
  | (BK c)::_ when c = coord -> Some (BK c)
  | _::t -> piece_at coord t

(*** TODO
     To discuss: shouldn't printing to terminal be handled in main instead of
     in game? 
     [print_prompt] displays the correct prompt and the available commands for 
     current game state, see command.ml for Menu Levels.
     Example: "Player 1 offered a draw, would you like to accept or reject?"
     "Player 2 played (x1,y1) to (x2,y2). It is now Player 1's turn." 
     "Both players have agreed on a draw, game over."
     "Player 1 Won!" 
     "50 move limit has been reached. Game Oh-vah", etc *)

let print_prompt = 
  failwith "unimplemented"

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
    | _, 2, Some B c
      -> ANSITerminal.print_string [Background Black] "  /";
      ANSITerminal.print_string [Background White] "    ";
      ANSITerminal.print_string [Background Black] "\\  ";
    | _, 2, Some R c
      -> ANSITerminal.print_string [Background Black;Foreground Magenta] "  /";
      ANSITerminal.print_string [Background Magenta] "    ";
      ANSITerminal.print_string [Background Black;Foreground Magenta] "\\  ";
    | _, 3, Some B c
      -> ANSITerminal.print_string [Background Black] "  ";
      ANSITerminal.print_string [Background White] "      ";
      ANSITerminal.print_string [Background Black] "  ";
    | _, 3, Some R c 
      -> ANSITerminal.print_string [Background Black] "  ";
      ANSITerminal.print_string [Background Magenta] "      ";
      ANSITerminal.print_string [Background Black] "  ";
    | _, 4, Some B c
      -> ANSITerminal.print_string [Background Black] "  \\";
      ANSITerminal.print_string [Background White] "    ";
      ANSITerminal.print_string [Background Black] "/  ";
    | _, 4, Some R c
      -> ANSITerminal.print_string [Background Black;Foreground Magenta] "  \\";
      ANSITerminal.print_string [Background Magenta] "    ";
      ANSITerminal.print_string [Background Black;Foreground Magenta] "/  ";
      (*King's Crown*)
    | _, 1, Some BK (x,y)
      -> ANSITerminal.print_string [Background Black] "  \\";
      ANSITerminal.print_string [Background Black;Underlined] "/\\/\\";
      ANSITerminal.print_string [Background Black] "/  ";
    | _, 1, Some RK (x,y) when (x+y) mod 2 = 1
      -> ANSITerminal.print_string [Background Black;Foreground Magenta] "  \\";
      ANSITerminal.print_string [Background Black;Foreground Magenta;Underlined] "/\\/\\";
      ANSITerminal.print_string [Background Black;Foreground Magenta] "/  ";
      (*Extra fun*)
    | _, 2, Some BK c
      -> ANSITerminal.print_string [Background Black] "  /";
      ANSITerminal.print_string [Background White; Foreground Red] "HIDE";
      ANSITerminal.print_string [Background Black] "\\  ";
    | _, 3, Some BK c 
      -> ANSITerminal.print_string [Background Black] "  ";
      ANSITerminal.print_string [Background White; Foreground Red] "  YO  ";
      ANSITerminal.print_string [Background Black] "  ";
    | _, 4, Some BK c 
      -> ANSITerminal.print_string [Background Black] "  \\";
      ANSITerminal.print_string [Background White; Foreground Red] "KIDS";
      ANSITerminal.print_string [Background Black] "/  ";
    | _, 2, Some RK c
      -> ANSITerminal.print_string [Background Black;Foreground Magenta] "  /";
      ANSITerminal.print_string [Background Magenta] "HIDE";
      ANSITerminal.print_string [Background Black;Foreground Magenta] "\\  ";
    | _, 3, Some RK c 
      -> ANSITerminal.print_string [Background Black] "  ";
      ANSITerminal.print_string [Background Magenta] "  YO  ";
      ANSITerminal.print_string [Background Black] "  ";
    | _, 4, Some RK c 
      -> ANSITerminal.print_string [Background Black;Foreground Magenta] "  \\";
      ANSITerminal.print_string [Background Magenta] "WIFE";
      ANSITerminal.print_string [Background Black;Foreground Magenta] "/  ";
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

