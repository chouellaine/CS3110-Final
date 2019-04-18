open State

let rec minimax st depth = 
  let rec mm_helper s d c_path = 
    if d = 0 || (List.length (get_all_moves s) = 0) then 
      ((get_eval s), List.rev c_path)
    else if s.turn mod 2 = 1 then 
      let max_eval_combiner = 
        fun acc el -> 
          let res = mm_helper (update_state s el) (d-1) (el::c_path) in
          if (fst res) >= (fst acc) then res else acc in 
      List.fold_left max_eval_combiner (neg_infinity, []) (get_all_moves s)
    else 
      let min_eval_combiner = 
        fun acc el -> 
          let res = mm_helper (update_state s el) (d-1) (el::c_path) in 
          if (fst res) <= (fst acc) then res else acc in 
      List.fold_left min_eval_combiner (infinity, []) (get_all_moves s)
  in mm_helper st depth []

let get_sugg_mv st depth = 
  let mv_lst = snd (minimax st depth) in
  pp_move_lst mv_lst;
  List.hd mv_lst



