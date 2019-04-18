open State

let minimax st depth = 
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

let pruned_minimax st depth = 
  let rec pmm_helper s d alpha beta = 
    if d = 0 || (List.length (get_all_moves s) = 0) then get_eval s
    else if s.turn mod 2 = 1 then 
      let max_val = ref neg_infinity in 
      let rec update_max_vals mv_lst = 
        match mv_lst with 
        | [] -> () 
        | h::t -> 
          max_val := (max !max_val (pmm_helper (update_state s h) (d-1) alpha beta));
          alpha := (max !alpha !max_val);
          if !alpha <= !beta then (update_max_vals t) else ()
      in update_max_vals (get_all_moves s); !max_val
    else 
      let min_val = ref infinity in 
      let rec update_min_vals mv_lst = 
        match mv_lst with 
        | [] -> () 
        | h::t -> 
          min_val := (min !min_val (pmm_helper (update_state s h) (d-1) alpha beta));
          alpha := (min !alpha !min_val);
          if !alpha <= !beta then (update_min_vals t) else ()
      in update_min_vals (get_all_moves s); !min_val
  in pmm_helper st depth (ref neg_infinity) (ref infinity)

let get_sugg_mv st depth = 
  let mv_lst = snd (minimax st depth) in
  pp_move_lst mv_lst;
  List.hd mv_lst



