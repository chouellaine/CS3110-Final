open OUnit2
open Command
open State

let make_parse_test 
    (name : string) 
    (str : string) 
    (expected_output : Command.command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str))

let command_tests =
  [
    "empty string" >:: (fun _ ->
        assert_raises (Empty) (fun () -> parse ""));
    "spaces" >:: (fun _ ->
        assert_raises (Empty) (fun () -> parse "  "));
    "single word" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move"));
    "lack of space" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "movea3tob5"));
    "to malformed 1" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move to a3 b5"));
    "to malformed 2" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a3 a5 to"));
    "to malformed 3" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a3 a5"));
    "to_malformed 4" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a3 to"));
    "to_malformed 4" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a9 to b3"));
    "to_malformed 4" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move i1 to b3"));
    "move multi end to" >:: (fun _ ->
        assert_raises (Malformed) (fun () -> parse "move a1 to b3 to c2 to"));
    make_parse_test "move 1" "move a1 to b3" (Move [(1,1);(2,3)]);
    make_parse_test "move extra spaces" "  move  a1  to   b3  " (Move [(1,1);(2,3)]);
    make_parse_test "move multi" "move a1 to b2 to c3" (Move [(1,1);(2,2);(3,3)]);
  ]

let suite =
  "test suite for checkers"  >::: List.flatten [
    command_tests
  ]

let _ = run_test_tt_main suite
