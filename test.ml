open OUnit2
open Command
open State

let suite =
  "test suite for checkers"  >::: List.flatten [

  ]

let _ = run_test_tt_main suite
