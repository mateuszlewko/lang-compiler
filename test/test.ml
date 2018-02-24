open OUnit

(* Test Runner *)
let _ = 
  run_test_tt_main Parser_tests.tests   |> ignore;
  run_test_tt_main Compiler_tests.tests |> ignore