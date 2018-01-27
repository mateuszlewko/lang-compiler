open OUnit

(* let all_tests = 
  [ Parser_tests.tests
  ; Compiler_tests.tests ]
  |> List.concat *)

(* Test Runner *)
(* let _ = run_test_tt ~verbose:false all_tests  *)
let _ = 
  run_test_tt_main Parser_tests.tests   |> ignore;
  run_test_tt_main Compiler_tests.tests |> ignore