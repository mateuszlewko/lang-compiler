open Core
open OUnit
open Lang_compiler
open Langc_compilation
(* open Langc_lib.Compilation *)
open BatPervasives

let binfile = ".test.tmp.e"
let stdout_file = ".test.tmp.out"

let compile file = 
  Compilation.compile_file ~log:false ("test/compiler-test-srcs/" ^ file) 
                                      binfile false

(** Create test that checks if stdout produced from compiled binary
    matches expected string. Whitespace will be stripped from both stdout 
    and expected value  *)
let create_test_of input_src_file expected_stdout = 
  fun () ->
    compile input_src_file;
    Sys.command_exn ("./" ^ binfile ^ " > " ^ stdout_file);
    let expected = String.strip expected_stdout in
    BatFile.with_file_in stdout_file
      (BatInnerIO.read_all %> String.strip %> (assert_equal expected));

    Sys.remove binfile;
    Sys.remove stdout_file
    
let tests = "compiler tests" >::: [
    "passing funcs 1" >:: 
      ("144\n104" |> create_test_of "passing_funcs1.la");
    
    "bool 1" >:: 
      ("true"     |> create_test_of "bool1.la");

    "letrec & array 1" >:: 
      ("0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n\n1\n10\n1\n\n6\n6\n\n6\n8"
       |> create_test_of "letrec_array1.la");
       
    "array 1" >:: 
      ("1\n2\n4\n0\n0\n0\n0\n0\n0\n10"
       |> create_test_of "array1.la");

    "tail rec 1" >:: 
      ("3\n8\n1650879261"
       |> create_test_of "tail_rec1.la");

    "modules 1" >:: 
      ("4\n4\n2\n3"
       |> create_test_of "modules1.la")
  ]