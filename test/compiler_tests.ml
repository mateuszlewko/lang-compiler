open Core
open OUnit
open Lang_compiler
open Langc_compilation
open BatPervasives

let binfile = ".test.tmp.e"
let stdout_file = ".test.tmp.out"

let path_prefix = "test/input/" 

let replace input output =
  Str.global_replace (Str.regexp_string input) output;;

let compile file = 
  Compilation.compile_file ~log:false (path_prefix ^ file) 
                                      binfile false     

let read_all_text_stripped file_path = 
  BatFile.with_file_in file_path (BatInnerIO.read_all %> String.strip)

(** Create test that checks if stdout produced from compiled binary
    matches expected string. Whitespace will be stripped from both stdout 
    and expected value  *)
let create_assert_of input_src_file = 
  fun () ->
    let out_file = 
      path_prefix ^ replace ".la" "-expected-out.txt" input_src_file in 

    if Sys.file_exists out_file = `Yes 
    then 
      let expected = read_all_text_stripped out_file in 
      compile input_src_file;
      Sys.command_exn (sprintf "./%s > %s" binfile stdout_file);
      
      read_all_text_stripped stdout_file |> assert_equal expected;

      Sys.remove binfile;
      Sys.remove stdout_file
    else 
      let msg = 
        sprintf 
          "\nCreate %s file with expected output in order to run test %s" 
          out_file input_src_file in 
      printf "%s\n" msg; 
      todo msg 

let all_files_in_input_folder () =
  Sys.readdir path_prefix 
  |> Array.filter ~f:(fun s -> BatString.Exceptionless.find s ".la" 
                               |> Option.is_some)
  |> Array.to_list

let create_test_of input_src_file = 
  sprintf "Test output of \"%s\"" input_src_file
  >:: create_assert_of input_src_file

let tests = 
  "compiler tests" 
  >::: (List.map (all_files_in_input_folder ()) ~f:create_test_of)