open Core
open OUnit
open Lang_compiler
open Langc_lib  
open BatPervasives

let binfile = ".test.tmp.e"
let stdout_file = ".test.tmp.out"

let compile file = 
  Compilation.compile_file ~log:false ("test/compiler-test-srcs/" ^ file) 
                                      binfile false
    
let tests = "compiler tests" >::: [
    "basic" >:: (fun () -> 
      compile "in1.la";
      Sys.command_exn ("./" ^ binfile ^ " > " ^ stdout_file);
      let expected = "144\
                    \n104" |> String.strip in
      BatFile.with_file_in stdout_file
        (BatInnerIO.read_all %> String.strip %> (assert_equal expected));

      Sys.remove binfile;
      Sys.remove stdout_file
    )
  ]