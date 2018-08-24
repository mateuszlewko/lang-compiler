open Core
open Lang_compiler
open Lang_parsing
open Langc_compilation.Compilation

let _ =
  (* parse command line arguments and display help *)
  Arg.parse specs (fun x ->
    if !input_path <> ""
    then raise (Arg.Bad ("Unknown argument: " ^ x))
    else input_path := x
  ) usage;

  (* enable pretty exceptions in parser *)
  Parser.pp_exceptions ();
  Llvm.enable_pretty_stacktrace ();

  if !input_path = ""
  then printf "No input files. Provide an input file: langc.exe example.la\n%s\n"
         usage
  else compile_file !input_path !output_path !llvm_out_only;

  flush_all ();