open Lang_compiler
open Core
open Llvm
open Core
open BatPervasives
open Ast
open Codegen

let gen_llvm prog =
  try
    let llval, env = gen_prog prog in
    string_of_llmodule env.llmod
  with Failure msg ->
    printf "Compilation failed. \nFailure:\n\t%s" msg;
    ""
  | e ->
    printf "Compilation failed with unknown error!";
    raise e

let llvm_out_only = ref false
let output_path = ref ""
let input_path = ref ""
let usage = "Usage: " ^ Sys.argv.(0) ^ " <source file> [-o <output file> ] [--ll-only]"

let specs = [
    ("--ll-only", Arg.Set llvm_out_only
                , " Save only generated llvm code in out.ll, without \
                    building binary");
    ("-o"       , Arg.Set_string output_path
                , "<file> Place the output binary into <file>");
  ] |> Arg.align

let compile src =
  let (Prog prog) = Parser.prog_of_string (src ^ "\n") in
  let ll_code = gen_llvm prog in
  if !llvm_out_only
  then
    BatFile.with_file_out "out.ll" (flip BatInnerIO.write_string ll_code);
    printf "Saved generated LLVM IR code to out.ll successfully.\n"

let _ =
  (* parse command line arguments and display help *)
  Arg.parse specs (fun x ->
    if !input_path <> ""
    then raise (Arg.Bad ("Unknown argument: " ^ x))
    else input_path := x
  ) usage;

  Parser.pp_exceptions ();
  Llvm.enable_pretty_stacktrace ();

  BatFile.with_file_in !input_path (BatInnerIO.read_all %> compile);
  flush_all ();