open Lang_compiler
open Core
open Llvm
open Core
open BatPervasives
open Ast
open Codegen

let gen_llvm_exn prog =
  try
    let llval, env = gen_prog prog in
    string_of_llmodule env.llmod
  with Failure msg -> begin
    printf "Compilation failed. \nFailure:\n\t%s\n" msg;
    flush_all ();
    exit 0 |> ignore;
    ""
    end
  | e ->
    printf "Compilation failed with unknown error!";
    raise e

let llvm_out_only = ref false
let output_path = ref "a.out"
let input_path = ref ""
let usage = "Usage: " ^ Sys.argv.(0) ^ " <source file> [-o <output file> ] [--ll-only]"

let specs = [
    ("--ll-only", Arg.Set llvm_out_only
                , " Save only generated llvm code in out.ll, without \
                    building binary");
    ("-o"       , Arg.Set_string output_path
                , "<file> Place the output binary into <file>");
  ] |> Arg.align

let compile file_name src =
  let (Prog prog) = Parser.prog_of_string (src ^ "\n") file_name in
  let ll_code = gen_llvm_exn prog in
  let save_llvm file_name =
    BatFile.with_file_out file_name (flip BatInnerIO.write_string ll_code) in

  if !llvm_out_only
  then begin
    save_llvm "out.ll";
    printf "Saved generated LLVM IR code to out.ll successfully.\n"
  end
  else begin
    let do_cmd cmd =
      printf "%s\n" cmd;
      Sys.command_exn cmd |> ignore in

    let tmp_ll_file = sprintf ".langc_build_temp_%f.ll" (Unix.time ()) in
    let tmp_s_file = tmp_ll_file ^ ".s" in
    save_llvm tmp_ll_file;
    sprintf "llc \"%s\" -o \"%s\" -relocation-model=pic -O 3" tmp_ll_file
      tmp_s_file |> do_cmd;
    sprintf "gcc \"%s\" external.c -o \"%s\"  -O3" tmp_s_file !output_path
    |> do_cmd;
    Sys.remove tmp_ll_file;
    Sys.remove tmp_s_file;

    printf "Saved compiled binary to %s.\n" !output_path
  end

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
  then
    printf "No input files. Provide an input file: langc example.ll.\n%s\n"
      usage
  else
    BatFile.with_file_in !input_path
      (BatInnerIO.read_all %> compile !input_path);

  flush_all ();