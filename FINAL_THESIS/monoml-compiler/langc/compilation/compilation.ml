open Lang_compiler
open Llvm
open Core
open BatPervasives
open Lang_parsing.Ast
open Lang_parsing
open Codegen

let gen_llvm_exn prog =
  try Codegen.gen_prog prog |> string_of_llmodule
  with 
  | Failure msg -> begin
    Logs.err (fun m -> m "Compilation failed. \nFailure:\n\t%s\n" msg);
    flush_all ();
    
    exit 0 |> ignore;
    ""
    end
  | e ->
    Logs.err (fun m -> m "Compilation failed with unknown error!");
    raise e

let gen_llvm_exn prog =
  Codegen.gen_prog prog |> string_of_llmodule
  
let llvm_out_only = ref false
let keep_temp_file = ref false 

let log_level = ref Logs.Warning

let output_path = ref "a.out"
let input_path = ref ""
let usage = "Usage: " ^ Sys.argv.(0) ^ " <source file> [-o <output file> ] [--ll-only]"

let write_str_to_file str file_path =
  let oc = Pervasives.open_out file_path in 

  fprintf oc "%s\n" str;  
  Pervasives.close_out oc         

let specs = [
    ("--ll-only", Arg.Set llvm_out_only
                , " Save only generated llvm code in out.ll, without \
                    building binary");
                    
    ("--keep-temp-file", Arg.Set keep_temp_file
                , " Keeps temp file (.langc_build_temp_*) in case of failure");
    ("-o"       , Arg.Set_string output_path
                , "<file> Place the output binary into <file>");
  ] |> Arg.align

let compile ?(log=true) file_name output_path llvm_out_only src =
  let (Prog prog) = Parser.prog_of_string (src ^ "\n") file_name in

  Logs.debug (fun m -> 
    m "ast:\n %s\n" (Lang_parsing.Ast.show_program (Prog prog)));

  let ll_code     = gen_llvm_exn prog in
  let save_llvm   = write_str_to_file ll_code in

  if llvm_out_only
  then begin
    save_llvm "out.ll";
    if log 
    then Logs.app (fun m -> 
      m "Saved generated LLVM IR code to out.ll successfully.")
  end
  else begin
    let do_cmd cmd =
      if log then Logs.info (fun m -> m "%s" cmd);
      Sys.command_exn cmd |> ignore in

    let tmp_ll_file = sprintf ".langc_build_temp_%f.ll" (Unix.time ()) in
    let tmp_s_file = tmp_ll_file ^ ".s" in
    save_llvm tmp_ll_file;

    try 
      sprintf "llc \"%s\" -o \"%s\" -relocation-model=pic -O 3" tmp_ll_file
        tmp_s_file |> do_cmd;
      sprintf "gcc \"%s\" external.c -o \"%s\"  -O3" tmp_s_file output_path
      |> do_cmd;
  
      Sys.remove tmp_ll_file;
      Sys.remove tmp_s_file;

      if log
      then Logs.app (fun m -> 
        m "Success. Saved compiled binary to %s." output_path)
    with _ ->  
      Logs.err (fun m -> m "Compilation failed.");
      if not !keep_temp_file
      then Sys.remove tmp_ll_file;
           Sys.remove tmp_s_file;
      
  end

let compile_file ?(log=true) file_path output_path llvm_out_only =
  BatFile.with_file_in file_path
    (BatInnerIO.read_all %> compile ~log:log file_path output_path llvm_out_only)
