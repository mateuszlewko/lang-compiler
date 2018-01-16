open Lang_compiler
open Core
open Llvm
(* open Core  *)
open BatPervasives
open Ast
open Codegen

let prompt () =
    printf "> ";
    flush_all ()

let line_stream_of_channel channel =
  Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)

let next_line () =
  try Some (input_line stdin)
  with _ -> None

let rec interactive curr_string =
  let commit = Str.regexp ".*;;" in

  match next_line () with
  | Some line when Str.string_match commit line 0 ->
      let line = BatString.replace ~str:line ~sub:";;" ~by:"" |> snd in
      let src = curr_string ^ "\n" ^ line ^ "\n" in
      (* printf "src: %s" src; *)

      let (Prog prog) = Parser.prog_of_string src in
      List.iter prog ~f:(show_expr %> printf "%s\n");

      prompt ();
      interactive ""
  | Some line ->
    interactive (curr_string ^ "\n" ^ line)
  | None -> printf "Bye.\n"

let _ =
  (* enable pretty error messages *)
  Parser.pp_exceptions ();

  printf "--- Ast pretty-printer ---
NOTE: use double semicolon (;;) at and of line, in order to commit code for parsing.\n\n";

  prompt ();
  interactive ""


