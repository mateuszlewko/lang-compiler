open Lang_compiler
open Core
open Llvm
open Core
open BatPervasives
open Ast
open Codegen

let _ =
  printf "\n";
  (* enable pretty error messages *)
  Parser.pp_exceptions ();

  let src =
"
external ll_putint : int -> ()
external ll_print_line : () -> ()

let sub a b : int =
  ll_putint (a - b)
  ll_print_line ()
  a - b

let add a b : () =
  ll_putint (a + b)
  ll_print_line ()

let adder (a : int) b : int =
  if (a + b) = 3
  then 100
  else 50

let id x = x

let main () : int =
  ll_putint (adder 1 2)
  ll_print_line ()
  add 20 20 + 2

  ll_putint ((id 5) + 5)

  ll_putint ((sub 20 20) + (sub 20 20))

  (* ll_print_line ()*)
  0
"
  in
  let (Prog prog) = Parser.prog_of_string src in
  (* let (Expr ast) = List.nth_exn prog 0 in *)
  (* printf "Ast:\n%s\n" (show_program (Prog prog)); *)

  (* printf "--- start ll ---\n"; *)
  let ll = gen_prog prog |> snd in
  List.iter ll ~f:(string_of_llvalue %> printf "%s\n");
  flush_all ();
  (* printf "\n--- end ll ---\n"; *)