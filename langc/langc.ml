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

let lettest a b =
  ll_putint (10 + 20)
  let inner_adder a b =
    a + b
  inner_adder 10 10

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

let fn2 a b =
  let adder2 x y = x + y
  adder2 a b

let main () : int =
  ll_print_line ()

  (* ll_putint(lettest 100 99) *)

  ll_putint (fn2 1 2)
  ll_print_line ()
  (* add 20
    22

  ll_putint ((sub 40 20) + (sub 21 20))

  ll_print_line () *)
  0
"
  in
  let (Prog prog) = Parser.prog_of_string src in
  (* let (Expr ast) = List.nth_exn prog 0 in *)
  (* printf "Ast:\n%s\n" (show_program (Prog prog));
  flush_all (); *)

  (* printf "--- start ll ---\n"; *)
  let env, llval = gen_prog prog in
  string_of_llmodule env.llmod |> printf "%s\n";
  (* List.iter ll ~f:(string_of_llvalue %> printf "%s\n"); *)
  flush_all ();
  (* printf "\n--- end ll ---\n"; *)