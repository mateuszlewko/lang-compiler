open Lang_compiler
open Core
open Llvm
open Core
open BatPervasives
open Ast
open Codegen

let _ =
  (* enable pretty error messages *)
  Parser.pp_exceptions ();

  let src =
"
external ll_putint : int -> ()
external ll_print_line : () -> ()

let lettest a b =
  ll_putint (10 + 20)
  ll_print_line ()
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
  if (a + b) = 10
  then (a + b)
  else 50

let id x = x

let fn2 a b =
  let adder2 x y = x + y
  adder2 a b

let rec power a n =
  if n = 0
  then 1
  else a * (power a (n - 1))

let main () : int =
  ll_print_line ()

  ll_putint(lettest 100 99)
  ll_print_line ()

  ll_putint (fn2 1 2)
  ll_print_line ()

  ll_putint (adder 5 5)
  ll_print_line ()

  let res a : () =
    if (2 + 2) = (id 3)
    then (ll_putint 1)
    elif (3 + 3) = (id 6)
    then (ll_putint 2)
    elif (2 + 2) = (id 4)
    then (ll_putint 3)

  res 5

  (* ll_putint (res 0) *)
  ll_print_line ()

  (* if (2 + 2) = 4
  then (lettest 0 0)
  else (lettest 1 1)

  if (2 + 2) = 4
  then (ll_putint 1) *)

  0
" in
  let src =
"

external ll_putint : int -> ()
external ll_print_line : () -> ()

let rec power a n =
  if n = 0
  then 1
  else a * (power a (n - 1))

let main () : int =
  ll_putint(power 3 200000)
  ll_print_line ()

  0
"
  in

  let (Prog prog) = Parser.prog_of_string src in
  (* printf "Ast:\n%s\n" (show_program (Prog prog));
  flush_all (); *)

  let env, llval = gen_prog prog in
  string_of_llmodule env.llmod |> printf "%s\n";
  flush_all ();