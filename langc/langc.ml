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
  Llvm.enable_pretty_stacktrace ();

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
external ll_get_ith_elem_of_int_array : int array -> int -> int
external ll_set_ith_elem_of_int_array : int array -> int -> int -> ()
external ll_new_int_array : int -> int array

module A =
    external ll_putint : int -> ()
    external ll_print_line : () -> ()

    let aaaaaa : () =
      ll_putint (1)
      ll_print_line ()

    let testA x = x * 2

    module B =
      let testB x y =
        x + y

module D =
    module E =
      let adder x y = x + y

    let testD x =
      E.adder 21 x

let apply (fn : int -> int) arg =
  fn arg

let mult2 x =
  x * 2

let rec power a n =
  if n = 0
  then 1
  else a * (power a (n - 1))

let fib n =
  let rec aux n a b =
    if n = 0
    then b
    else (aux (n-1) b (a+b))

  aux n 0 1

let app2 (fn : int -> int -> int) arg1 arg2 =
  fn arg1 arg2

module G =
  let testG a b =
    a + b

open G

let arr : int array =
  [| 3; 5; 10111 |]

let topval6 = 77

let topval7 = topval6 + 3

let topval8 x =
  topval7 + 7 + x


let topval = 4

(* let topval_fun : int -> int = mult2 *)

(* let topval2 =
  let top_inner_adder a b =
    a + b + 10

  ll_putint(top_inner_adder 1 3)
  ll_print_line ()
  G.testG 10 14 *)

let retfun : int -> int = mult2

let get_ith (arr : int array) ix =
  ll_get_ith_elem_of_int_array arr ix

let set_ith (arr : int array) ix val : () =
  ll_set_ith_elem_of_int_array arr ix val

let new size : int array =
  ll_new_int_array size

let my_arr : int array = (new 100)

let main () : int =
  set_ith my_arr 78 108
  ll_putint (get_ith my_arr 78)
  ll_print_line ()

  ll_putint (get_ith my_arr 79)
  ll_print_line ()

  ll_putint (get_ith arr 2)
  ll_print_line ()

  set_ith arr 2 999

  ll_putint (get_ith arr 2)
  ll_print_line ()

  ll_putint((retfun) 109)
  ll_print_line ()

  ll_putint(topval7)
  ll_print_line ()

  ll_putint(topval8 7)
  ll_print_line ()

  ll_putint(testG 2 3)
  ll_print_line ()

  ll_putint(A.testA 101)
  ll_print_line ()

  ll_putint(D.testD 35)
  ll_print_line ()

  ll_putint(fib 35)
  ll_print_line ()

  ll_putint (apply mult2 6)
  ll_print_line ()

  ll_putint(power 3 4)
  ll_print_line ()

  ll_putint(app2 power 2 10)
  ll_print_line ()

  ll_putint 104
  ll_print_line ()
  0
"
  in

  let (Prog prog) = Parser.prog_of_string src in
  (* printf "Ast:\n%s\n" (show_program (Prog prog));
  flush_all (); *)

  let llval, env = gen_prog prog in

  string_of_llmodule env.llmod |> printf "%s\n";
  flush_all ();