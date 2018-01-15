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
let adder a b =
  if (a + b) = 3
  then 100
  else 50
"
  in
  let (Prog prog) = Parser.prog_of_string src in
  let ast = List.nth_exn prog 0 in
  printf "Ast:\n%s\n" (show_expr ast);

  printf "--- start ll ---\n";
  flush_all ();
  let ll = gen_ast ast in
  dump_value ll;
  printf "\n--- end ll ---\n";