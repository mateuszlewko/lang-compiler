open Core
open Ocaml_parsing
open Ast

let () = begin
  (* enable pretty error messages *)
  Parser.pp_exceptions ();
  
  printf "Tokens:\n";
  (* List.iter Lexer.all_of_token ~f:(fun t ->
    Lexer.show_token t
    |> printf "  %s\n"); *)

  let s1 = "
let fn = a 

let fn a b =
  fn a
  fn a b
  b


let a = 4" in
  
  let s2 = 
"
let fn a b = a
  fn
  2
  a

" 
  in 

  let s3 = 
" 
let fn a b =
    fn a b
    bbbb
    aa
    
    3
    fn 2

" 
  in 

  let s4 = 
" 
let fn a b =
    fn a b
    bbbb
    aa

    3
    fn 2
      3
" 
  in 
  (* let s2 = "(()" in *)
  (* let s3 = "( () () ) ) ()" in *)
  List.iter [s2; s3; s4] ~f:(fun s ->
    printf "\nTrying to parse \"%s\".\n" s;
    printf "res: %s\n" (Parser.ast_of_string s |> show_expr);
    printf "-> success!\n";
  );
  ()
end
