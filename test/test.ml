open Core
open Ocaml_parsing

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
  (* let s2 = "(()" in *)
  (* let s3 = "( () () ) ) ()" in *)
  List.iter [s1 ] ~f:(fun s ->
    printf "\nTrying to parse \"%s\".\n" s;
    printf "res: %s\n" (Parser.ast_of_string s);
    printf "-> success!\n";
  );
  ()
end
