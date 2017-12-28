%{
(* OCaml preamble *)
open Core

%}

(* tokens *)
%token LPAR RPAR EOF LAMBDA

%start <string> ast_eof

%%

ast_eof:
  | a = ast; EOF { a }
  ;

ast:
  | LAMBDA { "λ" }
  | l = list(nested) { String.concat l }

nested:
  | LPAR; a = ast; RPAR { sprintf "(%s)" a }
  ;

%%
