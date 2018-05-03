#include "tokens.ml"
  [@@deriving show]

type ('token, 'a) parser =
  (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'a

val parse : Lex_buffer.t -> (token,'a) parser -> 'a

val parse_string : ?pos:Lexing.position -> ?file_name:string -> string  -> (token,'a) parser -> 'a

val parse_file : file:string -> (token,'a) parser -> 'a
