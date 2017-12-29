(** Thin wrapper around Menhir-generated parser, providing a saner interface. *)

val ast_of_string : ?pos: Lexing.position -> string -> string
val ast_of_file : string -> string

val pp_exceptions : unit -> unit
(** Registers a pretty printer for lex and parse exceptions. This results in
    colorful error messages including the source location when errrors occur. *)
