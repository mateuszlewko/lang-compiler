(** Thin wrapper around Menhir-generated parser, providing a saner interface. *)

val prog_of_string : ?pos: Lexing.position -> string -> string-> Ast.program

(** Registers a pretty printer for lex and parse exceptions. This results in
    colorful error messages including the source location when errors occur. *)
val pp_exceptions : unit -> unit