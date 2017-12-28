#include "Tokens.ml"
  [@@deriving show, enumerate]

(* use custom lexbuffer to keep track of source location *)
module Sedlexing = LexBuffer
module StdChar = Char
open LexBuffer
open Core
open BatPervasives

(** Signals a lexing error at the provided source location.  *)
exception LexError of (Lexing.position * string)

(** Signals a parsing error at the provided token and its start and end locations. *)
exception ParseError of (token * Lexing.position * Lexing.position)

(* Register exceptions for pretty printing *)
let _ =
  let open Location in
  register_error_of_exn (function
    | LexError (pos, msg) ->
      let loc = { loc_start = pos; loc_end = pos; loc_ghost = false} in
      Some { loc; msg; sub=[]; if_highlight=""; }
    | ParseError (token, loc_start, loc_end) ->
      let loc = Location.{ loc_start; loc_end; loc_ghost = false} in
      let msg =
        show_token token
        |> Printf.sprintf "parse error while reading token '%s'" in
      Some { loc; msg; sub=[]; if_highlight=""; }
    | _ -> None)


let failwith buf s = raise (LexError (buf.pos, s))

let illegal buf c =
  StdChar.escaped c
  |> Printf.sprintf "unexpected character in expression: '%s'"
  |> failwith buf

(* regular expressions  *)
let letter = [%sedlex.regexp? 'A'..'Z' | 'a'..'z']
let digit = [%sedlex.regexp? '0'..'9']
let id_init = [%sedlex.regexp? letter  | '_']
let id_cont = [%sedlex.regexp? id_init | Chars ".\'" | digit ]
let id = [%sedlex.regexp? id_init, Star id_cont ]
let hex = [%sedlex.regexp? digit | 'a'..'f' | 'A'..'F' ]
let hexnum = [%sedlex.regexp? '0', 'x', Plus hex ]
let decnum = [%sedlex.regexp? Plus digit]
let decbyte = [%sedlex.regexp? (digit,digit,digit) | (digit,digit) | digit ]
let hexbyte = [%sedlex.regexp? hex,hex ]
let blank = [%sedlex.regexp? ' ' | '\t' ]
let space = [%sedlex.regexp? ' ' ]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n" ]

type state = { new_line : bool; level : int; stack : int list }
let init_state = { new_line = false; level = 0; stack = [0] }

(* let end_of_indent state = 
  if state.level > List.hd_exn 
  then INDENT, { state with stack = state.level::state.stack }
  else if state.level < List.hd_exn 
  then 
    let rev dedents = 
      function
      | top::stack when top > stack.level 

let rec indentation state buf = 
  match%sedlex buf with 
  | space   -> indentation { state with level = state.level + 1 } buf 
  | newline -> indentation { state with level = 0 } buf 
  | _ -> token {state } *)

(* swallows whitespace and comments *)
let rec garbage buf =
  match%sedlex buf with
  | newline -> garbage buf (*indentation stack 0 buf*)
  | Plus blank -> garbage buf
  | "(*" -> comment 1 buf
  | _    -> ()

(* allow nested comments, like OCaml *)
and comment depth buf =
  if depth = 0 then garbage buf else
  match%sedlex buf with
  | eof -> failwith buf "Unterminated comment at EOF" 
  | "(*" -> comment (depth + 1) buf
  | "*)" -> comment (depth - 1) buf
  | any -> comment depth buf
  | _ -> assert false

(* returns the next token *)
let token buf =
  garbage buf;
  match%sedlex buf with
  | eof -> [EOF]

   (* parenths *)
  | '(' -> [LPAR]
  | ')' -> [RPAR]

  | _ -> illegal buf (StdChar.chr (next buf))

(* wrapper around `token` that records start and end locations *)
let loc_token buf =
  let () = garbage buf in (*  dispose of garbage before recording start location *)
  let loc_start = next_loc buf in
  let ts = token buf in
  let loc_end = next_loc buf in
  (ts, loc_start, loc_end)

(* menhir interface *)
type ('token, 'a) parser = ('token, 'a) MenhirLib.Convert.traditional

let aaaa = 3

let parse buf p =
  let pending_tokens = ref [] in
  let last_token = ref Lexing.(EOF, dummy_pos, dummy_pos) in

  let next_token () = 
    if !pending_tokens = [] 
    then 
      let (t::ts, p, q) = loc_token buf in
      pending_tokens := List.map ts ~f:(fun ti -> ti, Lexing.dummy_pos, Lexing.dummy_pos);
      last_token := t, p, q;
      (* printf "token A: %s, p: %s, q: %s\n" (show_token t) (dump p) (dump q); *)
      flush_all ();
      !last_token 
    else 
      let [t], ts = List.split_n !pending_tokens 1 in 
      pending_tokens := ts;
      (* printf "token B: %s\n" (show_token (fst3 t)); *)
      flush_all ();
      t
  in
  MenhirLib.Convert.Simplified.traditional2revised p next_token 
  (* try with
  | LexError (pos, s) -> raise (LexError (pos, s))
  | _                 -> raise (ParseError (!last_token)) *)

let parse_string ?pos s p =
  parse (LexBuffer.of_ascii_string ?pos s) p

let parse_file ~file p =
  parse (LexBuffer.of_ascii_file file) p
