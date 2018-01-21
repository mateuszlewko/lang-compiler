(* *** DISCLAIMER ***
   Code in this file is based on code from:
   https://github.com/smolkaj/ocaml-parsing/blob/master/src/Lexer.cppo.sedlex.ml
   Modified by: Mateusz Lewko *)

#include "tokens.ml"
  [@@deriving show]

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
let id_cont = [%sedlex.regexp? id_init | Chars "'" | digit ]
let id_nest_cont = [%sedlex.regexp? id_init | Chars ".\'" | digit ]
let id = [%sedlex.regexp? id_init, Star id_cont ]
let id_nest = [%sedlex.regexp? id_init, Star id_nest_cont ]
let hex = [%sedlex.regexp? digit | 'a'..'f' | 'A'..'F' ]
let hexnum = [%sedlex.regexp? '0', 'x', Plus hex ]
let decnum = [%sedlex.regexp? Plus digit]
let decbyte = [%sedlex.regexp? (digit,digit,digit) | (digit,digit) | digit ]
let hexbyte = [%sedlex.regexp? hex,hex ]
let blank = [%sedlex.regexp? ' ' | '\t' ]
let space = [%sedlex.regexp? ' ' ]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n" ]
let operator = [%sedlex.regexp? Plus (Chars "!%&*+-./<=>?@^|~") ]

type state = { level : int; stack : int list }
let init_state = { level = 0; stack = [0] }

let rec end_of_indent state =
  (* printf "level: %d, stack: %s\n" state.level (dump state.stack); *)

  if state.level > List.hd_exn state.stack
  then [INDENT], { state with stack = state.level::state.stack }
  else if state.level < List.hd_exn state.stack
  then
    let rec dedents res =
      function
      | top::stack when top > state.level ->
          dedents (DEDENT::res) stack
      | stack -> res, stack
    in
    let deds, stack = dedents [] state.stack in
    deds, { state with stack = stack }
  else [], state

let rec indentation state buf =
  match%sedlex buf with
  | space   -> indentation { state with level = state.level + 1 } buf
  | newline -> indentation { state with level = 0 } buf
  | _       -> end_of_indent state

(* swallows whitespace and comments *)
and garbage buf =
  match%sedlex buf with
  | Plus blank -> garbage buf
  | "(*"       -> comment 1 buf
  | _          -> ()

(* nested comments *)
and comment depth buf =
  if depth = 0
  then garbage buf
  else match%sedlex buf with
       | eof  -> failwith buf "Unterminated comment at EOF"
       | "(*" -> comment (depth + 1) buf
       | "*)" -> comment (depth - 1) buf
       | any  -> comment depth buf
       | _    -> assert false

(* returns the next token *)
and token state buf =
  garbage buf;

  match%sedlex buf with
  | eof -> [EOF], state

  (* newline and indentation *)
  | newline ->
    let toks, state = indentation { state with level = 0 } buf
    in NEWLINE::toks, state

  | "()" -> [UNIT], state

  | '(' -> [LPAR], state
  | ')' -> [RPAR], state

  | "let" -> [LET], state
  | "rec" -> [REC], state
  | "if"  -> [IF], state
  | "else"  -> [ELSE], state
  | "elif"  -> [ELIF], state
  | "then"  -> [THEN], state

  | "true" -> [BOOL true], state
  | "false" -> [BOOL true], state

  | "external" -> [EXTERNAL], state
  | "module" -> [MODULE], state
  | "open" -> [OPEN], state

  | '"'  -> [QUOTE], state
  | "->" -> [ARROW], state
  | ';'  -> [SEMICOL], state
  | ':'  -> [COLON], state
  | ','  -> [COMMA], state
  | '='  -> [EQ], state
  | "[|"  -> [ARRAY_OPEN], state
  | "|]"  -> [ARRAY_CLOSE], state
  | '['  -> [LBRACKET], state
  | ']'  -> [RBRACKET], state

  | operator -> [OPERATOR (ascii buf)], state

  | decnum -> [INT (ascii buf |> int_of_string)], state
  | id -> [SYMBOL (ascii buf)], state
  | id_nest -> [NESTED_SYMBOL (ascii buf)], state

  | any -> [KWD (ascii buf |> flip String.get 0 )], state
  | _ -> illegal buf (StdChar.chr (next buf))

(* wrapper around `token` that records start and end locations *)
let loc_token state buf =
  let () = garbage buf in (*  dispose of garbage before recording start location *)
  let loc_start = next_loc buf in
  let ts, state = token state buf in
  let loc_end   = next_loc buf in
  state, (ts, loc_start, loc_end)

(* menhir interface *)
type ('token, 'a) parser = ('token, 'a) MenhirLib.Convert.traditional

let parse buf p =
  let pending_tokens = ref [] in
  let last_token = ref Lexing.(NEWLINE, dummy_pos, dummy_pos) in
  let last_state = ref init_state in

  let next_token () =
    if !pending_tokens = []
    then
      let state, (t::ts, p, q) = loc_token !last_state buf in
      pending_tokens := List.map ts ~f:(fun ti -> ti, p, q);

      last_token := t, p, q;
      last_state := state;

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
  try MenhirLib.Convert.Simplified.traditional2revised p next_token with
  | LexError (pos, s) -> raise (LexError (pos, s))
  | _                 -> raise (ParseError (!last_token))

let parse_string ?pos s p =
  parse (LexBuffer.of_ascii_string ?pos s) p

let parse_file ~file p =
  parse (LexBuffer.of_ascii_file file) p
