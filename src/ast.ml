open Core

type literal = Int of int | String of string | Bool of bool
[@@deriving show]

type expr = 
  | VarExp of string
  | LitExp of literal
  | LetExp of string * string list * expr option * expr list option
  | AppExp of expr * expr list * expr list option
  | InfixOp of string * expr option * expr option
  | IfExp of expr * expr * expr option (* TODO: *)
  [@@deriving show]

type program = Prog of (expr list)
[@@deriving show]

(* type toplevel = 
  | Expr of expr
  | InfixSet    (* TODO: *)
  | ModuleDecl  (* TODO: *)
  | TypeDecl    (* TODO: *)
  [@@deriving show] *)