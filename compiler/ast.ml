open Core

type literal = Int of int | String of string | Bool of bool | Unit
[@@deriving show]
                     (* type name   field     type *)
type record_declaration = string * (string * string) list
[@@deriving show]

type type_declaration =
  | RecordType of record_declaration
  [@@deriving show]

  (* type t1 -> t2 -> ... -> tn is represented as list [t1; t2; ...; tn] *)
type type_annot = string list
[@@deriving show]
(* Type is either Some type, or none which means it's integer *)
type typed_var = string * type_annot option
[@@deriving show]

type expr =
  | VarExp of string
  | LitExp of literal
  | LetExp of typed_var * typed_var list * expr option * expr list option
  | AppExp of expr * expr list * expr list option
  | InfixOp of string * expr option * expr option
  | IfExp of expr * expr * expr option
  [@@deriving show]

type top_level =
  | Expr of expr
  | Extern of string * type_annot
  | TypeDecl of type_declaration
  [@@deriving show]

type program = Prog of top_level list
[@@deriving show]
