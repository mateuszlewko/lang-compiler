open Core

(** type a1 t1 -> a2 t2 -> ... -> an an' tn is represented as a list 
       [[a1; t1]; [[a2; t2]; ...; [an; an'; tn]] *)
type type_annot = string list list
[@@deriving show]

                   (** record name, (field, type) list *)
type record_declaration = string * (string * type_annot) list
[@@deriving show]

type type_declaration =
  | RecordType of record_declaration
  [@@deriving show]

(** Type is either Some type, or none which means it's integer *)
type typed_arg = string * type_annot option
[@@deriving show]

type expr =
  | VarExp of string
  | LitExp of literal
  | LetExp of bool * typed_arg * typed_arg list * expr option * expr list option
  | AppExp of expr * expr list * expr list option
  | InfixOp of string * expr option * expr option
          (** cond   then    elif elif-then      else *)
  | IfExp of expr * expr * (expr * expr) list * expr option
  | Exprs of expr list
  | RecordLiteral of (string * expr) list
  | FieldGetExp of expr * string
  | RecordWithExp of expr * (string * expr) list
  [@@deriving show]

and literal =
  | Int of int
  | Int8 of int
  | String of string
  | Bool of bool
  | Array of expr list
  | Unit
  [@@deriving show]

type top_level =
  | Expr of expr
  | Extern of string * type_annot
  | TypeDecl of type_declaration
  | Module of string * top_level list
  | Open of string
  [@@deriving show]

type program = Prog of top_level list
[@@deriving show]
