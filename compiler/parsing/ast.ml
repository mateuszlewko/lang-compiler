open Core

(** type a1 t1 -> a2 t2 -> ... -> an an' tn is represented as a list
       [[a1; t1]; [[a2; t2]; ...; [an; an'; tn]] *)

type basic_type = Single of string list | Fun of basic_type list
[@@deriving show]

type type_annotation =
  { basic   : basic_type
  ; classes : (basic_type * string list) list
  } [@@deriving show]

                   (** record name, (field, type) list *)
type record_declaration = string * (string * type_annotation) list
[@@deriving show]

type type_declaration =
  | RecordType of record_declaration
  [@@deriving show]

and class_declaration =
  { name           : string
  ; type_name      : string
  ; parent_classes : string list
  ; declarations   : (string * type_annotation) list
  } [@@deriving show]

(** Type is either Some type, or none which means it's integer *)
type typed_arg = string * type_annotation option
[@@deriving show]

type expr =
  | VarExp  of string
  | LitExp  of literal
  | LetExp  of letexp
  | AppExp  of expr * expr list
  (* TODO: remove option from infix op *)
  | InfixOp of string * expr option * expr option
  | IfExp         of ifexp
  | Exprs         of expr list
  | RecordLiteral of (string * expr) list
  | FieldGetExp   of expr * string
  | RecordWithExp of expr * (string * expr) list
  [@@deriving show]

and ifexp =
  { cond  : expr
  ; then_ : expr
  ; elif  : expr * expr list
  ; else_ : expr option
  } [@@deriving show]

and letexp =
  { name  : string
  ; args  : typed_arg list
  ; ret_t : type_annotation option
  ; body  : expr list
  } [@@deriving show]

and class_instance =
  { class_name  : string
  ; type_       : type_annotation
  ; definitions : letexp list
  } [@@deriving show]

and literal =
  | Int    of int
  | Int8   of int
  | String of string
  | Bool   of bool
  | Array  of expr list
  | Unit
  [@@deriving show]

type top_level =
  | Expr     of expr
  | Extern   of string * type_annotation
  | TypeDecl of type_declaration
  | Class    of class_declaration
  | Instance of class_instance
  | Module   of string * top_level list
  | Open     of string
  [@@deriving show]

type program = Prog of top_level list
[@@deriving show]
