open Core

(** type a1 t1 -> a2 t2 -> ... -> an an' tn is represented as a list 
       [[a1; t1]; [[a2; t2]; ...; [an; an'; tn]] *)

type basic_type = Single of string list | Fun of basic_type list 
type type_annotation = 
  { basic   : basic_type 
  ; classes : string list 
  } [@@deriving show]

                   (** record name, (field, type) list *)
type record_declaration = string * (string * type_annot) list
[@@deriving show]

type type_declaration =
  | RecordType of record_declaration
  [@@deriving show]

type class_declaration =
  { name           : string 
  ; type_name      : string 
  ; parent_classes : string list  
  ; definitions    : (string * type_annotation) list 
  } [@@deriving show]

let class_instance = 
  { class_name : string 
  ; type_name  : string 
  ; impls      : string list  
  } 

(** Type is either Some type, or none which means it's integer *)
type typed_arg = string * type_annotation option
[@@deriving show]

type expr =
  | VarExp  of string
  | LitExp  of literal
  (* TODO: simplify LetExp *)
  | LetExp  of bool * typed_arg * typed_arg list * expr option * expr list option
  (* TODO: simplify AppExp *)
  | AppExp  of expr * expr list * expr list option
  | InfixOp of string * expr option * expr option
          (** cond   then    elif elif-then      else *)
  | IfExp         of expr * expr * (expr * expr) list * expr option
  | Exprs         of expr list
  | RecordLiteral of (string * expr) list
  | FieldGetExp   of expr * string
  | RecordWithExp of expr * (string * expr) list
  [@@deriving show]

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
