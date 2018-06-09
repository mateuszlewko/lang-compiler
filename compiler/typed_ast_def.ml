open Lang_types_def
open Core 

type arg = string * t
[@@deriving show]

type subs = (t * t) list
[@@deriving show]

type funexp = 
  { name     : string
  ; gen_name : string
  ; is_rec   : bool 
  ; args     : arg list
  ; body     : expr_t list option }

and ifexp = 
  { cond      : expr_t 
  ; then_body : expr_t
  ; else_body : expr_t }

and gep_store = 
  { src  : expr_t 
  ; dest : expr_t
  ; idx  : int list 
  }

and literal =
  | Int    of int
  | String of string
  | Bool   of bool
  | Array  of expr_t list
  | Unit

and body_expr = 
  | Var        of string 
  | SetVar     of string * expr_t
  | Lit        of literal
  | Value      of string * expr_t
  | App        of expr_t * expr_t list 
  | InfixOp    of string * expr_t * expr_t 
  | If         of ifexp 
  | GepLoad    of expr_t * int list
  | Clone      of expr_t
  | GepStore   of gep_store
  | RecordLit  of expr_t list
  | Exprs      of expr_t list
  | Substitute of subs * expr_t
  [@@deriving show]

and expr_t = body_expr * t
[@@deriving show]

type declaration = { name : string; gen_name : string }
[@@deriving show]

(* type fun_decl = { name : string; gen_name : string; t : t }
[@@deriving show] *)

type top = 
  | Expr     of expr_t
  | Fun      of funexp * t
  | Class    of string * string * string list
  | Instance of string * t * (funexp * t) list
  | Extern   of declaration * t
  | Module   of string * top list
  | Open     of string
  | FunDecl  of declaration * t
  [@@deriving show]

type location = AtLevel of int | Global 
[@@deriving show]
type bound = t * location
[@@deriving show]
type bbb = bound * string * subs
[@@deriving show]

type fun_arg_type = Generic of string | Concrete of t

type key = 
  | Type       of string 
  | Val        of string 
  | Fields     of (string * t) BatSet.t
  
type bindings_map = (key, bound * string * subs) BatMap.t

type environment = { 
  (** symbols accessible with prefix *)
    prefixed      : bindings_map
  (** symbols available without any prefix  *)
  ; opened        : bindings_map
  (** current scope (module) prefix *)
  ; last_var      : int
  ; substitutions : (t, t list) BatMap.t
  ; prefix        : string
  ; free_vars     : (int, string * t) BatMultiMap.t
  ; level         : int 
  ; extra_fun     : (funexp * t) list 
  } 

(** Evaluates name in current scope *)
let name_in env = (^) env.prefix

(** Creates top-level env *)
let empty = { prefixed      = BatMap.empty
            ; opened        = BatMap.empty
            ; prefix        = "." 
            ; free_vars     = BatMultiMap.empty
            ; level         = 0 
            ; extra_fun     = []
            ; last_var      = 0
            ; substitutions = BatMap.empty }

let fresh_type env =
  let new_env = { env with last_var = env.last_var + 1 } in
  new_env, Lang_types_def.Generic (sprintf "'a%d" env.last_var)
