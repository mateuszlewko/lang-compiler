open High_ollvm

module LT = Lang_types

exception SymbolNotFound of string

module V = Ez.Value
module M = Ez.Module

type substitutions = (LT.t * LT.t) list 
[@@deriving show]

type bound        = Ez.Value.t * Lang_types.t 
type fun_binding  = { fn : bound; fns_arr : Ez.Value.t; arity : int }

type instance_key = string * LT.t * string
[@@deriving show]

type generic_fun  = { poli : t -> (LT.t, LT.t list) BatMap.t -> t * fun_binding
                    ; mono : (Ez.Type.t list, fun_binding) BatMap.t }

and binding = 
  | Fun        of fun_binding 
  | Val        of bound 
  | GlobalVar  of bound
  | GenericFun of generic_fun 
  | Class      of string * string

and bindings_map = (string, binding) BatMap.t

and environment = 
  { bindings      : bindings_map
  (** inferred type substitutions *)
  ; substitutions : (LT.t, LT.t list) BatMap.t
  (** methods of all class instances in a current scope *)
  (* TODO: Rename to instances *)
  ; classes       : (instance_key, binding) BatMap.t
  (** low-level module *)
  ; m             : M.t
  } 

and t = environment

open BatMap.Infix

(** Creates top-level env *)
let empty = { bindings      = BatMap.empty
            ; substitutions = BatMap.empty
            ; classes       = BatMap.empty
            ; m             = M.empty }

(** Evaluates name in current scope *)
(* let name_in env = (^) env.prefix *)

(** Adds new binding in current scope *)
let add env name binding = 
  { env with bindings = env.bindings <-- (name, binding) }

let of_binding = 
  function 
  | Fun ({fn = b; _}) | Val b | GlobalVar b  -> b
  | GenericFun _ -> failwith "of_binding GenericFun."

let find env name =
    try BatMap.find name env.bindings 
    with Not_found -> SymbolNotFound name |> raise

let find_val env name = find env name |> of_binding |> fst
