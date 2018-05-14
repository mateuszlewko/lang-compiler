open High_ollvm

type bound        = Ez.Value.t * Lang_types.t 
type fun_binding  = { fn : bound; arr : Ez.Value.t; arity : int }
type binding      = Fun of fun_binding | Val of bound
type bindings_map = (string, binding) BatMap.t

exception SymbolNotFound of string

module V = Ez.Value
module M = Ez.Module

type environment = 
  { bindings : bindings_map
  (** low-level module *)
  ; m        : M.t
  } 

type t = environment

open BatMap.Infix

(** Creates top-level env *)
let empty = { bindings = BatMap.empty
            ; m        = M.empty }

(** Evaluates name in current scope *)
(* let name_in env = (^) env.prefix *)

(** Adds new binding in current scope *)
let add env name binding = 
  { env with bindings = env.bindings <-- (name, binding) }

let of_binding = function Fun ({fn = b; _}) | Val b -> b

let find env name =
    try BatMap.find name env.bindings 
    with Not_found -> SymbolNotFound name |> raise

let find_val env name = find env name |> of_binding |> fst
