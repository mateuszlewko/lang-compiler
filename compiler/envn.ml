open High_ollvm

type bindings_map = (string, Ez.Value.t) BatMap.t

module V = Ez.Value
module M = Ez.Module

type environment = { 
  (** symbols accessible with prefix *)
    prefixed : bindings_map
  (** symbols available without any prefix  *)
  ; opened   : bindings_map
  (** low-level module *)
  ; m        : M.t
  (** Top level values.
      Each pair is a function with 0 arguments and variable
      equal to result of function call (if any) *)
  ; top_vals : (V.t * V.t option) list
  (** current scope (module) prefix *)
  ; prefix   : string
  } 

(** Creates top-level env *)
let empty = { prefixed = BatMap.empty
            ; opened   = BatMap.empty
            ; prefix   = ""
            ; top_vals = []
            ; m        = M.empty }

(** Evaluates name in current scope *)
let name_in env = (^) env.prefix

(** Adds new binding in current scope *)
let add env name value =
    let pref_name = name_in env name in
    
    { env with prefixed = BatMap.add pref_name value env.prefixed 
              ; opened  = BatMap.add name value env.opened }
