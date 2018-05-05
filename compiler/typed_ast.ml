open Lang_parsing.Ast
open Core
open BatPervasives

module LT = Lang_types
module A  = Lang_parsing.Ast

type arg = string * LT.t

type letexp = 
  { is_rec : bool 
  ; args   : arg list
  ; body   : expr_t list }

and ifexp = 
  { cond      : expr_t 
  ; then_body : expr_t
  ; else_body : expr_t }

and literal =
  | Int    of int
  | String of string
  | Bool   of bool
  | Array  of expr_t list
  | Unit

and expr = 
  | Var     of string 
  | Lit     of literal
  | Let     of letexp 
  | App     of expr_t * expr_t list 
  | InfixOp of expr_t option * expr_t option 
  | If      of ifexp 
  | Exprs   of expr_t list

and expr_t = expr * LT.t

type top = 
  | Expr   of expr_t
  | Extern of string * LT.t
  | Module of string * top list
  | Open   of string

type bindings_map = (string, LT.t) BatMap.t

type environment = { 
  (** symbols accessible with prefix *)
    prefixed : bindings_map
  (** symbols available without any prefix  *)
  ; opened   : bindings_map
  (** current scope (module) prefix *)
  ; prefix   : string
  } 

(** Creates top-level env *)
let empty = { prefixed = BatMap.empty
            ; opened   = BatMap.empty
            ; prefix   = "" }

(** Evaluates name in current scope *)
let name_in env = (^) env.prefix

(** Adds new binding in current scope *)
let add env name value =
    let pref_name = name_in env name in
    
    { env with prefixed = BatMap.add pref_name value env.prefixed 
             ; opened   = BatMap.add name value env.opened }

let find env name =
    try BatMap.find name env.opened
    with Not_found -> BatMap.find (name_in env name) env.prefixed

exception ArrayElementsMustHaveSameType 

let rec expr env = 
  function 
  | VarExp v -> Var v, find env v
  | LitExp l -> lit env l 
  | LetExp (is_rec, ret, args, body1, body) -> 
      let body = 
        let init = Option.value body ~default:[] in 
        Option.fold body1 ~init ~f:(flip List.cons) in
      assert false
  | other -> assert false

and lit env = 
  function
  | A.Int i | Int8 i -> Lit (Int    i), LT.Int    
  | String s         -> Lit (String s), LT.String 
  | Bool b           -> Lit (Bool   b), LT.Bool   
  | Array (x::xs)    ->
    let (x, t1), xs = expr env x, Core.List.map xs (expr env) in
    if List.exists xs (snd %> (<>) t1)
    then raise ArrayElementsMustHaveSameType 
    else Lit (Array ((x, t1)::xs)), LT.Array t1
  | Unit             -> Lit (Unit    ), LT.Unit
  | Array []         -> Lit (Array []), LT.Array LT.Int

 (* of_top = 
  | A.Expr -> *)

(* let of_prog (Prog tops) = 
  List.map tops top *)
