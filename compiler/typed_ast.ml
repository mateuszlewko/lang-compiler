open Lang_parsing.Ast
open Core
open BatPervasives

module LT = Lang_types
module A  = Lang_parsing.Ast

type arg = string * LT.t
[@@deriving show]

type letexp = 
  { name   : string
  ; is_rec : bool 
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

and body_expr = 
  | Var     of string 
  | Lit     of literal
  | Let     of letexp 
  | App     of expr_t * expr_t list 
  | InfixOp of string * expr_t option * expr_t option 
  | If      of ifexp 
  | Exprs   of expr_t list
  [@@deriving show]

and expr_t = body_expr * LT.t
[@@deriving show]

type top = 
  | Expr   of expr_t
  | Extern of string * LT.t
  | Module of string * top list
  | Open   of string
  [@@deriving show]

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

let add_builtin_ops env = 
  let ii2i  = LT.Fun ([LT.Int; LT.Int; LT.Int]) in 
  let ii2b  = LT.Fun ([LT.Int; LT.Int; LT.Bool]) in 
  let bb2b  = LT.Fun ([LT.Bool; LT.Bool; LT.Bool]) in 
  let map t = List.map ~f:(fun x -> x, t) in 
  let a     = ["+"; "-"; "*"; "/"]              |> map ii2i in
  let b     = ["="; "<"; "<="; ">"; ">="; "<>"] |> map ii2b in
  let c     = ["&&"; "||"]                      |> map bb2b in

  List.fold (a @ b @ c) ~init:env ~f:(fun env (n, t) -> add env n t) 
 
let rec expr env = 
  function 
  | VarExp v -> env, (Var v, find env v)
  | LitExp l -> env, lit env l 
  | LetExp (is_rec, (name, ret_t), args, body1, body) -> 
    let args, arg_ts = List.unzip args in 

    let arg_ts = List.map arg_ts LT.of_annotation in 
    let args   = List.zip_exn args arg_ts in 
    let ret_t  = LT.of_annotation ret_t in 
    let fn_t   = LT.merge arg_ts ret_t in 
    let env    = let env = if is_rec then add env name fn_t else env in 
                 List.fold args ~init:env ~f:(fun env (a, t) -> add env a t) in 
    
    let env, body = 
      let init = Option.value body ~default:[] in 
      Option.fold body1 ~init ~f:(flip List.cons)
      |> List.fold_map ~init:env ~f:expr in

    add env name fn_t, (Let { name; is_rec; args; body }, fn_t)
  | AppExp (callee, args1, args2) -> 
    let args = args1 @ (Option.value args2 ~default:[]) 
               |> List.map ~f:(expr env %> snd) in 

    let callee = expr env callee |> snd in 
    env, (App (callee, args), LT.apply (snd callee) (List.map args snd))
  | Exprs es ->
    let rec map env acc = 
      function
      | []      -> env, (Exprs [], LT.Unit)
      | [last]  -> let env, last = expr env last in
                   env, (Exprs (List.rev (last::acc)), snd last)
      | x::xs   -> let env, x = expr env x in 
                   map env (x::acc) xs
    in map env [] es
  | InfixOp (name, lhs, rhs)          -> 
    let op_t   = find env name in 
    let map    = Option.map ~f:(expr env %> snd) in 
    let arg_ts = Option.to_list lhs @ Option.to_list rhs 
               |> List.map ~f:(expr env %> snd %> snd) in 
    env, (InfixOp (name, map lhs, map rhs), LT.apply op_t arg_ts)
  | IfExp (cond, then_, elifs, else_) -> failwith "TODO IfExps"

and lit env = 
  function
  | A.Int i | Int8 i -> Lit (Int    i), LT.Int    
  | String s         -> Lit (String s), LT.String 
  | Bool b           -> Lit (Bool   b), LT.Bool   
  | Array (x::xs)    ->
    let (x, t1), xs = expr env x |> snd, Core.List.map xs (expr env %> snd) in
    if List.exists xs (snd %> (<>) t1)
    then raise ArrayElementsMustHaveSameType 
    else Lit (Array ((x, t1)::xs)), LT.Array t1
  | Unit             -> Lit (Unit    ), LT.Unit
  | Array []         -> Lit (Array []), LT.Array LT.Int

 and top env =
  function 
  | A.Expr e          -> let env, e = expr env e in env, Expr e
  | Extern (name, ta) -> let t = LT.of_annotation (Some ta) in 
                         add env name t, Extern (name, t)
  | _                 -> failwith "TODO2" 

let of_tops tops = 
  let env = empty |> add_builtin_ops in 
  List.folding_map tops ~init:env ~f:top
