open Lang_parsing.Ast
open Core
open BatPervasives

module LT = Lang_types
module A  = Lang_parsing.Ast

type arg = string * LT.t
[@@deriving show]

type funexp = 
  { name     : string
  ; gen_name : string
  ; is_rec   : bool 
  ; args     : arg list
  ; body     : expr_t list }

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
  | Var       of string 
  | SetVar    of string * expr_t
  | Lit       of literal
  | Value     of string * expr_t
  | App       of expr_t * expr_t list 
  | InfixOp   of string * expr_t option * expr_t option 
  | If        of ifexp 
  | GepLoad   of expr_t * int list
  | Clone     of expr_t
  | GepStore  of gep_store
  | RecordLit of expr_t list
  | Exprs     of expr_t list
  [@@deriving show]

and expr_t = body_expr * LT.t
[@@deriving show]

type extern = { name : string; gen_name : string }
[@@deriving show]

type top = 
  | Expr   of expr_t
  | Fun    of funexp * LT.t
  | Extern of extern * LT.t
  | Module of string * top list
  | Open   of string
  [@@deriving show]

type location = AtLevel of int | Global 
[@@deriving show]
type bound = LT.t * location
[@@deriving show]
type bbb = bound * string 
[@@deriving show]

type key = Type of string | Val of string | Fields of (string * LT.t) BatSet.t
(* [@@deriving show] *)
 
type bindings_map = (key, bound * string) BatMap.t

type environment = { 
  (** symbols accessible with prefix *)
    prefixed   : bindings_map
  (** symbols available without any prefix  *)
  ; opened     : bindings_map
  (** current scope (module) prefix *)
  ; prefix     : string
  ; free_vars  : (int, string * LT.t) BatMultiMap.t
  ; level      : int 
  ; extra_fun  : (funexp * LT.t) list 
  } 

(** Creates top-level env *)
let empty = { prefixed  = BatMap.empty
            ; opened    = BatMap.empty
            ; prefix    = "." 
            ; free_vars = BatMultiMap.empty
            ; level     = 0 
            ; extra_fun = [] }

(** Evaluates name in current scope *)
let name_in env = (^) env.prefix

let add_raw env name (key_type : string -> key) value = 
  let pref_name = name_in env name in
  
  { env with 
    prefixed = BatMap.add (key_type pref_name) (value, pref_name) env.prefixed 
  ; opened   = BatMap.add (key_type name) (value, pref_name) env.opened }

(** Adds new binding in current scope *)
let add env name value      = add_raw env name (fun x -> Val x) value
let add_type env name value = add_raw env name (fun x -> Type x) value

let find_type env name = 
  try BatMap.find (Type name) env.opened |> Some
  with Not_found -> 
  try BatMap.find (Type (name_in env name)) env.prefixed |> Some
  with Not_found -> None

(* TODO: Don't duplicate this code *)
let find_fields env fields = 
  try BatMap.find (Fields fields) env.opened |> Some
  with Not_found -> 
  try BatMap.find (Fields fields) env.prefixed |> Some
  with Not_found -> None

let (!->) env = find_type env %> Option.map ~f:(fst %> fst)

let find env name =
  try BatMap.find (Val name) env.opened 
  with Not_found -> 
  try BatMap.find (Val (name_in env name)) env.prefixed 
  with Not_found ->
    env.opened 
    |> BatMap.iter (fun k b -> printf "s: %s, b: %s" ("show_key k") 
                                 (show_bbb b));

    sprintf "Not found: %s, with prefix: %s\n" name env.prefix 
    |> failwith

exception ArrayElementsTypeMismatched
exception IfBranchesTypeMismatched 

let add_builtin_ops env = 
  let ii2i  = LT.Fun ([LT.Int; LT.Int; LT.Int]) in 
  let ii2b  = LT.Fun ([LT.Int; LT.Int; LT.Bool]) in 
  let bb2b  = LT.Fun ([LT.Bool; LT.Bool; LT.Bool]) in 
  let map t = List.map ~f:(fun x -> x, t) in 
  let a     = ["+"; "-"; "*"; "/"]              |> map ii2i in
  let b     = ["="; "<"; "<="; ">"; ">="; "<>"] |> map ii2b in
  let c     = ["&&"; "||"]                      |> map bb2b in

  List.fold (a @ b @ c) ~init:env ~f:(fun env (n, t) -> add env n (t, Global)) 
 
let rec expr env = 
  function 
  | VarExp v -> 
    let (t, loc), pref_name = find env v in
    let var = Var v, t in 
    begin 
    match loc with 
    | Global -> env, (Var pref_name, t)
    | AtLevel l when l = env.level -> 
      printf "var: %s is at level: %d\n" v l;
      env, var
    | AtLevel l -> let free_vars = BatMultiMap.add l (v, t) env.free_vars in 
                   printf "var: %s is at lower level: %d\n" v l;
                   { env with free_vars }, var
    end
  | LitExp l -> env, lit env l
  | LetExp (is_rec, (name, ret_t), [], body1, body) -> 
    if is_rec = true 
    then failwith "Value cannot be defined with 'rec'";

    let env, body = 
      let init = Option.value body ~default:[] in 
      Option.fold body1 ~init ~f:(flip List.cons)
      |> List.fold_map ~init:env ~f:expr in

    let t = List.last_exn body |> snd in 
    add env name (t, AtLevel env.level), (Value (name, (Exprs body, t)), t)
  | LetExp (is_rec, (name, ret_t), args, body1, body) -> 
    let args, arg_ts = List.unzip args in 

    let arg_ts = List.map arg_ts (LT.of_annotation !-> env) in 
    let args   = List.zip_exn args arg_ts in 
    let ret_t  = LT.of_annotation !-> env ret_t in 
    let fn_t   = LT.merge arg_ts ret_t in 
    
    let env = { env with level = env.level + 1 } in
    let lvl = AtLevel env.level in 
    let env = let env = if is_rec 
                        then add env name (fn_t, Global) 
                        else env in 
              List.fold args ~init:env 
                             ~f:(fun env (a, t) -> add env a (t, lvl)) in 
    
    let env, body = 
      let init = Option.value body ~default:[] in 
      Option.fold body1 ~init ~f:(flip List.cons)
      |> List.fold_map ~init:env ~f:expr in

    let extra_args   = BatMultiMap.enum env.free_vars |> BatList.of_enum 
                       |> List.map ~f:snd in 

    let extra_arg_ts = List.map extra_args snd in
 
    let global_fn_t = LT.merge extra_arg_ts fn_t in
    let g_name      = name ^ ".lifted" in
    let global_fn   = 
      let args = extra_args @ args in 
      printf "fun: %s, extra args:\n" g_name;
      List.iter extra_args (show_arg %> printf "e_arg: %s\n"); 

      { name = g_name; gen_name = g_name; is_rec; args; body }, global_fn_t in
    let env         = { env with extra_fun = global_fn::env.extra_fun } in
 
    let fn_with_env  = 
      let args = List.map extra_args (fun (s, t) -> Var s, t) in
      let app  = App ((Var g_name, global_fn_t), args) in 
      Value (name, (app, fn_t)) in

    (* remove vars defined in current scope *)
    let free_vars = BatMultiMap.remove_all env.level env.free_vars in 
    let env       = { env with level = env.level - 1; free_vars } in
    (* added new binding to parent scope *)
    let env       = add env name (fn_t, AtLevel env.level) in 

    env, (fn_with_env, fn_t)

  | AppExp (callee, args1, args2) -> 
    let env, args   = args1 @ (Option.value args2 ~default:[]) 
                      |> List.fold_map ~init:env ~f:expr in 
    let env, callee = expr env callee in 

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
    let (op_t, loc), name = find env name in 
   
    if loc <> Global 
    then sprintf "Non global operator: %s is not supported." name |> failwith;

    let open Option in 
    let env, arg_ts = to_list lhs @ to_list rhs 
                      |> List.fold_map ~init:env ~f:(expr) in 
    let arg_ts = List.map arg_ts snd in 

    (* TODO: Handle cases when one of operands is missing *)
    let Some (env, lhs) = map lhs ~f:(expr env) in 
    let Some (env, rhs) = map rhs ~f:(expr env) in 

    env, (InfixOp (name, Some lhs, Some rhs), LT.apply op_t arg_ts)
  | IfExp (cond, then_, elifs, else_) ->
    let env, then_body = expr env then_ in 
    let env, else_body = 
      match elifs with 
      | [] -> Option.value else_ ~default:(A.LitExp (A.Unit)) |> expr env
      | (cond, body)::es -> expr env (IfExp (cond, body, es, else_)) in
   
    if snd then_body <> snd else_body 
    then raise IfBranchesTypeMismatched;

    let env, cond = expr env cond in 
    env, (If { cond; then_body; else_body }, snd then_body)
  | FieldGetExp (e, field) -> 
    let env, (expr, t) = expr env e in 
    let fail () = LT.show t 
                  |> sprintf "Field %s is not defined on type %s.\n" field 
                  |> failwith in
    begin
    match t with 
    | Record fields -> 
      begin 
      match List.findi fields (fun _ (f,_ ) -> f = field) with 
      | Some (i, (_, field_t)) -> env, (GepLoad ((expr, t), [0; i]), field_t)
      | None                   -> fail () 
      end
    | _             -> fail ()
    end
  | RecordWithExp (e, withs) -> 
    let env, e = expr env e in 
    let t      = snd e in 
    let e      = Clone e, t in 
    begin 
    match t with 
    | Record fields -> 
      let set (env, e) (field, with_e) =  
        match List.findi fields (fun _ (f, ft) -> f = field) with 
        | None        -> failwith "Field not found.\n"
        | Some (i, _) -> 
          let env, with_e = expr env with_e in 
          let ft          = snd (List.nth_exn fields i) in
          
          if snd with_e <> ft 
          then sprintf "Expression in field %s assignment has incorrect type, \
                        expected: %s, instead of: %s.\n" 
                        field (LT.show ft) (LT.show (snd with_e)) |> failwith;
          env, (GepStore { src = with_e; dest = e; idx = [0; i] }, t) in
      List.fold withs ~init:(env, e) ~f:set
    | other         -> sprintf "Expression preceding with must be a record, \
                                not: %s.\n" (LT.show t) |> failwith
    end                        

  | RecordLiteral fields as rl -> 
    (* TODO: sort fields by index *)
    let env, fs = List.fold_map fields env (fun env (_, f) -> expr env f) in
    let r       = RecordLit fs in
    let t       = List.zip_exn (List.map fields fst) (List.map fs snd)
                  |> BatSet.of_list |> find_fields env in 
    match t with 
    | Some ((t, Global), _) -> env, (r, t)
    | _                     ->
      sprintf "Couldn't find type for record literal: %s.\n" (show_expr rl)
      |> failwith
    

and lit env = 
  function
  | A.Int i | Int8 i -> Lit (Int    i), LT.Int    
  | String s         -> Lit (String s), LT.String 
  | Bool b           -> Lit (Bool   b), LT.Bool  
  | Array (x::xs)    ->
    (* FIXME: TODO: add env passing here *)
    let (x, t1), xs = expr env x |> snd, Core.List.map xs (expr env %> snd) in
    if List.exists xs (snd %> (<>) t1)
    then raise ArrayElementsTypeMismatched 
    else Lit (Array ((x, t1)::xs)), LT.Array t1
  | Unit             -> Lit (Unit    ), LT.Unit
  | Array []         -> Lit (Array []), LT.Array LT.Int

and funexp env (is_rec, (name, ret_t), args, body1, body) =
  let gen_name = name_in env name in 
  let args, arg_ts = List.unzip args in 

  let arg_ts = List.map arg_ts (LT.of_annotation !-> env) in 
  let args   = List.zip_exn args arg_ts in 
  let ret_t  = LT.of_annotation !-> env ret_t in 
  let fn_t   = LT.merge arg_ts ret_t in 
  let env    = let env = if is_rec 
                         then add env name (fn_t, Global) 
                         else env in 
               List.fold args ~init:env 
                              ~f:(fun env (a, t) -> 
                                   add env a (t, AtLevel env.level)) in 
  
  let env = { env with level = env.level + 1} in 
  let env, body = 
    let init = Option.value body ~default:[] in 
    Option.fold body1 ~init ~f:(flip List.cons)
    |> List.fold_map ~init:env ~f:expr in
  let env = { env with level     = env.level - 1
                     ; free_vars = BatMultiMap.empty } in 

  add env name (fn_t, Global), [Fun ({ name = gen_name; gen_name; is_rec
                                     ; args; body }, fn_t)]

 and top env =
  function 
  | A.Expr (LetExp (is_rec, (name, ret_t), args, body1, body)) ->
    funexp env (is_rec, (name, ret_t), args, body1, body)
  | Expr e            -> let env, e = expr env e in env, [Expr e]
  | Extern (name, ta) -> 
    let t = LT.of_annotation !-> env (Some ta) in 
    add env name (t, Global), [Extern ( { name     = name_in env name
                                        ; gen_name = name }
                                      , t )]
  | Module (name, tops) -> 
    let parent_prefix = env.prefix in 
    let parent_opened = env.opened in 
    let env         = { env with prefix = env.prefix ^ name ^ "." } in 
    let env, tops   = List.fold_map tops ~init:env ~f:top in
    let env         = { env with prefix = parent_prefix
                               ; opened = parent_opened } in
    env, List.concat tops
  | Open path          -> 
    let merge key l r = 
      match l, r with 
      | Some l, Some r              -> Some r
      | Some x, None | None, Some x -> Some x
      | None  , None                -> None in

    let open BatString in 
    let path   = env.prefix ^ path ^ "." in 
    let opened =
      (* All symbols *)
      BatMap.merge merge env.prefixed env.opened
      (* Select symbols that will be opened *)
      |> BatMap.filter (function | Type s | Val s -> starts_with s path |> const
                                 | Fields _       -> const true)
      (* |> fun m -> BatMap.iter (fun k _ -> printf "-- o key: %s\n" k) m; m *)
      (* Remove path prefix from selected symbols *)
      |> fun map -> 
        BatMap.foldi 
          (fun key v new_map ->
            let add (k_type : string -> key) (key : string) = 
              let path_len = length path in
              let new_name = sub key path_len (length key - path_len) in
              BatMap.add (k_type new_name) v new_map in
            match key with 
            | Type key -> add (fun x -> Type x) key 
            | Val key  -> add (fun x -> Val x) key 
            | other    -> BatMap.add other v new_map
          )
          map BatMap.empty
      (* Merge with previously opened symbols, possibly overwriting
         some of them *)
      |> BatMap.merge merge env.opened in
    { env with opened }, []
  | TypeDecl (RecordType (name, fields))        -> 
    let get t  = LT.of_annotation !-> env (Some t) in  
    let fields = List.map fields (fun (f_name, ft) -> f_name, get ft) in 
    let t      = LT.Record fields in
    let env    = add_raw env name (const (Fields (BatSet.of_list fields))) 
                   (t, Global) in 
    add_type env name (t, Global), []

let of_tops tops = 
  let env = empty |> add_builtin_ops in 
  let top env expr =
    let env, res = top env expr in 
    let extra_fun = List.map env.extra_fun (fun (f, t) -> Fun (f, t)) in 
    { env with extra_fun = [] }, extra_fun @ res in 

  let env, tops = 
    List.fold_map tops ~init:env ~f:top in 
  List.concat tops  