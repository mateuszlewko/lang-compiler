open Lang_parsing.Ast
open Core
open BatPervasives
include Typed_ast_def 

module LT = Lang_types
module A  = Lang_parsing.Ast

let add_raw env name (key_type : string -> key) value = 
  let pref_name = name_in env name in
  
  { env with 
    prefixed = BatMap.add (key_type pref_name) (value, pref_name) env.prefixed 
  ; opened   = BatMap.add (key_type name) (value, pref_name) env.opened }

(** Adds new binding in current scope *)
let add env name value      = add_raw env name (fun x -> Val x) value
let add_type env name value = add_raw env name (fun x -> Type x) value

(* let find_type env name = 
  try BatMap.find (Type name) env.opened |> Some
  with Not_found -> 
  try BatMap.find (Type (name_in env name)) env.prefixed |> Some
  with Not_found -> None *)

(* TODO: Don't duplicate this code *)
let find_fields env fields = 
  try BatMap.find (Fields fields) env.opened |> Some
  with Not_found -> 
  try BatMap.find (Fields fields) env.prefixed |> Some
  with Not_found -> None

(* let (!->) env = find_type env %> Option.map ~f:(fst %> fst) *)

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
  | LetExp { name; is_rec; args = []; ret_t; body } -> 
    if is_rec = true 
    then failwith "Value cannot be defined with 'rec'";

    let env, body = List.fold_map body ~init:env ~f:expr in

    let t = List.last_exn body |> snd in 
    add env name (t, AtLevel env.level), (Value (name, (Exprs body, t)), t)
  | LetExp { name; is_rec; args; ret_t; body }  -> 
    let args_names, arg_ts = List.unzip args in 

    let env, arg_ts = List.fold_map arg_ts ~init:env ~f:LT.of_annotation  in 
    let args        = List.zip_exn args_names arg_ts in 
    let env, ret_t  = LT.of_annotation env ret_t in 
    let fn_t        = LT.merge arg_ts ret_t in 
    
    let env = { env with level = env.level + 1 } in
    let lvl = AtLevel env.level in 
    let env = let env = if is_rec 
                        then add env name (fn_t, Global) 
                        else env in 
              List.fold args ~init:env 
                             ~f:(fun env (a, t) -> add env a (t, lvl)) in 
    
    let env, body = List.fold_map body ~init:env ~f:expr in

    let extra_args   = BatMultiMap.enum env.free_vars |> BatList.of_enum 
                       |> List.map ~f:snd in 

    (* apply unbound type to concrete *)
    let env = 
      match LT.valid_replacements ret_t (List.last_exn body |> snd) with 
      | []   -> env 
      | subs -> let substitutions = List.fold subs ~init:env.substitutions 
                                    ~f:(fun m (u, v) -> BatMap.add u v m) in 
              { env with substitutions } in 

    let try_concrete t = LT.find_concrete_lt env.substitutions t 
                        |> Option.value ~default:t in 

    let fn_t = match fn_t with 
              | LT.Fun ts -> List.map ts try_concrete |> LT.Fun
              | other     -> other in 

    let extra_arg_ts = List.map extra_args (snd %> try_concrete) in
    let args         = List.zip_exn args_names (List.map arg_ts try_concrete) in 
    let extra_args   = List.zip_exn (List.map extra_args fst) extra_arg_ts in 

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

  | AppExp (callee, args) -> 
    let env, args   = List.fold_map args ~init:env ~f:expr in 
    printf "callee: %s\n" (Lang_parsing.Ast.show_expr callee);
    let env, callee = expr env callee in 
    printf "callee_t: %s\n" (show_expr_t callee);
    let env, e = 
      LT.apply env (snd callee) (List.map args snd) (App (callee, args)) in 

    env, e
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

    let env, lhs = expr env lhs in 
    let env, rhs = expr env rhs in 
    let arg_ts   = List.map [lhs; rhs] snd in 

    printf "doing op: %s\n" name;

    let env, e = LT.apply env op_t arg_ts (InfixOp (name, lhs, rhs)) in 
    env, e
  | IfExp { cond; then_; elifs; else_ } ->
    let env, cond      = expr env cond |> uncurry (LT.unify_expr LT.Bool) in 
    let env, then_body = expr env then_ in 
    let env, else_body = 
      match elifs with 
      | [] -> Option.value else_ ~default:(A.LitExp (A.Unit)) |> expr env
      | (cond, then_)::elifs -> expr env (IfExp {cond; then_; elifs; else_}) in
    let env, then_body = LT.unify_expr (snd else_body) env then_body in 
   
    if snd then_body <> snd else_body 
    then raise IfBranchesTypeMismatched;

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

    begin
    match t with 
    | Some ((t, Global), _) -> env, (r, t)
    | _                     ->
      sprintf "Couldn't find type for record literal: %s.\n" (show_expr rl)
      |> failwith
    end
  | LetRecsExp ls -> failwith "typedAst LetRecsExp TODO"  

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

and funexp_raw env { name; is_rec; args; ret_t; body } =
  let args_names, arg_ts = List.unzip args in 

  (* TODO: This code is duplicated *)
  let env, arg_ts = List.fold_map arg_ts ~init:env ~f:LT.of_annotation  in 
  let args        = List.zip_exn args_names arg_ts in 
  let env, ret_t  = LT.of_annotation env ret_t in 
  let fn_t        = LT.merge arg_ts ret_t in 

  let env = if is_rec then add env name (fn_t, Global) else env in 
  let env = List.fold args ~init:env 
                           ~f:(fun env (a, t) -> 
                                add env a (t, AtLevel env.level)) in 
  
  let env = { env with level = env.level + 1} in 
  let env, body = List.fold_map body ~init:env ~f:expr in
  let env = { env with level     = env.level - 1
                     ; free_vars = BatMultiMap.empty } in 

  printf "FUN name: %s\n" name; 

  LT.show__substitutions (BatMap.bindings env.substitutions)
  |> printf "inner funexp subs: %s\n";

  (* TODO: Duplicated code *)
  let env = 
    match LT.valid_replacements ret_t (List.last_exn body |> snd) with 
    | []   -> env 
    | subs -> LT.show__substitutions subs |> printf "adding last subs: %s\n";
              let substitutions = List.fold subs ~init:env.substitutions 
                                  ~f:(fun m (u, v) -> BatMap.add u v m) in 
              { env with substitutions } in 

  let try_concrete t = LT.find_concrete_lt env.substitutions t 
                       |> Option.value ~default:t in 
                       
  let fn_t = match fn_t with 
             | LT.Fun ts -> List.map ts try_concrete |> LT.Fun
             | other     -> other in 

  printf "final fn_t: %s\n" (LT.show fn_t);

  let args = List.zip_exn args_names (List.map arg_ts try_concrete) in 

  let gen_name = name_in env name in 
  let f        = { name = gen_name; gen_name; is_rec; args; body } in 
  add env name (fn_t, Global), f, fn_t

and funexp env let_exp = 
  let env, f, fn_t = funexp_raw env let_exp in 
  env, [Fun (f, fn_t)]

 and top env =
  function 
  | A.Expr (LetExp e) ->
    funexp env e
  | Expr e            -> let env, e = expr env e in env, [Expr e]
  | Extern (name, ta) -> 
    let env, t = LT.of_annotation env (Some ta) in 
    add env name (t, Global), [Extern ( { name     = name_in env name
                                        ; gen_name = name }
                                      , t )]
  | Module (name, tops) -> 
    let parent_prefix = env.prefix in 
    let parent_opened = env.opened in 
    let env           = { env with prefix = env.prefix ^ name ^ "." } in 
    let env, tops     = List.fold_map tops ~init:env ~f:top in
    let env           = { env with prefix = parent_prefix
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
      |> BatMap.filter (function 
                        | Type s | Val s (*| GenericFun (s, _)*) -> 
                          starts_with s path |> const
                        | Fields _                           -> const true)
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
    (* let full_name = name_in env name in
    let env    = add_type env name (LT.Ptr full_name, Global) in *)
    let get t  = LT.of_annotation env (Some t) in  
    let env, fields = 
      List.fold_map fields ~init:env ~f:(fun env (f_name, ft) -> 
                                           let env, t = get ft in 
                                           env, (f_name, t)) in

    let t   = LT.Record fields in
    let env = add_raw env name (const (Fields (BatSet.of_list fields)))
              (t, Global) in 

    add_type env name (t, Global), []
  | Class { declarations; name; type_name }    -> 
    let env = 
    List.fold declarations ~init:env 
      ~f:(fun env (name, t) -> 
            let env, t = LT.of_annotation env (Some t) in 
            add env name (t, Global)) in 

    (* let class_t = Some { basic = A.Single [type_name]; classes = [] } 
                  |> LT.of_annotation !-> env  in  *)

    env, [Class (name, type_name, List.map declarations (fst %> name_in env))]
  | Instance { class_name; definitions; type_ } -> 
    let parent_env     = env in 
    (* create funexps representing methods in class *)
    let funexp env e = let env, f, t = funexp_raw env e in env, (f, t) in 
    let env, funexps = List.fold_map definitions ~init:env ~f:funexp in 
    (* replace opened and prefixed symbols with ones from parent_env  *)
    let env = { env with prefixed = parent_env.prefixed
                       ; opened   = parent_env.opened } in 
    
    let env, impl_t = LT.of_annotation env (Some type_) in 

    printf "Adding instance for type: %s\n" (LT.show impl_t);

    env, [Instance (class_name, impl_t, funexps)]

let of_tops tops = 
  let env = empty |> add_builtin_ops in 
  let top env expr =
    let env, res = top env expr in 
    let extra_fun = List.rev_map env.extra_fun (fun (f, t) -> Fun (f, t)) in 
    { env with extra_fun = [] }, extra_fun @ res in 

  let env, tops = 
    List.fold_map tops ~init:env ~f:top in 
  List.concat tops  