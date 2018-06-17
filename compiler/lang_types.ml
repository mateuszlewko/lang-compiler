open Core
open BatPervasives
include Lang_types_def

module A   = Lang_parsing.Ast
module TAD = Typed_ast_def

(* type t = Lang_types_def.lang_type *)
[@@deriving show]

exception UnsupportedType of string
exception WrongNumberOfApplyArguments
exception WrongTypeOfApplyArgument
exception ValueCannotBeApplied 

let find_type (env : TAD.environment) name  = 
  begin
  try BatMap.find (TAD.Type name) env.opened |> Some
  with Not_found -> 
  try BatMap.find (TAD.Type (TAD.name_in env name)) env.prefixed |> Some
  with Not_found -> None
  end
  |> Option.map ~f:(fst3 %> fst)

let of_annotation ?(mono=false) env annotation =
  let rec of_basic =
    function
    | A.Fun []                   -> raise (UnsupportedType "<empty>")
    | Fun [t]                    -> of_basic t
    | Fun ts                     -> Fun (List.map ts of_basic)
    | Single ["int"]             -> Int
    | Single ["bool"]            -> Bool
    | Single ["int"; "array"]    -> Array Int
    | Single (["()"] | ["unit"]) -> Unit
    | Single [t] when BatString.starts_with t "'" -> Generic t
    | Single [name]              -> 
      begin 
      match find_type env name with 
      | Some t -> t 
      | None   -> UnsupportedType name |> raise
      end
    | Single other ->
      BatString.concat " " other
      |> UnsupportedType |> raise in

  match annotation with 
  | Some { A.basic; _ } -> env, of_basic basic 
  | None                -> TAD.fresh_type env

let value_if_not_function = 
  function
  | Fun []      -> raise (UnsupportedType "<empty function>")
  | Fun [x]     -> x 
  | Fun _ as ft -> ft
  | other       -> other 

let merge ts = 
  begin
  function
  | Fun latter_ts -> Fun (ts @ latter_ts)
  | last_t        -> Fun (ts @ [last_t])
  end %> value_if_not_function

let rec replacements old_t new_t =
  match old_t, new_t with 
  | Fun [t]           , other                -> replacements t other 
  | old               , Fun [other]          -> replacements old other
  | Generic _ as old_t, new_t                
  | new_t             , (Generic _ as old_t) -> [old_t, new_t]
  | Fun ts            , Fun new_ts           -> 
    let min_len = min (List.length ts  - 1) (List.length new_ts - 1) in 
    let ts    , ret_t    = List.split_n ts min_len in 
    let new_ts, ret_newt = List.split_n new_ts min_len in 
    (List.map2_exn ts new_ts replacements
     |> List.concat) @ replacements (Fun ret_t) (Fun ret_newt) 
  | old_t             , new_t                ->
    if old_t = new_t 
    then []
    else (
      printf "Can't do replacement for %s and %s.\n" (show old_t) (show new_t);
      raise WrongTypeOfApplyArgument)

let rec add_equality ?(both=true) t1 t2 substitutions = 
  let last = try BatMap.find t1 substitutions 
             with Not_found -> [] in 

  BatMap.add t1 (t2::last) substitutions
  |> if both then add_equality ~both:false t2 t1 
             else identity

let valid_replacements old_t new_t = replacements old_t new_t 
                                     |> List.dedup_and_sort 
                                     |> List.filter ~f:(uncurry (<>))

let unify_expr new_t (env : TAD.environment) (expr, et) =
  match valid_replacements et new_t with 
  | []   -> env, (expr, et)
  | subs -> let substitutions = List.fold subs ~init:env.substitutions 
                                  ~f:(fun m (u, v) -> add_equality u v m) in 
            let env = { env with substitutions } in 
            env, (TAD.Substitute (subs, (expr, new_t)), new_t)

type _substitutions = (t * t) list 
[@@deriving show]

type _substitutions2 = (t * t list) list 
[@@deriving show]

let show_subs substitutions = 
  BatMap.bindings substitutions
  |> show__substitutions2

let rec find_concrete substitutions generic_t = 
  let rec dfs vis curr = 
    try BatSet.find curr vis |> ignore; vis, None 
    with Not_found ->
      printf "curr: %s\n" curr;

      let vis = BatSet.add curr vis in  

      let rec find_first vis =  
        function 
        | (Generic s)::subs -> 
          (* let alt    = 
            (* let s_pref   = BatMap.find_default 0 s preferred in *)
            (* let alt_pref = Option.value_map alt ~default:0 ~f:snd in  *)
            
            if s_pref < alt_pref
            then Some (Generic s, s_pref)
            else alt in  *)

          let vis, t = dfs vis s in 
          begin 
          match t with 
          | Some _ as t -> 
            vis, t
          | None        -> find_first vis subs 
          end
        | concrete::_ ->
          printf "found: %s\n" (show concrete);
          vis, Some concrete
        | []          -> vis, None          in 

      let neighbours = BatMap.find_default [] (Generic curr) substitutions 
                       |> List.stable_dedup in 

      printf "ns: ";
      List.iter neighbours (show %> printf "%s, ");
      printf "\n";

      match neighbours with 
      | []   -> vis, None
      | subs -> find_first vis subs in 

  dfs BatSet.empty generic_t |> snd

let rec find_concrete_lt ?(preferred=BatMap.empty) substitutions = 
  function 
  | Generic generic_t -> find_concrete substitutions generic_t 
  | Fun ts            -> 
    List.map ts (fun t -> find_concrete_lt ~preferred substitutions t
                          |> Option.value ~default:t)
    |> Fun |> Some
  | other             -> Some other

let rec drop_args n vis subs t = 
  match n, t with  
  | 0, Fun [t] | 0, t          -> Some t 
  | n, Fun (_::((_::_) as ts)) -> drop_args (n - 1) vis subs (Fun ts) 
  | n, (Generic _ as t)
  | n, Fun [Generic _ as t]    -> 
    begin 
    match BatMap.Exceptionless.find t subs with 
    | Some (Fun (_::((_::_) as ts)) as t) when BatSet.mem t vis |> not ->
      drop_args (n - 1) (BatSet.add t vis) subs (Fun ts)
    | _ -> None 
    end 
  | _, t            -> None 

let rec apply (env : TAD.environment) fn_t arg_ts = 
  let no_substitution t e = env, (e, t) in 
  
  let t_as_fun gen_t arg_ts = fun expr ->
    let env, ret_t     = TAD.fresh_type env in 
    let fn_t           = Fun (arg_ts @ [ret_t]) in 
    
    (* printf "new fn_t is: %s\n" (show fn_t); *)
    let (env : TAD.environment), (expr, t) = apply env fn_t arg_ts expr in 
   
    let substitutions  = add_equality gen_t fn_t env.substitutions in 
    let env            = { env with substitutions } in

   env, (TAD.Substitute ([gen_t, fn_t], (expr, t)), t) in
  
  match fn_t, arg_ts with 
  | _        , []     -> no_substitution fn_t 
  (* | Generic _ as gen_t, arg_ts -> t_as_fun gen_t arg_ts *)
  | fn_t, arg_ts -> 
    (* let cnt = List.length arg_ts in 

    if cnt >= List.length ts 
    then 

    else  *)
    (* let before, after = List.split_n ts cnt in  *)
    
    (* let substitution t =  *)
    begin
    let env, ret_t = TAD.fresh_type env in 

    match replacements fn_t (Fun (arg_ts @ [ret_t]))
          |> List.dedup_and_sort 
          |> List.filter ~f:(uncurry (<>))
    with 
    | []   -> no_substitution fn_t
    | subs -> 
      show__substitutions subs |> printf "adding subs: %s\n";

      (* let t = List.find subs (fst %> (=) fn_t) 
              |> Option.map ~f:snd |> Option.value ~default:fn_t in  *)
      let cnt    = List.length arg_ts in 
      let subs_m = subs |> BatList.enum |> BatMap.of_enum in
      let t      = BatOption.get_exn (drop_args cnt BatSet.empty subs_m fn_t)
                                      WrongNumberOfApplyArguments in

      fun expr -> 
        let substitutions = List.fold subs ~init:env.substitutions 
                              ~f:(fun m (u, v) -> add_equality u v m) in 

        { env with substitutions }, (TAD.Substitute (subs, (expr, t)), t) 
    end
    (* begin 
    match List.exists2 before arg_ts (<>) with 
    | Ok _        -> 
      begin 
      match after with 
      | []  -> 
        raise WrongNumberOfApplyArguments
      | [t] -> substitution t 
      | ts  -> Fun ts |> substitution
      end
    | Unequal_lengths -> raise WrongNumberOfApplyArguments
    (* | Ok true         -> 
      printf "fn_t: %s\n" (show_lang_type fn_t);
      List.iter arg_ts (show_lang_type %> printf "arg: %s\n");
      raise WrongTypeOfApplyArgument *)
    end *)
  | _     , _      -> raise ValueCannotBeApplied 

let closure_t = let open High_ollvm.Ez.Type in
                structure ~packed:true 
                  [ ptr (ptr (fn void [])); ptr i8; i8; i8
                  ; i32 ]

let rec is_generic = 
  function 
  | Generic _ -> true 
  | Fun ts    -> List.exists ts is_generic 
  | _         -> false

let rec to_ollvm ?(is_arg=true) = 
  let module T = High_ollvm.Ez.Type in
  function
  | Int           -> T.i32 
  | Bool | Unit   -> T.i1
  | Generic g     -> sprintf "Can't convert generic type %s to ollvm type." g 
                     |> failwith
  | Fun _         -> closure_t
  | Float         -> T.float
  | Array t       -> T.array 0 (to_ollvm t) |> T.ptr
  | String        -> T.ptr T.i8
  | Record fields -> List.map fields (snd %> to_ollvm)  
                     |> T.structure ~packed:true 
                     |> (if is_arg then T.ptr else identity)

let rec mangle_name = 
  function
  | Int  -> "i"
  | Bool -> "b"
  | Unit -> "u"
  | Generic g -> "g_" ^ g
  | Fun ts    -> "fun(" ^ BatString.concat "|" (List.map ts mangle_name) ^ ")"
  | Float     -> "f"
  | Array t   -> "arr_" ^ mangle_name t
  | Record fs -> 
    let fs = (List.map fs (fun (f, t) -> f ^ "_" ^ mangle_name t)) in 
    "rec(" ^ BatString.concat "|" fs ^ ")"
  | String    -> "s"