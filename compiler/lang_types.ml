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
  |> Option.map ~f:(fst3 %> fst3)

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
  | new_t             , (Generic _ as old_t) -> [old_t, new_t, true]
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
  if t1 = t2 
  then substitutions
  else let last = try BatMap.find t1 substitutions 
                  with Not_found -> [] in 
     
       BatMap.add t1 (t2::last) substitutions
       |> if both then add_equality ~both:false t2 t1 
                  else identity

let valid_replacements old_t new_t = replacements old_t new_t 
                                     |> List.dedup_and_sort 
                                     |> List.filter ~f:(fun (u, v, _) -> u <> v)

let unify_expr new_t (env : TAD.environment) (expr, et) =
  match valid_replacements et new_t with 
  | []   -> env, (expr, et)
  | subs -> let substitutions = List.fold subs ~init:env.substitutions 
                                  ~f:(fun m (u, v, both) -> 
                                      add_equality ~both u v m) in 
            let env = { env with substitutions } in 
            env, (TAD.Substitute (subs, (expr, new_t)), new_t)

type _substitutions = (t * t * bool) list 
[@@deriving show]

type _substitutions2 = (t * t list) list 
[@@deriving show]

let show_subs substitutions = 
  BatMap.bindings substitutions
  |> show__substitutions2

let rec is_generic = 
  function 
  | Generic _ -> true 
  | Fun ts    -> List.exists ts is_generic 
  | _         -> false

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

let just_drop_args n t = drop_args n BatSet.empty BatMap.empty t 

let rec take_arg = 
  function 
  | Fun (t::_::_) -> Some t 
  | Fun [t]       -> take_arg t
  | _             -> None 
  (* | other      -> Some other *)

let find_equalities subs = 
  let rec find_single vis curr =
    Logs.debug (fun m -> m "find single: %s\n" (show curr));

    if BatSet.mem curr vis 
    then vis, []
    else 
      let vis = BatSet.add curr vis in 
      let vis, ns = 
        BatMap.find_default [] curr subs 
        |> List.fold_map ~init:vis ~f:find_single in 
      vis, curr :: List.concat ns in 

  let rec add_subs subs = 
    function 
    | [] | [_]     -> subs 
    | x1::x2::rest -> 
      Logs.debug (fun m -> m "adding equality: %s %s\n" (show x1) (show x2));
      add_subs (add_equality x1 x2 subs) (x2::rest) in 

  let rec merge subs = 
    function 
    | []    -> subs 
    | equal -> 
      let args = List.map equal take_arg |> List.filter_opt in
      let rest = List.map equal (just_drop_args 1) |> List.filter_opt in 
      let subs = add_subs subs args in 
      let subs = add_subs subs equal in 
      merge subs rest in 

  let is_fun = function Fun _ -> true | _ -> false in 

  BatEnum.filter is_fun (BatMap.keys subs)
  |> BatList.of_enum
  |> List.fold ~init:subs ~f:(
      fun subs curr -> 
        Logs.debug (fun m -> m "finding for: %s\n" (show curr));

        let equal = find_single BatSet.empty curr |> snd in 
        Logs.debug (fun m -> m "got equal:\n");
        List.iter equal (show %> printf "%s\n");
        Logs.debug (fun m -> m "end\n");
        merge subs equal
    )

let shrink subs = BatMap.map List.stable_dedup subs 

let length t = 
  let rec count curr = 
    function 
    | Fun []      -> curr 
    | Fun [t]     -> count curr t 
    | Fun (t::ts) -> count (curr + 1) (Fun ts)
    | other       -> curr + 1 in 

  count 0 t 

let make_mostly_same ?(initial_sets) subs t = 
  let subs = find_equalities subs |> shrink in 
  let sets = 
    match initial_sets with 
    | Some sets -> ref sets 
    | None      -> ref BatMap.empty in  

  let rec find t =
    Logs.debug (fun m -> m "u-find: %s\n" (show t));

    let f = BatMap.find_default t t !sets in 
    if t <> f 
    then 
      let f = find f in 
      sets := BatMap.add t f !sets; 
      f
    else t in 

  let union t1 t2 = 
    let f1, f2 = find t1, find t2 in 
    if not (is_generic f1) || (is_generic f2 && length f1 > length f2)
    then sets := BatMap.add t2 f1 !sets |> BatMap.add f2 f1
    else sets := BatMap.add t1 f2 !sets |> BatMap.add f1 f2; 
    in 

  BatMap.iter (fun t ts -> List.iter ts (union t)) subs;

  let rec make = 
    function 
    | t when not (is_generic t) -> t 
    | Fun ts -> Fun (List.map ts make)
    | other  -> find other in 

  Logs.debug (fun m -> m "initial: %s\n" (show t));
  let res = make t |> make in 
  Logs.debug (fun m -> m "simplified: %s\n" (show res));
  !sets, res

let rec find_conc ?(find_eqs=false) subs curr =
  (* TODO: Improve this anyway *)

  let subs = 
    if find_eqs
    then find_equalities subs |> shrink 
    else subs |> shrink in 

  let res_subs = ref subs in 
  Logs.debug (fun m -> m "--------\n");

  let vis = ref BatMap.empty in 

  let rec find_first funs from curr =  
    match curr with 
    | x::xs ->
      let res = find funs from x in 
      begin 
      match res with 
      | None          -> find_first funs from xs 
      | Some _ as res -> 
        vis := BatMap.add x res !vis;
        res 
      end
    | []    -> None 

  and find funs prev curr : _ option =
    if not (is_generic curr)
    then (
      List.iter funs (show %> 
        fun s -> Logs.debug (fun m -> m "one of funs: %s\n" s));
      
      vis := 
        List.fold funs ~init:!vis 
          ~f:(fun m f -> BatMap.add f (Some curr) m);

      Logs.debug (fun m -> m "found: %s\n" (show curr));

      BatMap.iter (fun k -> 
        function Some v -> 
                Logs.debug (fun m -> m "vis %s -> some %s\n" (show k) (show v)) 
               | None   -> 
                Logs.debug (fun m -> m "vis %s -> none\n" (show k))) !vis;

      vis := (match prev with Some prev -> BatMap.add prev (Some curr) !vis 
                           | None      -> !vis);

      Some curr 
    )
    else if BatMap.mem curr !vis 
    then 
      begin 

      Logs.debug (fun m -> m "curr A: %s\n" (show curr));

      BatMap.iter (fun k -> 
        function Some v -> Logs.debug (fun m -> 
                              m "s vis %s -> some %s\n" (show k) (show v))
               | None   -> Logs.debug (fun m -> 
                              m "s vis %s -> none\n" (show k))) !vis;

      match BatMap.find curr !vis with 
      | Some _ as res -> res 
      | None          -> 
        let ns = BatMap.find_default [] curr subs |> List.stable_dedup
                 |> List.map ~f:(flip (BatMap.find_default None) !vis) in 
        
        List.find ns Option.is_some |> Option.value ~default:None  
      end
    else 
      begin 
      printf "find: %s\n" (show curr);

      vis := BatMap.add curr None !vis;

      let add_curr res = 
        if Option.is_some res 
        then begin 
          vis := BatMap.add curr res !vis;
          res end
        else res in 

      let check_ns funs curr = 
        let ns = BatMap.find_default [] curr subs |> List.stable_dedup in
        find_first funs (Some curr) ns |> add_curr in 
    
      match curr with 
      | Fun [t]             -> find funs (Some curr) t |> add_curr
      | Fun (t::ts) as fn_t -> 
        let res = check_ns (fn_t::funs) fn_t in 

        begin 
        match res with 
        | Some _ as res -> add_curr res
        | None          -> 
          let funs1 = t :: (BatList.map take_arg funs |> List.filter_opt) in 
          let funs2 = (Fun ts) :: 
                      (BatList.map (drop_args 1 BatSet.empty BatMap.empty) funs
                       |> List.filter_opt) in 

          let res1 = find funs1 None t in 
          let res  = find funs2 None (Fun ts) in 
          begin 
          match res1, res with 
          | Some res1, Some (Fun res) -> 
            let res = Some (Fun (res1::res)) in 
            vis := BatMap.add curr res !vis; 
            res 
          | Some res1, Some res       -> 
            let res = Some (Fun (res1::[res])) in 
            vis := BatMap.add curr res !vis;
            res 
          | _        , _              -> None 
          end 
        end 
      | Generic _ as g -> check_ns funs g 
      | concrete       -> assert false
      end
    in 
  
  let t = find [] None curr in 
  BatMap.iter (fun k -> 
    function Some v -> res_subs := add_equality k v !res_subs
           | None   -> ()) !vis;

  !res_subs |> shrink, t

(* 
let rec find_concrete substitutions generic_t = 
  let rec dfs vis curr = 
    try BatSet.find curr vis |> ignore; vis, None 
    with Not_found ->
      printf "curr: %s\n" curr;

      let vis = BatSet.add curr vis in  

      let rec find_first vis =  
        function 
        | (Generic s)::subs -> 
          let vis, t = dfs vis s in 
          begin 
          match t with 
          | Some _ as t -> 
            vis, t
          | None        -> find_first vis subs 
          end
        | (Fun ts as fn_t)::subs when is_generic fn_t -> 
          let vis, ts = List.fold_map ts ~init:vis ~f:dfs in 

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
  | other             -> Some other *)

let find_concrete_lt ?(find_eqs=false) substitutions curr  =
  find_conc ~find_eqs substitutions curr 

let rec apply (env : TAD.environment) fn_t arg_ts = 
  let no_substitution t e = env, (e, t) in 
  
  (* let t_as_fun gen_t arg_ts = fun expr ->
    let env, ret_t     = TAD.fresh_type env in 
    let fn_t           = Fun (arg_ts @ [ret_t]) in 
    
    (* printf "new fn_t is: %s\n" (show fn_t); *)
    let (env : TAD.environment), (expr, t) = apply env fn_t arg_ts expr in 
   
    let substitutions  = add_equality gen_t fn_t env.substitutions in 
    let env            = { env with substitutions } in

   env, (TAD.Substitute ([gen_t, fn_t, true], (expr, t)), t) in *)
  
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
          |> List.filter ~f:(fun (u, v, _) -> u <> v)
    with 
    | []   -> no_substitution fn_t
    | subs -> 
      show__substitutions subs 
      |> fun s -> Logs.debug (fun m -> m "adding subs: %s\n" s);

      (* let t = List.find subs (fst %> (=) fn_t) 
              |> Option.map ~f:snd |> Option.value ~default:fn_t in  *)
      let cnt    = List.length arg_ts in 
      let subs_m = subs |> BatList.map (fun (u, v, _) -> u, v) |> BatList.enum 
                        |> BatMap.of_enum in
      let t      = BatOption.get_exn (drop_args cnt BatSet.empty subs_m fn_t)
                                      WrongNumberOfApplyArguments in

      fun expr -> 
        let substitutions = List.fold subs ~init:env.substitutions 
                              ~f:(fun m (u, v, both) -> 
                                    add_equality ~both u v m) in 

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