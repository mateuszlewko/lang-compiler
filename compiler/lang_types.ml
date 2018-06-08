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
  |> Option.map ~f:(fst %> fst)

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

let rec find_concrete preferred substitutions generic_t = 
  let rec dfs vis alt curr = 
    try BatSet.find curr vis |> ignore; vis, None, alt 
    with Not_found ->
      let vis = BatSet.add curr vis in  

      let rec find_first alt vis =  
        function 
        | (Generic s)::subs -> 
          let alt    = 
            let s_pref   = BatMap.find_default 0 s preferred in
            let alt_pref = Option.value_map alt ~default:0 ~f:snd in 
            
            if s_pref < alt_pref
            then Some (Generic s, s_pref)
            else alt in 

          let vis, t, alt = dfs vis alt s in 
          begin 
          match t with 
          | Some _ as t -> vis, t, alt
          | None        -> find_first alt vis subs 
          end
        | concrete::_ -> vis, Some concrete, alt
        | []          -> vis, None         , alt in 

      match BatMap.find_default [] (Generic curr) substitutions with 
      | []   -> vis, None, alt
      | subs -> find_first alt vis subs in 

  match dfs BatSet.empty None generic_t with 
  | _, None, alt -> alt |> Option.map ~f:fst 
  | _, s   , _   -> s

let rec find_concrete_lt ?(preferred=BatMap.empty) substitutions = 
  function 
  | Generic generic_t -> find_concrete preferred substitutions generic_t 
  | other             -> Some other

let apply (env : TAD.environment) fn_t arg_ts = 
  let no_substitution t e = env, (e, t) in 

  match fn_t, arg_ts with 
  | _     , []     -> no_substitution fn_t 
  | Fun ts, arg_ts -> 
    let cnt           = List.length arg_ts in 
    let before, after = List.split_n ts cnt in 
    
    let substitution t = 
      match List.map2_exn before arg_ts replacements 
            |> List.concat 
            |> List.dedup_and_sort 
            |> List.filter ~f:(uncurry (<>))
      with 
      | []   -> no_substitution t 
      | subs -> 
        show__substitutions subs |> printf "adding subs: %s\n";

        let t = List.find subs (fst %> (=) t) 
                |> Option.map ~f:snd |> Option.value ~default:t in 

        fun expr -> 
          let substitutions = List.fold subs ~init:env.substitutions 
                                ~f:(fun m (u, v) -> add_equality u v m) in 

          { env with substitutions }, (TAD.Substitute (subs, (expr, t)), t) in 

    begin 
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
    end
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
