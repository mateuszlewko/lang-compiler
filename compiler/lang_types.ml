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

let of_annotation env annotation =
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

let valid_replacements old_t new_t = replacements old_t new_t 
                                     |> List.dedup_and_sort 
                                     |> List.filter ~f:(uncurry (<>))

let unify_expr new_t (env : TAD.environment) (expr, et) =
  match valid_replacements et new_t with 
  | []   -> env, (expr, et)
  | subs -> let substitutions = List.fold subs ~init:env.substitutions 
                                  ~f:(fun m (u, v) -> 
                                        BatMultiMap.add u v m
                                        |> BatMultiMap.add v u) in 
            let env = { env with substitutions } in 
            env, (TAD.Substitute (subs, (expr, new_t)), new_t)

type _substitutions = (t * t) list 
[@@deriving show]

let show_subs substitutions = 
  BatMultiMap.enum substitutions
  |> BatList.of_enum
  |> show__substitutions

let rec find_concrete substitutions generic_t = 
  let rec dfs vis curr = 
    try BatSet.find curr vis |> ignore; vis, None 
    with Not_found ->
      let vis = BatSet.add curr vis in  

      let rec find_first vis =  
        function 
        | (Generic s)::subs -> 
          let vis, t = dfs vis s in 
          begin 
          match t with 
          | Some _ as t -> vis, t 
          | None        -> find_first vis subs 
          end
        | concrete::_ -> vis, Some concrete
        | []          -> vis, None in 

      let subs = BatMultiMap.find (Generic curr) substitutions in 

      if BatSet.is_empty subs 
      then vis, None 
      else find_first vis (BatSet.to_list subs) in 

  dfs BatSet.empty generic_t |> snd

    (* | Some (Generic t) 
      when t = generic_t -> None 
    | Some (Generic t)   -> find_concrete substitutions t 
    | Some concrete      -> Some concrete
    | None               -> None *)

let rec find_concrete_lt substitutions = 
  function 
  | Generic generic_t -> find_concrete substitutions generic_t 
    (* begin
    match BatMap.Exceptionless.find generic_t substitutions with 
    | Some (Generic _ as t) 
      when t = generic_t    -> None 
    | Some (Generic _ as t) -> find_concrete_lt substitutions t 
    | Some concrete         -> Some concrete
    | None                  -> None
    end  *)
  | other -> Some other

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
                                ~f:(fun m (u, v) -> BatMultiMap.add u v m) in 

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
