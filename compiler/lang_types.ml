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

let of_annotation find_type =
  let single =
    function
    | ["int"]           -> Int
    | ["bool"]          -> Bool
    | ["int"; "array"]  -> Array Int
    | ["()"] | ["unit"] -> Unit
    | [t] when BatString.starts_with t "'" -> Generic t
    | [name]            -> 
      begin 
      match find_type name with 
      | Some t -> t 
      | None   -> UnsupportedType name |> raise
      end
    | other ->
      BatString.concat " " other
      |> UnsupportedType |> raise in

  function
  | None     -> Int
  | Some []  -> raise (UnsupportedType "<empty>")
  | Some [t] -> single t
  | Some ts  -> List.map ts single |> Fun

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

let rec replace old_t new_t =
  match old_t, new_t with 
  | Fun [t]           , other                -> replace t other 
  | old               , Fun [other]          -> replace old other
  | Generic _ as old_t, new_t                
  | (Fun _ as old_t)  , (Generic _ as new_t) -> [old_t, new_t]
  | Fun ts            , Fun new_ts           -> 
    let min_len = min (List.length ts  - 1) (List.length new_ts - 1) in 
    let ts    , ret_t    = List.split_n ts min_len in 
    let new_ts, ret_newt = List.split_n new_ts min_len in 
    (List.map2_exn ts new_ts replace
     |> List.concat) @ replace (Fun ret_t) (Fun ret_newt) 
  | old_t             , new_t                ->
    if old_t = new_t 
    then []
    else (printf "here\n"; raise WrongTypeOfApplyArgument)

let apply fn_t arg_ts = 
  let no_substitution t e = e, t in 

  match fn_t, arg_ts with 
  | _     , []     -> no_substitution fn_t 
  | Fun ts, arg_ts -> 
    let cnt           = List.length arg_ts in 
    let before, after = List.split_n ts cnt in 
    
    let substitution t = 
      match List.map2_exn before arg_ts replace |> List.concat with 
      | []   -> no_substitution t 
      | subs -> fun expr -> TAD.Substitute (subs, (expr, t)), t in 

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
