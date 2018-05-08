open Core

module A = Lang_parsing.Ast

type lang_type = 
  | Unit
  | Int
  | Bool
  | Float 
  | String 
  | Array of lang_type
  | Fun of lang_type list
  [@@deriving show]

type t = lang_type
[@@deriving show]

exception UnsupportedType of string
exception WrongNumberOfApplyArguments
exception WrongTypeOfApplyArgument
exception ValueCannotBeApplied 

let of_annotation : A.type_annot option -> _ =
  let single =
    function
    | ["int"]           -> Int
    | ["bool"]          -> Bool
    | ["int"; "array"]  -> Array Int
    | ["()"] | ["unit"] -> Unit
    | other             -> BatString.concat " " other
                           |> UnsupportedType |> raise in
  function
  | None     -> Int
  | Some []  -> raise (UnsupportedType "<empty>")
  | Some [t] -> single t
  | Some ts  -> List.map ts single |> Fun

let merge ts = 
  function
  | Fun latter_ts -> Fun (ts @ latter_ts)
  | last_t        -> Fun (ts @ [last_t])

let apply fn_t arg_ts = 
  match fn_t, arg_ts with 
  | _     , []     -> fn_t 
  | Fun ts, arg_ts -> let cnt = List.length arg_ts in 
                      let before, after = List.split_n arg_ts cnt in 
                      begin 
                      match List.exists2 before arg_ts (<>) with 
                      | Ok false        -> Fun after
                      | Unequal_lengths -> raise WrongNumberOfApplyArguments
                      | Ok true         -> raise WrongTypeOfApplyArgument
                      end
  | _     , _      -> raise ValueCannotBeApplied

let to_ollvm = 
  let module T = High_ollvm.Ez.Type in
  function
  | Int         -> T.i32 
  | Bool | Unit -> T.i1
  | other       -> failwith "TODO to_ollvm"
