open Core

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

exception UnsupportedType of string

let of_annotation annots =
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
  | Some ts  -> let ts, last_t = List.split_n ts (List.length ts - 1) in
                let is_unit t  = t = ["()"] || t = ["unit"] in

                let ts  = List.filter ts is_unit in
                let ret = single (List.hd_exn last_t) in
                List.map ts single |> Fun