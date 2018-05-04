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

let of_annotations annots = Unit