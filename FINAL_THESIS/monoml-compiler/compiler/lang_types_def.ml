type lang_type = 
  | Unit
  | Int
  | Bool
  | Float 
  | String 
  | Array   of lang_type
  | Fun     of lang_type list
  | Generic of string
  (* | Mono    of string * lang_type option ref *)
  | Record  of (string * lang_type) list (* TODO: possibly store record name *)
  [@@deriving show]

type t = lang_type
[@@deriving show]
