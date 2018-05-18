open Core
open BatPervasives

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

let apply fn_t arg_ts = 
  match fn_t, arg_ts with 
  | _     , []     -> fn_t 
  | Fun ts, arg_ts -> let cnt = List.length arg_ts in 
                      let before, after = List.split_n ts cnt in 
                      begin 
                      match List.exists2 before arg_ts (<>) with 
                      | Ok false        -> 
                        begin 
                        match after with 
                        | []  -> 
                          raise WrongNumberOfApplyArguments
                        | [t] -> t 
                        | ts  -> Fun ts
                        end
                      | Unequal_lengths -> raise WrongNumberOfApplyArguments
                      | Ok true         -> 
                        printf "fn_t: %s\n" (show_lang_type fn_t);
                        List.iter arg_ts (show_lang_type %> printf "arg: %s\n");
                        raise WrongTypeOfApplyArgument

                      end
  | _     , _      -> raise ValueCannotBeApplied

let closure_t = let open High_ollvm.Ez.Type in
                structure ~packed:true 
                  [ ptr (ptr (fn void [])); ptr i8; i8; i8
                  ; i32 ]

let rec to_ollvm = 
  let module T = High_ollvm.Ez.Type in
  function
  | Int         -> T.i32 
  | Bool | Unit -> T.i1
  | Fun _       -> closure_t
  | Float       -> T.float
  | Array t     -> T.array 0 (to_ollvm t) |> T.ptr
  | String      -> T.ptr T.i8
