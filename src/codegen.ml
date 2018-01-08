open Ast
open Llvm
open Core 

type environment = { m : int }

let literal ctx = 
  function
  | Int i  -> const_int (i32_type ctx) i
  | Bool b -> const_int (i1_type ctx)  (BatBool.to_int b)
  | other  -> show_literal other |> sprintf "Unsupported literal: %s"
              |> failwith

let letexp ctx (name, args, fst_line, snd_line) = 
  let local = create_context () in 
  let builder = builder local in 
  
  ()

let main () = printf "<main>\n"