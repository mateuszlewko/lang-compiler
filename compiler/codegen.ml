open Ast
open Llvm
open Core 
open BatPervasives

type environment = { m : int }

let gen_literal ctx = 
  function
  | Int i  -> const_int (i32_type ctx) i
  | Bool b -> const_int (i1_type ctx) (BatBool.to_int b)
  | other  -> show_literal other |> sprintf "Unsupported literal: %s"
              |> failwith

let gen_expr ctx expr = 
  (* TODO *)
  gen_literal ctx (Int 42) 
  
let gen_exprs ctx exprs = 
  (* TODO *)
  gen_literal ctx (Int 42) 

(* Converts type annotation to lltype *)
let annot_to_lltype = 
  function 
  | None | Some "int" -> i32_type
  | Some "bool"       -> i1_type
  | Some other        -> sprintf "Unsupported type annotation: %s" other 
                         |> failwith

let gen_letexp ctx curr_module ((name, ret_type), args, fst_line, body_lines) = 
  let local = create_context () in 
  let ret = (annot_to_lltype ret_type) ctx in
  let args = Array.of_list_map args (snd %> annot_to_lltype %> (|>) local) in
  let ft = function_type ret args in 
  let fn = declare_function name ft curr_module in 
  let bb = append_block ctx "Entry" fn in 
  let builder = builder local in 

  position_at_end bb builder;

  let body = Option.value body_lines ~default:[] in 
  let body_exprs = Option.map fst_line (flip List.cons body) in
  let ret_val = gen_exprs local body_exprs in 

  build_ret ret_val builder |> ignore;
  fn 

let gen_ast = 
  let ctx = create_context () in 
  let md = create_module ctx "TopLevel" in 

  function 
  | LetExp (e1, e2, e3, e4) -> gen_letexp ctx md (e1, e2, e3, e4)
  | other -> sprintf "Unsupported exp: %s" (show_expr other) |> failwith