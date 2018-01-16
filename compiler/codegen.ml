open Ast
open Llvm
open Core
open BatPervasives

module StrMap = Map.Make(String)

type environment = {
    named_vals : llvalue StrMap.t
  }

let emptyEnv = { named_vals = StrMap.empty }

(* Converts type annotation to lltype *)
let annot_to_lltype ctx =
  let single_type =
    function
    | "int"  -> i32_type ctx
    | "bool" -> i1_type ctx
    | "()"   -> void_type ctx
    | other  -> sprintf "Unsupported type annotation: %s" other |> failwith
  in
  function
  | None     -> i1_type ctx
  | Some [t] -> single_type t
  | Some ts  -> let ret = List.last_exn ts |> single_type in
                function_type ret (Array.of_list_map ts ~f:single_type)

let gen_literal ctx =
  function
  | Int i  -> const_int (i32_type ctx) i
  | Bool b -> const_int (i1_type ctx) (BatBool.to_int b)
  | other  -> show_literal other |> sprintf "Unsupported literal: %s"
              |> failwith

let rec gen_infix_op env ctx builder op lhs rhs =
  match lhs, rhs with
  | Some lhs, Some rhs ->
    let lhs_val = gen_expr env ctx builder lhs in
    let rhs_val = gen_expr env ctx builder rhs in
    begin
      let build_fn, name =
        match op with
        | "+" -> build_add, "addtmp"
        | "-" -> build_sub, "subtmp"
        | "*" -> build_mul, "multmp"
        | "/" -> build_sdiv, "divtmp"
        | "=" -> build_icmp Icmp.Eq, "eqcmp"
        | other -> sprintf "Unsupported operator: %s" other |> failwith
      in build_fn lhs_val rhs_val name builder
    end
  | _, _ ->
    failwith "Operator is missing operands"

and gen_var env ctx var_name =
    match StrMap.find env.named_vals var_name with
    | Some v -> v
    | None   -> sprintf "Unbound variable %s" var_name
                |> failwith

and gen_if env ctx builder cond then_exp else_exp =
  (* TODO: Handle if without else_exp *)
  let (Some else_exp) = else_exp in
  (* let ll_false = const_int (i1_type ctx) 0 in *)
  let cond_val = gen_expr env ctx builder cond in
  let start_bb = insertion_block builder in
  let the_function = block_parent start_bb in
  let then_bb = append_block ctx "then" the_function in

  (* Emit 'then' value. *)
  position_at_end then_bb builder;
  let then_val = gen_expr env ctx builder then_exp in
  let new_then_bb = insertion_block builder in

  let else_bb = append_block ctx "else" the_function in
  position_at_end else_bb builder;

  let else_val = gen_expr env ctx builder else_exp in

  let new_else_bb = insertion_block builder in
  let merge_bb = append_block ctx "ifcont" the_function in
  position_at_end merge_bb builder;

  let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
  let phi = build_phi incoming "iftmp" builder in

  (* Return to the start block to add the conditional branch. *)
  position_at_end start_bb builder;
  ignore (build_cond_br cond_val then_bb else_bb builder);

  (* Set a unconditional branch at the end of the 'then' block and the
    * 'else' block to the 'merge' block. *)
  position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
  position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

  (* Finally, set the builder to the end of the merge block. *)
  position_at_end merge_bb builder;
  phi

and gen_expr env ctx builder =
  function
  | LitExp lit             -> gen_literal ctx lit
  | InfixOp (op, lhs, rhs) -> gen_infix_op env ctx builder op lhs rhs
  | IfExp (cond, then_exp, else_exp) ->
    gen_if env ctx builder cond then_exp else_exp
  | VarExp var_name        -> gen_var env ctx var_name

  | other -> show_expr other |> sprintf "Unsupported expression: %s" |> failwith

and gen_exprs env ctx builder =
  function
  | [] -> failwith "Let body can't be empty"
  | e::es -> let llval = gen_expr env ctx builder e in
             List.fold es ~init:llval ~f:(fun _ -> gen_expr env ctx builder)

and gen_letexp ctx curr_module ((name, ret_type), args, fst_line, body_lines) =
  let local = create_context () in
  let args = Array.of_list args in
  (* return type *)
  let ret = annot_to_lltype ctx ret_type in
  (* create argument types *)
  let arg_types =
    Array.map args (snd %> annot_to_lltype local) in
  let ftype = function_type ret arg_types in
  let fn = declare_function name ftype curr_module in
  (* name arguments for later use in let scope *)
  let env =
    Array.foldi (params fn) ~init:emptyEnv ~f:(
      fun i env arg ->
        let name = fst args.(i) in
        set_value_name name arg;
        let named_vals = StrMap.set env.named_vals ~key:name ~data:arg in
        { named_vals = named_vals }
      ) in
  let bb = append_block ctx "Entry" fn in
  let builder = builder local in
  (* params *)
  position_at_end bb builder;

  let body = Option.value body_lines ~default:[] in
  let body_exprs = match fst_line with Some l -> l::body | None -> body in
  let ret_val = gen_exprs env local builder body_exprs in

  build_ret ret_val builder |> ignore;
  fn

let gen_ast =
  let ctx = create_context () in
  let md = create_module ctx "TopLevel" in

  function
  | LetExp (e1, e2, e3, e4) -> gen_letexp ctx md (e1, e2, e3, e4)
  | other -> sprintf "Unsupported exp: %s" (show_expr other) |> failwith

let gen_prog = ()