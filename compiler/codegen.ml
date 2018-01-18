open Ast
open Llvm
open Core
open BatPervasives

module StrMap = Map.Make(String)

type environment = {
  named_vals : llvalue StrMap.t;
  llmod      : llmodule;
  builder    : llbuilder;
  ctx        : llcontext
}

module Env =
struct
  let create module_name =
    let ctx = create_context () in
    { named_vals = StrMap.empty;
      ctx        = ctx;
      builder    = builder ctx;
      llmod      = create_module ctx module_name }

  let print env =
    printf "Env:\n";
    StrMap.iter_keys env.named_vals ~f:(printf "+ %s\n");
    printf "\n"
end

let skip_void_vals =
  Array.filter ~f:(type_of %> classify_type %> (<>) TypeKind.Void)

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
  | None     -> i32_type ctx
                (* TODO: This will probably be incorrect when I start
                         implementing currying *)
  | Some []  -> failwith "Empty type (??)"
  | Some [t] -> single_type t
  | Some ts  -> let ts, last_t = List.split_n ts (List.length ts - 1) in
                let ts = List.filter ts ((<>) "()") in
                let ret = single_type (List.hd_exn last_t) in
                function_type ret (Array.of_list_map ts ~f:single_type)

let gen_literal ctx =
  function
  | Int i  -> const_int (i32_type ctx) i
  | Bool b -> const_int (i1_type ctx) (BatBool.to_int b)
  | Unit   -> undef (void_type ctx)
  | other  -> show_literal other |> sprintf "Unsupported literal: %s"
              |> failwith

let rec gen_infix_op env op lhs rhs =
  match lhs, rhs with
  | Some lhs, Some rhs ->
    let lhs_val = gen_expr env lhs |> fst in
    let rhs_val = gen_expr env rhs |> fst in
    begin
(*
      if is_constant lhs_val
      then build_add (const_int (i32_type ctx 0)) lhs_val "" builder;
           set_value_name "rhs" rhs_val; *)
      let build_fn, name =
        match op with
        | "+" -> build_add, "add_tmp"
        | "-" -> build_sub, "sub_tmp"
        | "*" -> build_mul, "mul_tmp"
        | "/" -> build_sdiv, "div_tmp"
        | "=" -> build_icmp Icmp.Eq, "eq_cmp"
        | other -> sprintf "Unsupported operator: %s" other |> failwith
      in build_fn lhs_val rhs_val name env.builder
      (* Llvm.Attrib *)
    end
  | _, _ ->
    failwith "Operator is missing operands"

and gen_var env var_name =
    match StrMap.find env.named_vals var_name with
    | Some v -> v
    | None   -> sprintf "Unbound variable %s" var_name
                |> failwith

and gen_if env cond then_exp else_exp =
  let cond_val = gen_expr env cond |> fst in
  let start_bb = insertion_block env.builder in
  let parent   = block_parent start_bb in

  let emit_branch name exp =
    let bb = append_block env.ctx name parent in
    position_at_end bb env.builder;

    let llval = gen_expr env exp |> fst in
    let new_bb = insertion_block env.builder in
    llval, bb, new_bb
  in

  let (Some else_exp) = else_exp in
  let then_val, then_bb, new_then_bb = emit_branch "then" then_exp in
  let else_val, else_bb, new_else_bb = emit_branch "else" else_exp in
  let merge_bb = append_block env.ctx "if_cont" parent in

  if then_val |> type_of |> classify_type <> TypeKind.Void
  then (
    position_at_end merge_bb env.builder;

    let incoming = [then_val, new_then_bb; else_val, new_else_bb] in
    let phi = build_phi incoming "if_result" env.builder in

    (* Return to the start block to add the conditional branch. *)
    position_at_end start_bb env.builder;
    ignore (build_cond_br cond_val then_bb else_bb env.builder);

    (* Set a unconditional branch at the end of the 'then' block and the
      * 'else' block to the 'merge' block. *)
    position_at_end new_then_bb env.builder;
    ignore (build_br merge_bb env.builder);
    position_at_end new_else_bb env.builder;
    ignore (build_br merge_bb env.builder);

    (* Finally, set the builder to the end of the merge block. *)
    position_at_end merge_bb env.builder;
    phi
  )
  else (
    position_at_end start_bb env.builder;
    ignore (build_cond_br cond_val then_bb else_bb env.builder);

    position_at_end new_then_bb env.builder;
    ignore (build_br merge_bb env.builder);
    position_at_end new_else_bb env.builder;
    ignore (build_br merge_bb env.builder);

    (* Finally, set the builder to the end of the merge block. *)
    position_at_end merge_bb env.builder;
    undef (void_type env.ctx)
  )

and gen_letexp env (name, ret_type) args fst_line body_lines =
  (* let local = create_context () in *)
  let args = Array.of_list args in

  (* return type *)
  let ret = annot_to_lltype env.ctx ret_type in

  (* skip args of type unit *)
  let args = Array.filter args ~f:(snd %> (<>) (Some ["()"])) in

  (* create types for args *)
  let arg_types = Array.map args ~f:(snd %> annot_to_lltype env.ctx) in

  let ftype = function_type ret arg_types in
  let fn = declare_function name ftype env.llmod in
  (* printf "declw func %s\n " name; *)
   (* (string_of_llmodule env.llmod); *)

  (* name arguments for later use in let's scope *)
  let inner_env =
    Array.foldi (params fn) ~init:env ~f:(
      fun i env arg ->
        let name = fst args.(i) in
        set_value_name name arg;
        { env with named_vals = StrMap.set env.named_vals ~key:name ~data:arg }
      ) in

  let bb = append_block env.ctx "entry" fn in

  (* params *)
  let inner_env = { inner_env with builder = Llvm.builder_at_end env.ctx bb } in
  (* position_at_end bb env.builder; *)

  let body = Option.value body_lines ~default:[] in
  let body_exprs = Option.fold fst_line ~init:body ~f:(flip List.cons) in
  let ret_val = gen_exprs inner_env fn body_exprs |> fst in

  (* env extended with new binding to generated let *)
  let env_with_let = { env with
    named_vals = StrMap.set env.named_vals ~key:name ~data:fn
  } in

  (* generate body and return function definition *)
  (if ret_val |> type_of |> classify_type = TypeKind.Void
   then build_ret_void inner_env.builder
   else build_ret ret_val inner_env.builder)
  |> ignore;
  (* printf "env after let\n";
  Env.print env_with_let; *)
  fn, env_with_let

and gen_exprs env let_block =
  function
  | []    -> failwith "Let body can't be empty"
  | e::es ->
             (* block_parent let_block |> string_of_llvalue |> printf "parent block: %s\n"; *)
             (* position_at_end (let_block |> block_of_value) env.builder; *)
             let llval, env = gen_expr env e in
             List.fold es ~init:(llval, env)
                             ~f:(fun (_, env) ->
                                  (* position_at_end let_block env.builder; *)
                                  gen_expr env)

and gen_application env callee line_args rest_of_args =
  let args = line_args @ Option.value rest_of_args ~default:[] in
  let args_val =
    Array.of_list_map args ~f:(gen_expr env %> fst)
    |> skip_void_vals in

  let callee_val = gen_expr env callee |> fst in
  let ret_type_kind = callee_val |> type_of |> return_type
                      |> return_type |> classify_type in
  let name = if ret_type_kind = TypeKind.Void
             then "" else "call_tmp" in
  build_call callee_val args_val name env.builder

and gen_expr env =
  (* printf "env:\n";
  Env.print env;
  flush_all (); *)
  function
  | LetExp (e1, e2, e3, e4) ->
    gen_letexp env e1 e2 e3 e4
  | LitExp lit              -> gen_literal env.ctx lit, env
  | AppExp (callee, args, rest_of_args) ->
    gen_application env callee args rest_of_args, env
  | InfixOp (op, lhs, rhs)  -> gen_infix_op env op lhs rhs, env

  | IfExp (cond, then_exp, else_exp) ->
    gen_if env cond then_exp else_exp, env
  | VarExp var_name -> gen_var env var_name, env

  (* | other -> show_expr other |> sprintf "Unsupported expression: %s" |> failwith *)


and gen_extern env name tp =
  let ftype = annot_to_lltype env.ctx (Some tp) in
  let fn = declare_function name ftype env.llmod in
  let env = { env with
                named_vals = StrMap.set env.named_vals ~key:name ~data:fn } in
  fn, env

and gen_top_level env =
  function
  | Expr e            -> gen_expr env e
  | Extern (name, tp) -> gen_extern env name tp
  | other             -> show_top_level other
                         |> sprintf "Unsupported type level: %s" |> failwith

let gen_prog top_level_exprs =
  let env = Env.create "main" in
  let swap (x, y) = y, x in

  List.fold_map top_level_exprs ~init:env
    ~f:(fun env ->
      (* printf "curr env:\n";
      Env.print env; *)
      gen_top_level env %> swap)