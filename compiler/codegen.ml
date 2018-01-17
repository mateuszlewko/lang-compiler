open Ast
open Llvm
open Core
open BatPervasives

module StrMap = Map.Make(String)

type environment = { named_vals : llvalue StrMap.t }

let kind_to_str =
  function
  | TypeKind.Function -> "func"
  | TypeKind.Integer -> "int"
  | TypeKind.Void -> "void"
  | TypeKind.Pointer -> "ptr"

module Env =
struct
  let empty = { named_vals = StrMap.empty }

  let print env =
    StrMap.iter_keys env.named_vals ~f:(printf "key: %s\n");
    printf "\n"
end

(* type const_val = Val of llvalue | Void *)

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

let rec gen_infix_op env ctx builder op lhs rhs =
  match lhs, rhs with
  | Some lhs, Some rhs ->
    let lhs_val = gen_expr env ctx builder lhs |> fst in
    let rhs_val = gen_expr env ctx builder rhs |> fst in
    begin
      if is_constant lhs_val || is_constant rhs_val
         && (not (is_constant lhs_val && is_constant rhs_val))
      then set_value_name "lhs" lhs_val;
           set_value_name "rhs" rhs_val;
      let build_fn, name =
        match op with
        | "+" -> build_add, "add_tmp"
        | "-" -> build_sub, "sub_tmp"
        | "*" -> build_mul, "mul_tmp"
        | "/" -> build_sdiv, "div_tmp"
        | "=" -> build_icmp Icmp.Eq, "eq_cmp"
        | other -> sprintf "Unsupported operator: %s" other |> failwith
      in build_fn lhs_val rhs_val name builder
      (* Llvm.Attrib *)
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
  let cond_val = gen_expr env ctx builder cond |> fst in
  let start_bb = insertion_block builder in
  let the_function = block_parent start_bb in
  let then_bb = append_block ctx "then" the_function in

  (* Emit 'then' value. *)
  position_at_end then_bb builder;
  let then_val = gen_expr env ctx builder then_exp |> fst in
  let new_then_bb = insertion_block builder in

  let else_bb = append_block ctx "else" the_function in
  position_at_end else_bb builder;

  let else_val = gen_expr env ctx builder else_exp |> fst in

  let new_else_bb = insertion_block builder in
  let merge_bb = append_block ctx "if_cont" the_function in
  position_at_end merge_bb builder;

  let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
  let phi = build_phi incoming "if_tmp" builder in

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

and gen_letexp env ctx builder curr_module
               ((name, ret_type), args, fst_line, body_lines) =
  let local = create_context () in
  let args = Array.of_list args in

  (* return type *)
  let ret = annot_to_lltype ctx ret_type in

  (* skip args of type unit *)
  let args = Array.filter args ~f:(snd %> (<>) (Some ["()"])) in

  (* create types for args *)
  let arg_types = Array.map args ~f:(snd %> annot_to_lltype local) in

  let ftype = function_type ret arg_types in
  let fn = declare_function name ftype curr_module in

  (* name arguments for later use in let scope *)
  let inner_env =
    Array.foldi (params fn) ~init:env ~f:(
      fun i env arg ->
        let name = fst args.(i) in
        set_value_name name arg;
        let named_vals = StrMap.set env.named_vals ~key:name ~data:arg in
        { named_vals = named_vals }
      ) in

  let bb = append_block ctx "Entry" fn in
  let builder = Llvm.builder local in

  (* params *)
  position_at_end bb builder;

  let body = Option.value body_lines ~default:[] in
  let body_exprs = Option.fold fst_line ~init:body ~f:(flip List.cons) in
  let ret_val = gen_exprs inner_env local builder body_exprs |> fst in

  (* env extended with new binding to generated let *)
  let env_with_let = {
    named_vals = StrMap.set env.named_vals ~key:name ~data:fn
  } in

  (* generate body and return function definition *)
  (if ret_val |> type_of |> classify_type = TypeKind.Void
   then build_ret_void builder
   else build_ret ret_val builder)
  |> ignore;
  fn, env_with_let

and gen_application env ctx builder (callee, line_args, rest_of_args) =
  let args = line_args @ Option.value rest_of_args ~default:[] in
  let args_val =
    Array.of_list_map args ~f:(gen_expr env ctx builder %> fst)
    |> skip_void_vals in

  let callee_val = gen_expr env ctx builder callee |> fst in
  let ret_type_kind = callee_val |> type_of |> return_type
                      |> return_type |> classify_type in
  let name = if ret_type_kind = TypeKind.Void
             then "" else "call_tmp" in
  build_call callee_val args_val name builder

and gen_expr env ctx builder expr =
  match expr with
  | LetExp (e1, e2, e3, e4) ->
    let md = create_module ctx "LetModule" in
    gen_letexp env ctx builder md (e1, e2, e3, e4)
  | LitExp lit              -> gen_literal ctx lit, env
  | AppExp (callee, args, rest_of_args) ->
    gen_application env ctx builder (callee, args, rest_of_args), env
  | InfixOp (op, lhs, rhs)  -> gen_infix_op env ctx builder op lhs rhs, env

  | IfExp (cond, then_exp, else_exp) ->
    gen_if env ctx builder cond then_exp else_exp, env
  | VarExp var_name -> gen_var env ctx var_name, env

  (* | other -> show_expr other |> sprintf "Unsupported expression: %s" |> failwith *)

and gen_exprs env ctx builder =
  function
  | []    -> failwith "Let body can't be empty"
  | e::es -> let llval = gen_expr env ctx builder e |> fst in
             List.fold es ~init:(llval, env)
                             ~f:(fun (_, env) -> gen_expr env ctx builder)

and gen_extern env ctx builder main_mod name tp =
  (* let tp = List.filter tp ((<>) "()") in *)
  let ftype = annot_to_lltype ctx (Some tp) in
  let fn = declare_function name ftype main_mod in
  let env = { named_vals = StrMap.set env.named_vals ~key:name ~data:fn } in
  fn, env

and gen_top_level env ctx builder main_mod =
  function
  | Expr e            -> gen_expr env ctx builder e
  | Extern (name, tp) -> gen_extern env ctx builder main_mod name tp
  | other             -> show_top_level other
                         |> sprintf "Unsupported type level: %s" |> failwith

let gen_prog top_level_exprs =
  let ctx = create_context () in
  let builder = builder ctx in
  let main_mod = create_module ctx "TopLevel" in
  let env = Env.empty in
  let swap (x, y) = y, x in

  List.fold_map top_level_exprs ~init:env
    ~f:(fun env ->
      (* printf "curr env:\n";
      Env.print env; *)
      gen_top_level env ctx builder main_mod %> swap)

  (* |> List.last_exn *)

  (* function
  | LetExp (e1, e2, e3, e4) -> gen_letexp ctx md (e1, e2, e3, e4)
  | other -> sprintf "Unsupported exp: %s" (show_expr other) |> failwith *)

(* let gen_prog = () *)