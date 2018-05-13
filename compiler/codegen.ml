open Lang_parsing.Ast
open Llvm
open Core
open BatPervasives
open BatString
open Codegen_utils

module Codegen = struct 
  open High_ollvm.Ez.Value
  open High_ollvm.Ez.Instr
  open High_ollvm.Ez.Block
  open High_ollvm
  (* open Lang_types *)

  module Env = Envn (* temporary *)
  module M   = High_ollvm.Ez.Module
  module T   = High_ollvm.Ez.Type
  module TA  = Typed_ast
  module LT  = Lang_types

  let unsupp ?(name="") str = sprintf "Unsupported %s: %s" name str |> failwith

  (* type result = 
    { env   : Env.environment
    ; value : Ez.Value.t
    ; t     : LT.t
    } [@@deriving fields] *)

  type result_type = Instr of Ast.instr | Block of Ez.Block.block

  let result_to_blocks m res = 
    let rec merge m blocks = 
      function 
      | []    , [] -> m, List.rev blocks 
      | instrs, [] -> 
        let m, b = M.local m T.label "instrs" in 
        m, (List.rev instrs |> block b)::blocks |> List.rev
      | instrs, (Instr i)::xs -> merge m blocks (i::instrs, xs)
      | []    , (Block b)::xs -> merge m (b::blocks) ([], xs)
      | instrs, (Block b2)::xs -> 
        let m, b1 = M.local m T.label "instrs" in 
        merge m (b2::(List.rev instrs |> block b1)::blocks) ([], xs)
      in 

    merge m [] ([], res) 
  
  let gen_literal env expr = 
    function 
    | TA.Int   i    -> [], i32 i, env
    | Bool  b       -> [], i1 (BatBool.to_int b), env
    | Array xs -> failwith "TODO array"
        (* let elems, res = List.fold_map xs 
        List.map xs (expr env %> snd) |> array *)
    | Unit          -> [], i1 0, env
    | other         -> unsupp ~name:"literal" (TA.show_literal other)

  let gen_op (env : Envn.t) expr lhs rhs op = 
    match lhs, rhs with
    | Some lhs, Some rhs ->
      let res1, lhs, _ = expr env lhs in
      let res2, rhs, _ = expr env rhs in
      let res          = res1 @ res2 in 
      let m, v         = M.tmp env.m in
      
      (match op with
      (* operators returning int *)
      | "+"   -> add  lhs rhs
      | "-"   -> sub  lhs rhs
      | "*"   -> mul  lhs rhs
      | "/"   -> sdiv lhs rhs
      (* operators returning bool *)
      | "="   -> eq   lhs rhs
      | "<"   -> slt  lhs rhs
      | "<="  -> sle  lhs rhs
      | ">"   -> sgt  lhs rhs
      | ">="  -> sge  lhs rhs
      | "<>"  -> ne   lhs rhs
      | "&&"  -> and_ lhs rhs
      | "||"  -> or_  lhs rhs
      (* raise when operator is unknown *)
      | other -> unsupp ~name:"operator" other)
      |> fun op_res -> res @ [Instr (v <-- op_res)], v, { env with m }
    | _, _ ->
      failwith "Operator is missing operands"
  
  let gen_let (env : Env.t) expr letexp ts = 
    match ts with 
    | LT.Fun ts as fn_t -> 
      let { TA.args = ta_args; name; is_rec; body } = letexp in 

      let args_cnt = List.length ta_args in 
      let ret      = List.drop ts args_cnt in 
      let is_fun = function Some LT.Fun _ -> true | _ -> false in
      
      let ret_t = if List.length ret > 1 || is_fun (List.hd ret)
                  then Letexp.closure_t else LT.to_ollvm (List.hd_exn ret) in
      
      let m, fn = M.global env.m ret_t name in
      let to_local t = LT.to_ollvm t, "" in 
      let fn_args    = List.take ts (List.length ts - 1) in
      let fn_args_named = 
        List.mapi fn_args (fun i a -> if i < args_cnt 
                                      then fst (List.nth_exn ta_args i), a
                                      else "", a) in 
      let typed_args = List.map fn_args to_local in 
      let m, args = M.batch_locals m typed_args in
      
      let full_args =
        let ts = List.take ts (List.length ts - 1) in 
        List.mapi ts (fun i t -> LT.to_ollvm t, sprintf "arg-%d" i) in 
    
      let m, fns_arr = 
        if List.length ret > 1 || is_fun (List.hd ret)
        then (* returns closure *)
          Letexp.closure_entry_fns env.m name full_args args_cnt fn 
        else (* returns value *)
          let ret = List.hd_exn ret |> LT.to_ollvm in 
          Letexp.value_entry_fns env.m name ret full_args fn in 

      let env = { env with m } in
      let body_env : Env.t = 
        (if is_rec then Env.add env name (Fun ((fn, fn_t), fns_arr)) else env)
        |> fun env -> List.fold2_exn args fn_args_named ~init:env 
                           ~f:(fun env v (name, t) -> 
                                  Env.add env name (Val (v, t))) in 
    
      let iss, values, _ = List.map body (expr body_env) |> List.unzip3 in 
    
      let iss       = List.concat iss in 
      let ret_v     = List.last_exn values in 
      let ret_i     = Ez.Instr.ret ret_v |> Instr in
      let m, blocks = result_to_blocks env.m (iss @ [ret_i]) in 
      let df        = define fn args blocks in
      let env = { env with m = M.definition m df } in 

      printf "add %s to env.m\n" name;
      
      let env = Env.add env name (Fun ((fn, fn_t), fns_arr)) in env
    | other -> failwith "TODO let-value"

  let gen_apply (env : Env.t) expr callee args app_t = 
    let args = List.map args (expr env %> snd3) in

    match callee with 
    | TA.Var name, t -> 
      begin 
      match Env.find env name with
      | Fun ((fn, fn_t), fns_arr) -> 
        let fn_arg_ts = match fn_t with 
                        | Fun ts -> List.take ts (List.length ts - 1)   
                                    |> List.map ~f:LT.to_ollvm 
                        | _      -> [] in

        let m, instrs, res = 
          Letexp.known_apply env.m args fn_arg_ts fn fns_arr in
        List.map instrs (fun x -> Instr x), res, { env with m }
      | Val (v , t   ) -> failwith "unknown apply 1"
      end
    | v -> 
      let iss, callee, _ = expr env v in 
      match app_t with 
      | LT.Fun app_ts -> 
        let m, sink_b = M.local env.m T.label "sink_b" in 
        let m, res    = M.tmp m in 
        let m, app_iss, blocks, phi_i = 
          Letexp.closure_apply env.m callee args sink_b in 

        let sink_b  = block sink_b [res <-- phi_i] in
        let app_iss = List.map app_iss (fun x -> Instr x) in 
        let blocks  = List.map (blocks @ [sink_b]) (fun x -> Block x) in
        
        iss @ app_iss @ blocks, res, { env with m }
      | app_t         -> failwith "unknown apply 2"

  let rec gen_expr env = 
    function 
    | TA.Var v, _               -> [], Env.find_val env v, env
    | Lit    l, _               -> gen_literal env gen_expr l 
    | Let _   , _               -> failwith "nested let here TODO"
    | App (callee, args), t     -> gen_apply env gen_expr callee args t 
    | InfixOp (op, lhs, rhs), _ -> gen_op env gen_expr lhs rhs op
    | If    _, _                -> failwith "TODO if-expr"
    | Exprs _, _                -> failwith "TODO exprs"

  (* let gen_module *)

  let gen_top env =
    function 
    | TA.Expr (TA.Let letexp, ts) -> 
      gen_let env gen_expr letexp ts 
    | other -> sprintf "NOT SUPPORTED top of: %s" (TA.show_top other)
               |> failwith

  let gen_prog ?(module_name="<stdin>") top_lvl_exprs =
    let env = TA.of_tops top_lvl_exprs 
              |> List.fold ~init:Env.empty ~f:gen_top in  

    (* printf "module: %s" (Ast.show_modul env.m.m_module); *)
    
    let llenv = LLGate.ll_module env.m.m_module in 
    llenv.m

  (* later:
      - gen_extern
      - gen_mod_open
      - gen_mod_decl
      - gen_top_value *)
end 

let rec gen_literal env =
  let array_lit xs =
    if List.exists xs ~f:(function LitExp (Int _) -> false | _ -> true)
    then failwith "Only arrays of integers are currently supported";
   
    let elems =
      Array.of_list_map xs ~f:(function LitExp x -> x | _ -> assert false)
      |> Array.map ~f:(gen_literal env) in
    let arr = const_array (i32_type env.ctx) elems in
    let arr_ptr = build_malloc (type_of arr) "malloc_tmp" env.builder in
    build_store arr arr_ptr env.builder |> ignore;
    build_pointercast arr_ptr array_ptr "exp_ptr32" env.builder
  in

  function
  | Int i    -> Const.g_i32 env.builder env.llmod i
  | Int8 i   -> Const.g_i8 env.builder env.llmod i
  | Bool b   -> const_int (i1_type env.ctx) (BatBool.to_int b)
  | Array xs -> array_lit xs
  | Unit     -> undef_val
  | other    -> show_literal other |> sprintf "Unsupported literal: %s"
                |> failwith

let rec gen_infix_op env op lhs rhs =
  match lhs, rhs with
  | Some lhs, Some rhs ->
    let lhs_val = gen_expr env lhs |> fst in
    let rhs_val = gen_expr env rhs |> fst in
    begin
      let build_fn, name =
        match op with
        (* operators returning int *)
        | "+"   -> build_add , "add_tmp"
        | "-"   -> build_sub , "sub_tmp"
        | "*"   -> build_mul , "mul_tmp"
        | "/"   -> build_sdiv, "div_tmp"
        (* operators returning bool *)
        | "="   -> build_icmp Icmp.Eq , "eq_cmp"
        | "<"   -> build_icmp Icmp.Slt, "slt_cmp"
        | "<="  -> build_icmp Icmp.Sle, "sle_cmp"
        | ">"   -> build_icmp Icmp.Sgt, "sgt_cmp"
        | ">="  -> build_icmp Icmp.Sge, "sgt_cmp"
        | "<>"  -> build_icmp Icmp.Ne , "ne_cmp"
        | "&&"  -> build_and, "and_cmp"
        | "||"  -> build_or , "and_cmp"
        (* raise when operator is unknown *)
        | other -> sprintf "Unsupported operator: %s" other |> failwith
      in build_fn lhs_val rhs_val name env.builder
    end
  | _, _ ->
    failwith "Operator is missing operands"

and gen_if_with_elif env cond then_exp elif_exps else_exp =
  let rec nest_elif_exps =
    function
    | [] -> else_exp
    | (cond_exp, then_exp)::els ->
      Some (IfExp (cond_exp, then_exp, [], nest_elif_exps els))
  in gen_simple_if env cond then_exp (nest_elif_exps elif_exps)

and gen_raw_if env cond then_exp else_exp =
  let cond_val = gen_expr env cond |> fst in
  let start_bb = insertion_block env.builder in
  let parent   = block_parent start_bb in

  let emit_branch name exp =
    let bb = append_block env.ctx name parent in
    position_at_end bb env.builder;

    let llval  = gen_expr env exp |> fst in
    let new_bb = insertion_block env.builder in
    llval, bb, new_bb
  in

  let then_val, then_bb, new_then_bb = emit_branch "then" then_exp in
  let res_is_void = kind_of then_val = TypeKind.Void in

  match else_exp with
  | Some else_exp ->
    let else_val, else_bb, new_else_bb = emit_branch "else" else_exp in
    let merge_bb = append_block env.ctx "if_cont" parent in

    let result =
      if not res_is_void
      then (
        position_at_end merge_bb env.builder;
        let incoming = [then_val, new_then_bb; else_val, new_else_bb] in
        build_phi incoming "if_result" env.builder
      )
      else undef_val
    in

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
    then_bb, Some else_bb, merge_bb, result
  | None when not res_is_void ->
    failwith "If expression needs an else branch or must return unit."
  | None ->
    let merge_bb = append_block env.ctx "if_cont" parent in

    position_at_end start_bb env.builder;
    ignore (build_cond_br cond_val then_bb merge_bb env.builder);

    position_at_end new_then_bb env.builder;
    ignore (build_br merge_bb env.builder);

    position_at_end merge_bb env.builder;
    then_bb, None, merge_bb, undef_val

and gen_simple_if env cond then_exp else_exp =
  let _, _, _, res = gen_raw_if env cond then_exp else_exp in res

and gen_letexp env is_rec (name, ret_type_raw) args_raw fst_line body_lines =
  let args = Array.of_list args_raw in
  let is_val = Array.is_empty args in
  if is_rec && is_val
  then failwith "Can't define recursive value. Either remove 'rec' from let \
                 or add at least one argument.";

  (* return type *)
  let ret_type = annot_to_lltype env.ctx ~func_as_ptr:true ret_type_raw in
  (* printf "ret type of %s is %s\n" name (string_of_lltype ret_type); *)
  (* skip args of type unit *)
  let args = Array.filter args ~f:(snd %> (<>) (Some [["()"]])) in
  (* create types for args *)
  let arg_types =
    Array.map args ~f:(snd %> annot_to_lltype env.ctx ~func_as_ptr:true) in

  let ftype   = function_type ret_type arg_types in
  let fn_name = Env.name_of env name in
  let fn      = define_function fn_name ftype env.llmod in
  let bb      = entry_block fn in

  (* create new builder for body *)
  let body_env = { env with builder  = Llvm.builder_at_end env.ctx bb
                          ; top_vals = [] } in

  (* name arguments for later use in let's scope *)
  let body_env =
    Array.foldi (params fn) ~init:body_env ~f:(
      fun i env arg ->
        let name = fst args.(i) in
        set_value_name name arg;

        let null = const_null (type_of arg) in
        let g_arg = define_global ("g_arg_" ^ name) null env.llmod in
        build_store arg g_arg env.builder |> ignore;
        Env.add_var env name g_arg ~of_ptr:true
      )
    |> fun env ->
        if is_rec (* add binding to currently created let inside body *)
        then Env.add_var env name fn
        else env
  in
  let body       = Option.value body_lines ~default:[] in
  let body_exprs = Option.fold fst_line ~init:body ~f:(flip List.cons) in

  (* build body and return value of last expression as a value of let *)
  let ret_val, body_res_env = gen_exprs body_env body_exprs in
  (* env extended with new binding to generated let *)
  let ret_is_void = kind_of ret_val = TypeKind.Void in
  let env_with_let, expr_result =
    if not is_val
    then Env.add_var env name fn, fn
    else
      let mod_name = Env.name_of env name in
      let g_var =
        if ret_is_void
        then None
        else
          let null = const_null ret_type in
          Some (define_global (mod_name ^ "_val") null env.llmod)
      in

      let top_val = {ll = fn; of_ptr = false}
                  , g_var |> Option.map ~f:(fun v -> {ll = v; of_ptr = true}) in
      let env     = { env with top_vals = top_val::env.top_vals } in
      let res_var = Option.value g_var ~default:undef_val in
      (* Generate call to let-value and store result.
         This won't generate any code for top-level let (which is fine). *)
      let ret_name = if Option.is_some g_var then "ret" else "" in
      let ret_val  = build_call fn [||] ret_name env.builder in
      Option.iter g_var (fun g_var -> build_store ret_val g_var env.builder
                                      |> ignore);

      Env.add_var env name ~of_ptr:true res_var, res_var
  in

  (* generate body and return function definition *)
  let _ = if ret_is_void
          then build_ret_void body_env.builder
          else build_ret ret_val body_env.builder in

  if Array.length args > 1 && kind_of ret_val <> TypeKind.Pointer
  then begin 
    (* Letexp.gen_pre_fun env is_rec (name, ret_type) args_raw body_exprs fn
                       gen_raw_if;  *)
    let open High_ollvm.Ez in 
    let open High_ollvm.Ez.Block in 
    let m = Module.empty in
    let ret_t : High_ollvm.Ast.raw_type = annot_to_ho_type ret_type_raw in
    let args_t = 
                 Array.map args (snd %> annot_to_ho_type ~fn_ptr:(true)) 
                 |> Array.to_list in
    let m, raw_fn = Module.global m ret_t fn_name in
    let decl = declare raw_fn args_t in
    let m = M.declaration m decl in
    let args = Array.to_list args @ ["d", None; "e", None] in
    if String.contains fn_name 'V'
    then (); (*Letexp.value_entry_fns m env_with_let fn_name ret_t args raw_fn;*)
    let m = M.declaration m decl in
    let arity = 3 in
    if String.contains fn_name 'C' 
    then () (*Letexp.closure_entry_fns m env_with_let fn_name args arity raw_fn *)
  end;

  expr_result, env_with_let

and gen_exprs env =
  function
  | []    -> failwith "Let body can't be empty"
  | e::es -> let llval, env = gen_expr env e in
             List.fold es ~init:(llval, env) ~f:(fun (_, env) -> gen_expr env)

and gen_application env callee line_args rest_of_args =
  let args     = line_args @ Option.value rest_of_args ~default:[] in
  let args_val = Array.of_list_map (List.rev args) ~f:(gen_expr env %> fst)
                 >>* Array.rev_inplace |> skip_void_vals in

  let callee_val = gen_expr env callee |> fst in
  
  let ret_type_kind = callee_val |> type_of |> return_type |> return_type
                      |> classify_type in
  let name = if ret_type_kind = TypeKind.Void
             then "" else "call_tmp" in
  (* let open High_ollvm.Ez in 
  let open High_ollvm.Ez.Block in 
  let m = Module.empty in *)

  (* TODO: args, raw_arity, full_args *)
  (* let known_apply m  *)

  build_call callee_val args_val name env.builder
  |> fun x -> set_tail_call true x; x

and gen_expr env =
  function
  | LetExp (is_rec, e1, e2, e3, e4) ->
    gen_letexp env is_rec e1 e2 e3 e4
  | LitExp lit -> gen_literal env lit, env
  | AppExp (callee, args, rest_of_args) ->
    gen_application env callee args rest_of_args, env
  | InfixOp (op, lhs, rhs) -> gen_infix_op env op lhs rhs, env
  | IfExp (cond, then_exp, elif_exps, else_exp) ->
    gen_if_with_elif env cond then_exp elif_exps else_exp, env
  | VarExp var_name -> get_var env var_name, env
  | Exprs exps -> gen_exprs env exps

and gen_extern env name tp =
  let ftype = annot_to_lltype env.ctx (Some tp) in
  let fn = declare_function name ftype env.llmod in
  let env = Env.add_var env name fn in
  fn, env

and gen_module env name exprs =
  let inner_env = { env with mod_prefix = env.mod_prefix ^ name ^ "."} in
  let llval, inner_env = gen_top_levels inner_env exprs in
  llval, { inner_env with mod_prefix  = env.mod_prefix
                        ; opened_vals = env.opened_vals }

and gen_top_level env =
  function
  | Expr e            -> gen_expr env e
  | Extern (name, tp) -> gen_extern env name tp
  | Open path         -> gen_open env path
  | Module (name, es) -> gen_module env name es
  | other             -> show_top_level other
                         |> sprintf "Unsupported top level expression: %s"
                         |> failwith

and gen_top_levels env top_lvl_exprs =
  List.fold top_lvl_exprs ~init:(undef_val, env)
                             ~f:(fun (_, env) -> gen_top_level env)

and insert_vals env add_to_func =
    let entry_bb = entry_block add_to_func in
    let bb       = insert_block env.ctx "calls_to_top_vals" entry_bb in
    let builder  = builder_at_end env.ctx bb in

    List.rev env.top_vals |> List.iter ~f:(
      fun (fn, g_var) ->
        let name = if Option.is_some g_var then "ret" else "" in
        let ret_val = build_call fn.ll [||] name builder in
        Option.iter g_var (fun g_var -> build_store ret_val g_var.ll builder
                                        |> ignore)
    );
    build_br entry_bb builder |> ignore

and insert_top_vals env =
    match lookup_function "main" env.llmod with
  | None      -> failwith "Main function (main : () -> int) is not defined."
  | Some main -> insert_vals env main

and gen_prog ?(module_name="<stdin>") top_lvl_exprs =
  let env = Env.create module_name in
  let llval, env = gen_top_levels env top_lvl_exprs in
  insert_top_vals env;
  llval, env