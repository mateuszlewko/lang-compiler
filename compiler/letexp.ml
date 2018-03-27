open Ast
open Llvm
open Core
open BatPervasives
open CodegenUtils 

let ptr_t   = pointer_type
let fn_t    = function_type
let byte_t  = i8_type (global_context ())
let def_fun = define_function

let closure_t = 
  let ctx    = global_context () in 
  let fields = [|fn_t (void_type ctx) [||] |> ptr_t; ptr_t byte_t
               ; byte_t; byte_t; i32_type ctx |] in
  struct_type ctx fields

let pre_fn_name = (^) "lang.pre."

(** Minimum number of args for pre-* function *)
let closure_args_cnt = 3

let build_call_switch env fn fn_params raw_fn raw_arg_cnt = 
  let entry_bb = entry_block fn in
  (** builder to use in function's scope *)
  let entry_builder  = builder_at_end env.ctx entry_bb in

  (** first basic block after switch cases  *)
  let next_bb = append_block env.ctx "after_switch" fn in

  let ll_data         = fn_params.(2) in
  let raw_fn_arg_ts   = Array.map (params raw_fn) type_of in
  let raw_fn_struct_t = packed_struct_type (env.ctx) raw_fn_arg_ts in
  let data_struct     =
    build_bitcast ll_data (pointer_type raw_fn_struct_t) "data" entry_builder in
  let closure         = build_alloca closure_t "closure" entry_builder in

  (** builds case with call to raw function *)
  let build_case ix cnt =
    let arg_cnt = raw_arg_cnt - cnt in 
    let bb      = insert_block env.ctx (sprintf "case-%d-%d" ix cnt) next_bb in
    let bd      = builder_at_end env.ctx bb in

    let data_args = 
      let rec get args = 
        function 
        | 0 -> args |> Array.of_list
        (* TODO: arg is ptr, so load arg instruction *)
        | n -> let arg = build_struct_gep data_struct (n - 1) 
                                          (sprintf "%d-arg-%d" cnt (n - 1)) bd 
               in get (arg::args) (n - 1)  
      in get [] cnt in

    let passed_args = Array.slice fn_params ix (ix + arg_cnt) in
    let args        = Array.append data_args passed_args in

    build_call raw_fn args "raw" bd |> ignore;
    (* TODO: store call result in thunk or build phi instruction *)
    build_br next_bb bd |> ignore;
    bb
  in
 
  (** basic blocks for all switch cases  *)
  let cases = Array.init raw_arg_cnt (build_case closure_args_cnt) in

  (** switch of env_args (first argument of fn) *)
  let switch = 
    build_switch (fn_params.(0)) next_bb (raw_arg_cnt) entry_builder in

  (** add cases to switch *)
  Array.iteri cases (const_int (byte_t) %> add_case switch);

  let fields = [|fn_t (void_type env.ctx) [||] |> ptr_t; ptr_t byte_t
               ; byte_t; byte_t; i32_type env.ctx |] in

  next_bb, closure

let build_ret_call_switch env fn_params raw_closure then_bd left_args 
                          raw_arg_cnt then_bb arg_ts if_cont_bb = 
  let arity         = 
    build_struct_gep raw_closure 3 "arity" then_bd in
  let pass_env_args = build_sub arity left_args "pass_env_args" then_bd in
  let args_data     = build_struct_gep raw_closure 1 "args_data" then_bd in
 
  let left_args    =
    let total_args = build_add fn_params.(0) fn_params.(1) "" then_bd in
    build_sub total_args (const_int (i32_type env.ctx) raw_arg_cnt) "" then_bd
  in 

  (** builds case with call to raw function *)
  let build_case ix cnt =
    let bb = insert_block env.ctx (sprintf "case-ret-%d" cnt) then_bb in
    let bd = builder_at_end env.ctx bb in

    let data_args   = [|pass_env_args; Const.i32 cnt; args_data|] in
    let passed_args = Array.slice fn_params ix (ix + cnt) in
    let args        = Array.append data_args passed_args in
  
    let call_fn = 
      let fn = build_struct_gep raw_closure 0 "call.fn.pre" bd in   
      build_bitcast fn (function_type closure_t [||] |> ptr_t ) "call.fn" bd in

    let res = build_call call_fn args "res" bd in
    build_ret res bd |> ignore;
    bb
  in
 
  (** basic blocks for all switch cases  *)
  let arg_cnt = Array.length arg_ts in 
  let cases   = 
    Array.init arg_cnt (fun i -> build_case (arg_cnt - i - 1) (i + 1)) in

  (** switch of env_args (first argument of fn) *)
  let switch = build_switch left_args if_cont_bb raw_arg_cnt then_bd in

  (** add cases to switch *)
  Array.iteri cases ((fun i -> Const.i8 (i + 1)) %> add_case switch)

let gen_pre_fun env is_rec (name, ret_type) args exprs raw_fn gen_raw_if = 
  let arg_names, arg_lang_ts = Array.of_list args |> Array.unzip in

  let arg_ts = Array.map arg_lang_ts
                         ~f:(annot_to_lltype env.ctx ~func_as_ptr:true) in
 
  (** function arguments extended with helper arg types *)
  let ext_arg_ts  = Array.append [| byte_t; byte_t; ptr_t byte_t |] arg_ts in
  let fn_type     = function_type closure_t ext_arg_ts in
  let name        = pre_fn_name name in
  let fn          = def_fun (Env.name_of env name) fn_type env.llmod in
  let fn_params   = params fn in
  let raw_params  = params raw_fn in
  let raw_arg_ts  = Array.map raw_params type_of in
  let raw_arg_cnt = Array.length raw_params in

  let last_bb, raw_closure = 
    build_call_switch env fn fn_params raw_fn raw_arg_cnt in
  
  let u           = (LitExp Unit) in
  let cnt_sub_arg = InfixOp ("-", Some (VarExp "lang.cnt")
                                , Some (LitExp (Int raw_arg_cnt))) in
  let condExp = InfixOp ("<", Some (VarExp ("lang.left_args")) 
                            , Some (InfixOp ("+"
                                           , Some (VarExp("lang.env_args"))
                                           , Some cnt_sub_arg
                                            )
                                   ) 
                        ) in

  let bv v      = { ll = v; of_ptr = false } in
  let builder   = builder_at_end env.ctx last_bb in 
  let left_args = build_struct_gep raw_closure 2 "lang.left_args" builder in

  let if_env = { env with opened_vals = 
                            [ "lang.cnt"      , bv fn_params.(1)
                            ; "lang.env_args" , bv fn_params.(0) 
                            ; "lang.left_args", bv left_args 
                            ] |> StrMap.of_alist_exn 

                        ; builder = builder } in 
  
  let then_bb, else_bb, if_cont_bb, _ = gen_raw_if if_env condExp u (Some u) in

  let else_bb = Option.value_exn else_bb in
  let then_bd = builder_at env.ctx (instr_begin then_bb) in 
  
  build_ret_call_switch env fn_params raw_closure then_bd left_args raw_arg_cnt 
                        then_bb arg_ts if_cont_bb;

  let args_struct_t = packed_struct_type env.ctx raw_arg_ts in
  let else_bd       = builder_at env.ctx (instr_begin else_bb) in 
  let params_struct = const_packed_struct env.ctx raw_params in

  let params_ptr =
    let ptr = build_alloca args_struct_t "data" else_bd in
    build_store params_struct ptr else_bd in

  let pref_sum n arr = 
    let res = Array.init n (const 0) in
    for i = 1 to n - 1 do res.(i) <- res.(i - 1) + arr.(i - 1) done;
    res in

  let make_ixs n name = Array.map arg_lang_ts size_of_lang 
                    |> pref_sum n 
                    |> Array.map ~f:Const.i32
                    |> const_array (i32_type env.ctx) 
                    |> fun arr -> define_global name arr env.llmod in

  let from_b_arr = make_ixs raw_arg_cnt "from_b" in 
  let to_b_arr   = make_ixs (raw_arg_cnt + 1) "to_b" in 

  let args       = build_struct_gep raw_closure 1 "t.args" else_bd in
  let used_bytes = build_struct_gep raw_closure 4 "t.used_bytes" else_bd in
  let from_ix    = build_sub (Const.i32 raw_arg_cnt) fn_params.(0) "" else_bd in 
  let from       = build_in_bounds_gep from_b_arr [|Const.i32 0; from_ix|]
                                       "from" else_bd in

  ()