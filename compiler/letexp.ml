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

  (** builds case with call to raw function *)
  let build_case ix cnt =
    let arg_cnt = raw_arg_cnt - cnt in 
    let bb      = insert_block env.ctx (sprintf "case %d-%d" ix cnt) next_bb in
    let bd      = builder_at_end env.ctx bb in
  
    let data_args = 
      let rec get args = 
        function 
        | 0 -> args |> Array.of_list
        | n -> let arg = build_struct_gep data_struct (n - 1) 
                                          (sprintf "%d-arg-%d" cnt (n - 1)) bd 
               in get (arg::args) (n - 1)  
      in get [] cnt in

    let passed_args = Array.slice fn_params ix (ix + arg_cnt) in
    let args        = Array.append data_args passed_args in

    build_call raw_fn args "raw" bd |> ignore;
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
  next_bb

let gen_pre_fun env is_rec (name, ret_type) args exprs raw_fn = 
  let arg_names, arg_ts = Array.of_list args |> Array.unzip in
  let arg_ts            = 
   Array.map arg_ts ~f:(annot_to_lltype env.ctx ~func_as_ptr:true) in
 
  (** function arguments extended with helper arg types *)
  let ext_arg_ts  = Array.append [| byte_t; byte_t; ptr_t byte_t |] arg_ts in
  let fn_type     = function_type closure_t ext_arg_ts in
  let name        = pre_fn_name name in
  let fn          = def_fun (Env.name_of env name) fn_type env.llmod in
  let fn_params   = params fn in
  let raw_params  = params raw_fn in
  let raw_arg_cnt = Array.length raw_params in

  let last_bb = build_call_switch env fn fn_params raw_fn raw_arg_cnt in

  build_ret (const_null closure_t) (builder_at_end env.ctx last_bb) |> ignore;

  ()