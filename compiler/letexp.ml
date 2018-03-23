open Ast
open Llvm
open Core
open BatPervasives
open BatString
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

let gen_pre_fun env is_rec (name, ret_type) args exprs = 
  let arg_names, arg_ts = Array.of_list args |> Array.unzip in
  
  let arg_ts     = 
   Array.map arg_ts ~f:(annot_to_lltype env.ctx ~func_as_ptr:true) in
  let ext_arg_ts = Array.append [| byte_t; byte_t; ptr_t byte_t |] arg_ts in
  let fn_type    = function_type closure_t ext_arg_ts in
  let name       = pre_fn_name name in
  let fn         = def_fun (Env.name_of env name) fn_type env.llmod in

  let entry_bb = entry_block fn in
  
  position_at_end entry_bb env.builder;

  let next_bb  = append_block env.ctx "next" fn in
  (* let bd = builder? *)
  
  build_ret (const_null closure_t) env.builder |> ignore;

  let switch   = build_switch (const_int (i8_type env.ctx) 0) next_bb
                              (0) env.builder in

  add_case switch (const_int (byte_t) 1) next_bb;

  ()