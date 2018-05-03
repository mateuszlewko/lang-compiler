open Lang_parsing.Ast
open Llvm
open Core
open BatPervasives
open CodegenUtils 
open High_ollvm

let ptr_t   = pointer_type
let fn_t    = function_type
let byte_t  = i8_type (global_context ())
let i32_t  = i32_type (global_context ())
let i8_t = byte_t
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
        | n -> let name = sprintf "%d-arg-%d" cnt (n - 1) in
               let arg  = struct_gep_load data_struct (n - 1) name bd in 
               get (arg::args) (n - 1)  
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

let build_ret_call_switch env fn_params raw_closure then_bd left_args arg_cnt
                          raw_arg_cnt then_bb raw_arg_ts if_cont_bb = 
  let arity         = struct_gep_load raw_closure 3 "arity" then_bd in
  let pass_env_args = build_sub arity left_args "pass_env_args" then_bd in
  let args_data     = struct_gep_load raw_closure 1 "args_data" then_bd in
 
  let left_args    =
    let total_args = build_add fn_params.(0) fn_params.(1) "" then_bd in
    build_sub total_args (Const.i8 raw_arg_cnt) "left_args-switch" then_bd
  in 

  (** builds case with call to raw function *)
  let build_case ix cnt =
    let bb = insert_block env.ctx (sprintf "case-ret-%d" cnt) then_bb in
    let bd = builder_at_end env.ctx bb in

    let data_args   = [|pass_env_args; Const.i8 cnt; args_data|] in
    let passed_args = Array.slice fn_params (ix + 3) (ix + cnt + 3) in
    let args        = Array.append data_args passed_args in
  
    let call_fn = 
      let fn        = struct_gep_load raw_closure 0 "call.fn.pre" bd in  
      let len       = raw_arg_cnt in
      let last_args = Array.slice raw_arg_ts (len - cnt) len in
      let call_args = Array.append [|byte_t; byte_t; ptr_t byte_t|] last_args in

      build_bitcast fn (fn_t closure_t call_args |> ptr_t ) "call.fn" bd in

    let res = build_call call_fn args "res" bd in
    build_ret res bd |> ignore;
    bb
  in
 
  (** basic blocks for all switch cases  *)
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
                                , Some (LitExp (Int8 raw_arg_cnt))) in
  let condExp = InfixOp ("<", Some (VarExp ("lang.left_args")) 
                            , Some (InfixOp ("+"
                                           , Some (VarExp("lang.env_args"))
                                           , Some cnt_sub_arg
                                            )
                                   ) 
                        ) in
 
  let bv v         = { ll = v; of_ptr = false } in
  let builder      = builder_at_end env.ctx last_bb in 
  let left_args    = struct_gep_load raw_closure 2 "lang.left_args" builder in
  let passed_cnt   = fn_params.(1) in
  let env_args_cnt = fn_params.(0) in

  let if_env = { env with opened_vals = 
                            [ "lang.cnt"      , bv passed_cnt
                            ; "lang.env_args" , bv fn_params.(0) 
                            ; "lang.left_args", bv left_args 
                            ] |> StrMap.of_alist_exn 

                        ; builder = builder } in 
  
  let then_bb, else_bb, if_cont_bb, _ = gen_raw_if if_env condExp u (Some u) in

  let else_bb = Option.value_exn else_bb in
  let then_bd = builder_at env.ctx (instr_begin then_bb) in 
  
  build_ret_call_switch env fn_params raw_closure then_bd left_args raw_arg_cnt 
                        (Array.length arg_ts) then_bb raw_arg_ts if_cont_bb;

  let args_struct_t = packed_struct_type env.ctx raw_arg_ts in
  let else_bd       = builder_at env.ctx (instr_begin else_bb) in 
  (* let params_struct = const_packed_struct env.ctx fn_params in *)

  let set_struct_elem struct_ptr bd ix elem = 
    let ix = ix in
    let ptr = build_struct_gep struct_ptr ix (sprintf "arg-%d" ix) bd in
    build_store elem ptr bd |> ignore in

  let params_ptr =
    let ptr_s = build_alloca args_struct_t "data" else_bd in
    Array.iteri (Array.slice fn_params 3 0) (set_struct_elem ptr_s else_bd);

    (* let ptr_s = build_store params_struct ptr_s else_bd in *)
    (* bitcast to i8* *)
    build_bitcast ptr_s (i8_type env.ctx |> ptr_t) "data.ptr" else_bd in

  let pref_sum n arr = 
    let res = Array.init n (const 0) in
    for i = 1 to n - 1 do res.(i) <- res.(i - 1) + arr.(i - 1) done;
    res in

  let make_ixs n name = Array.map arg_lang_ts size_of_lang 
                    |> pref_sum n 
                    |> Array.map ~f:Const.i32
                    |> const_array (i32_type env.ctx) 
                    |> fun arr -> define_global name arr env.llmod 
                                  >>* set_global_constant true in

  let from_b_arr = make_ixs raw_arg_cnt "from_b" in 
  let to_b_arr   = make_ixs (raw_arg_cnt + 1) "to_b" in 
  
  let ll_sub lhs rhs = build_sub lhs rhs "ll_sub" else_bd in 
  let ll_add lhs rhs = build_add lhs rhs "ll_add" else_bd in 

  let args       = struct_gep_load raw_closure 1 "t.args" else_bd in
  let used_bytes = struct_gep_load raw_closure 4 "t.used_bytes" else_bd in
  let from_ix    = ll_sub (Const.g_i8 else_bd env.llmod raw_arg_cnt)
                          fn_params.(0) in 
  let from       = build_in_bounds_gep from_b_arr [|Const.i32 0; from_ix|]
                                       "from_ptr" else_bd 
                   |> fun v -> build_load v "from_val" else_bd
                   (* |> fun v -> build_bitcast v (ptr_t i8_t) "to_i8*" else_bd *)
                   in 

                   (* |> fun v -> build_load v "" else_bd in *)
  (* from_pos = (( byte* )data) + from *)
  
  let to_int ptr = build_ptrtoint ptr i32_t "p->i" else_bd in
  let to_ptr p = build_inttoptr p (ptr_t byte_t) "i->p" else_bd in

  let from_pos  = ll_add (to_int params_ptr) from |> to_ptr in
  let dest      = ll_add (to_int args) used_bytes |> to_ptr in
  let bytes_cnt = build_gep to_b_arr [|Const.i32 0; passed_cnt|] "b_cnt_pre_ptr" 
                            else_bd
                  |> fun p -> build_load p "b_cnt_pre" else_bd 
                  |> fun pre -> ll_sub pre from in

  build_memcpy dest from_pos bytes_cnt else_bd env.llmod;

  let left_args = ll_add left_args env_args_cnt 
                  |> fun lhs -> ll_sub lhs (Const.i8 raw_arg_cnt)
                  |> ll_sub left_args in

  let gep_store bd ll_val ix str = 
    build_struct_gep str ix "gep" bd
    |> fun ptr -> build_store ll_val ptr bd |> ignore in

  let res =
    gep_store else_bd left_args 2 raw_closure;
    gep_store else_bd  (ll_add used_bytes bytes_cnt) 4 raw_closure in
     
    (* |> fun t -> build_insertvalue t (ll_add used_bytes bytes_cnt) 4
                                  "t" else_bd in  *)
  
  let last_bd = builder_at_end env.ctx if_cont_bb in
  build_load raw_closure "res" last_bd
  |> fun res -> build_ret res last_bd |> ignore;  

  ()

open High_ollvm.Ez.Value
open High_ollvm.Ez.Instr
open High_ollvm.Ez.Block
open High_ollvm
module M = High_ollvm.Ez.Module
module T = High_ollvm.Ez.Type

type entry_fn_info = 
  { env_args_cnt  : Ez.Value.t 
  ; pass_args_cnt : Ez.Value.t
  ; data_ptr      : Ez.Value.t 
  ; args          : Ez.Value.t list
  ; definition    : Ez.Block.block list -> Ast.definition
  }

let define_entry_fn m args name ret_type kind = 
  let arg_names, arg_lang_ts = List.unzip args in
  let arg_ts = List.map arg_lang_ts
                        ~f:(annot_to_ho_type ~fn_ptr:true) in
 
  let m, env_args_cnt  = M.local m T.i8 "env_args_cnt" in 
  let m, pass_args_cnt = M.local m T.i8 "pass_args_cnt" in 
  let m, data_ptr      = M.local m (T.ptr T.i8) "data_ptr" in 
  let m, args          = List.zip_exn arg_ts arg_names |> M.batch_locals m in

  let name       =
    (match kind with 
     | `ReturnsValue -> "lang.entry.value."
     | `ReturnsFn    -> "lang.entry.fn."
    ) ^ name in 
  
  let m, fn    = M.global m ret_type name in 
  let def      = define fn (env_args_cnt::pass_args_cnt::data_ptr::args) in
  let info     = 
    { env_args_cnt  = env_args_cnt
    ; pass_args_cnt = pass_args_cnt
    ; data_ptr      = data_ptr
    ; args          = args 
    ; definition    = def
    } in m, info  

type value_entry_info = 
  { definition : Ez.Block.block list -> Ast.definition
  ; data_ptr   : Ez.Value.t 
  ; args       : Ez.Value.t list 
  }

let struct_fields m struct_ptr fields_idx = 
  let rec extract m args instructions =
    function 
    | []      -> m, instructions, List.rev args
    | ix::ixs -> 
      let m, ptr = M.local m (T.ptr T.opaque) "ptr" in
      let ptr_i  = ptr <-- struct_gep struct_ptr ix in
      let m, arg = M.local m T.opaque "arg" in
      let arg_i  = arg <-- load ptr in 
      extract m (arg::args) (ptr_i::arg_i::instructions) ixs in

  extract m [] [] fields_idx 

let extract_from_struct m struct_ptr idx = failwith "TODO: extract_from_struct"

let define_common m args name ret_type =
  let m, fn       = M.global m ret_type name in 
  let m, args     = M.batch_locals m args in
  let m, data_ptr = M.local m (T.ptr T.i8) "env_ptr" in 
  m, fn, args, data_ptr

let define_value_entry m args name ret_type =
  let m, fn, args, data_ptr = define_common m args name ret_type in
 
  let def = define fn (args @ [data_ptr])
  in m, { definition = def; data_ptr; args }

let entry_body_common m pref_args raw_fn data_ptr pass_args = 
  let data_t           = T.structure ~packed:true pref_args |> T.ptr in
  let m, data_ptr_cast = M.local m data_t "data_ptr_cast" in 
  
  let data_i          = data_ptr_cast <-- bitcast data_ptr data_t in
  let m, instrs, args = BatList.range 0 `To (List.length pref_args - 1)  
                        |> struct_fields m data_ptr_cast in 

  let m, res = M.local m T.opaque "ret__" in
  let args   = args @ pass_args in 
  let instrs = data_i::instrs @ [ res <-- call ~tail:true raw_fn args ] in 
  
  let m, entry_b = M.local m T.label "entry" in
  m, entry_b, res, instrs

let value_entry_body m pref_args raw_fn info = 
  let m, entry_b, res, instrs = entry_body_common m pref_args raw_fn 
                                                  info.data_ptr info.args in
  let instrs = instrs @ [ ret res ] in
  m, info.definition [ block entry_b instrs ]

let value_entry_fns m env name ret_type args raw_fn = 
  let args_cnt = List.length args in 
  let arg_names, arg_lang_ts = List.unzip args in
  let arg_ts = List.map arg_lang_ts (annot_to_ho_type ~fn_ptr:true) in
  let args   = List.zip_exn arg_ts arg_names in 

  let rec fold_args ix m =
    if ix > args_cnt
    then m 
    else begin 
      let pref_args, args = List.split_n args ix in 
      let name    = sprintf "lang.entry.value.%s-%d" name ix in
      let m, info = define_value_entry m args name ret_type in
      let m, def  = value_entry_body m (List.map pref_args fst) raw_fn info in 
      M.definition m def |> fold_args (ix + 1) 
    end
    in

  let m = fold_args 1 m in
  LLGate.ll_module_in env.llmod m.m_module |> ignore

type closure_entry_info = 
  { v   : value_entry_info
  (** function argument representing number of passed arguments  *)
  ; cnt : Ez.Value.t
  }
let closure_t = T.structure ~packed:(true) [ T.ptr T.void; T.ptr T.i8; T.i8
                                            ; T.i8; T.i32]

let define_closure_entry m args name =
  let m, fn, args, data_ptr = define_common m args name closure_t in

  let m, cnt = M.local m T.i8 "cnt" in 
  let def    = define fn (data_ptr::cnt::args) in 
  
  m, { v = { definition = def; data_ptr; args } ; cnt }                  

let set_field m ptr ix value =
    let m, elem_ptr = M.local m (T.ptr T.opaque) "elem_ptr" in
    let is = [ elem_ptr <-- struct_gep ptr ix
             ; store value elem_ptr |> snd ] in
    m, is

let set_fields m ptr fields values = 
  let rec set instrs = 
    function
    | [], _ | _, []        -> m, instrs
    | f::fields, v::values -> 
      let m, is = set_field m ptr f v in 
      set (is @ instrs) (fields, values) in 
  set [] (fields, values)

[@@@warning "-8"]
let closure_entry_body m arity pref_args raw_fn info = 
  let m, entry_b, res, instrs = 
    entry_body_common m pref_args raw_fn info.v.data_ptr info.v.args in

  let env_args   = List.length pref_args in
  let env_args_c = i8 env_args in 
  let arity_c    = i8 arity in 

  let m, entry_instrs, [ res_left_args; res_arity; res_fn; res_args
                       ; res_used_bytes ] = 
    struct_fields m res [2; 3; 0; 1; 4] in
  
  let m, [x1; left_pass_args] = M.locals m T.i8 [""; "left_pass_args"] in 
  let m, x3 = M.local m T.i1 "" in 

  let m, then_b = M.local m T.label "then_b" in 
  let m, else_b = M.local m T.label "else_b" in 

  let entry_instrs = instrs @ entry_instrs @
    [ x1             <-- add env_args_c info.cnt
    ; left_pass_args <-- sub x1 arity_c
    ; x3             <-- slt res_left_args left_pass_args
    ; br x3 then_b else_b
    ] in

  (* then branch *)
  let m, pass_env_args = M.local m T.i8 "pass_env_args" in
  (* let m, unreach_l     = M.local m T.label "unreach_b" in  *)
  let m, ret_res       = M.local m closure_t "ret_res" in 
  (* let unreach_b        = block unreach_l [unreachable] in  *)
  let used_args_cnt   = arity - env_args in
  let unused_args     = List.drop info.v.args used_args_cnt in
  let unused_args_cnt = List.length unused_args in
  
  let then_instrs = 
    let args = res_args::left_pass_args::unused_args in 
    [ ret_res <-- call res_fn args
    ; ret ret_res
    ] in

  (* else branch *)

  let size_pref_sums = 
    List.map unused_args bs_size 
    |> List.fold ~init:([0], 0) ~f:(fun (sums, last) s -> let s = last + s
                                                         in (s::sums, s))
    |> fst |> List.rev in

  let m, size_pref_sums_g = 
    let arr = array (List.map size_pref_sums i32) in
    M.global_val m arr ~const:(true) "size_pref_sums" in

  let data_t = T.structure ~packed:(true) (List.map unused_args fst) in

  let m, b_cnt_ptr = M.local m (T.ptr T.i32) "b_cnt_ptr" in
  let m, b_cnt     = M.local m T.i32 "b_cnt" in
  let m, data_ptr  = M.local m (T.ptr data_t) "data_ptr" in

  let else_instrs = 
    [ b_cnt_ptr <-- get_elem_ptr_raw size_pref_sums_g [left_pass_args] 
    ; b_cnt     <-- load b_cnt_ptr
    ; data_ptr  <-- alloca data_t ] in

  let m, instrs = 
    set_fields m data_ptr (List.init unused_args_cnt identity) unused_args in

  let m, dest_ptr       = M.local m (T.ptr T.opaque) "dest_ptr" in
  let m, left_args_ptr  = M.local m T.opaque "" in 
  let m, new_left_args  = M.local m T.opaque "" in
  let m, used_bytes_ptr = M.local m T.opaque "" in 
  let m, new_used_bytes = M.local m T.opaque "" in

  let else_instrs = else_instrs @ instrs @ 
    [ dest_ptr <-- get_elem_ptr_raw res_args [res_used_bytes]
    (* memcpy(res_args + res_used_bytes, data_ptr, b_cnt) *)
    ; memcpy dest_ptr data_ptr b_cnt |> snd
    
    (* res.left_args -= left_pass_args *)
    ; left_args_ptr <-- struct_gep res 2
    ; new_left_args <-- sub res_left_args left_pass_args
    ; store new_left_args left_args_ptr |> snd
    
    (* res.used_bytes += b_cnt *)
    ; used_bytes_ptr <-- struct_gep res 4 
    ; new_used_bytes <-- add res_used_bytes b_cnt
    ; store new_used_bytes used_bytes_ptr |> snd ] in

  m, info.v.definition [ block entry_b entry_instrs 
                       ; block then_b then_instrs
                       ; block else_b else_instrs ]
[@@@warning "+8"]

let sum_by lst fn = List.fold lst ~init:0 ~f:(fun s x -> s + fn x)
let size_of_args args = sum_by args bs_size 

let closure_entry_fns m env name (full_args : typed_var list) arity raw_fn = 
  (* let args_cnt = List.length full_args in  *)
  let arg_names, arg_lang_ts = List.unzip full_args in
  let arg_ts = List.map arg_lang_ts (annot_to_ho_type ~fn_ptr:true) in
  let args   = List.zip_exn arg_ts arg_names in 

  let rec fold_args ix m =
    if ix >= arity
    then m 
    else begin 
      let pref_args, args = List.split_n args ix in 
      let name    = sprintf "lang.entry.closure.%s-%d" name ix in
      let m, info = define_closure_entry m args name in
      let m, def  = 
        closure_entry_body m arity (List.map pref_args fst) raw_fn info in 
      M.definition m def |> fold_args (ix + 1) 
    end
    in

  let m = fold_args 1 m in
  LLGate.ll_module_in env.llmod m.m_module |> ignore

(** apply arguments to function which returns closure *)
let value_apply m env closure_ptr ret_t args =  
  (* let m,  = M.local m T.label "after_b" in  *)
  (* let m, res_fn = M.local m (T.fn ret_t ) *)
  (* let after_instrs =  *)

  let m, entry_instrs, [ cl_left_args; cl_arity; cl_fn; cl_args
                       ; cl_used_bytes ] = 
    struct_fields m closure_ptr [2; 3; 0; 1; 4] in

  let m, then_b = M.local m T.label "then_b" in 
  let m, else_b = M.local m T.label "else_b" in 
  let m, last   = M.local m T.opaque "__tmp_last" in 
  let args_cnt_c = List.length args |> i8 in 
  

  let call_args   = cl_args::args_cnt_c::args in 
  let call_args_t = List.map call_args fst in 

  let entry_instrs = entry_instrs @ 
    [ last <-- eq cl_left_args args_cnt_c
    ; br last then_b else_b
    ] in

  let then_instrs = 
    [ last <-- bitcast cl_fn (T.fn ret_t call_args_t)
    ; last <-- call last call_args 
    ] in

  let m, tmp = M.local m T.opaque "tmp_res" in 

  let else_instrs = 
    [ last <-- bitcast cl_fn (T.fn closure_t call_args_t)
    ; tmp  <-- call last call_args] in

  let m, instrs, [tmp_fn; tmp_env] = struct_fields m tmp [0; 1] in

  let call_args_empty   = [tmp_env; i8 0] in 
  let call_args_empty_t = List.map call_args fst in 

  let else_instrs = else_instrs @ instrs @ 
    [ tmp_fn <-- bitcast tmp_fn (T.fn ret_t call_args_empty_t)
    ; last   <-- call tmp_fn call_args_empty
    ] in

  entry_instrs, [block then_b then_instrs; block else_b else_instrs], last

(** apply arguments to function which returns value *)
let closure_apply m closure_ptr args = 
  let m, entry_instrs, [ cl_left_args; cl_arity; cl_fn; cl_args
                       ; cl_used_bytes ] = 
    struct_fields m closure_ptr [2; 3; 0; 1; 4] in

  let m, x1     = M.local m T.i8 "cmp" in 
  let m, then_b = M.local m T.label "then_b" in 
  let m, else_b = M.local m T.label "else_b" in 
  
  let entry_instrs = entry_instrs @  
    [ x1 <-- sgt cl_left_args (List.length args |> i8)
    ; br x1 then_b else_b
    ] in 

  let m, res      = M.local m closure_t "res" in 

  (* then branch *)
  let m, res_args_ptr = M.local m (T.ptr T.i8) "res_args" in 
  let m, total_bytes  = M.local m T.i8 "total_bytes" in 
  let m, heap_bytes   = M.local m (T.ptr T.i8) "bytes_ptr" in
  let m, cl_args_ptr  = M.local m (T.ptr T.i8) "args_ptr" in 
  let m, last         = M.local m T.opaque "__any_last" in 
  let data_t = T.structure ~packed:true (List.map args fst) in
  let args_size = size_of_args args |> i8 in 

  let then_instrs = 
    [ res          <-- alloca closure_t 
    ; res_args_ptr <-- struct_gep res 1 
    ; total_bytes  <-- add cl_used_bytes args_size
    ; heap_bytes   <-- malloc_raw total_bytes
    ; store heap_bytes res_args_ptr |> snd
    ; memcpy res_args_ptr cl_args_ptr cl_used_bytes |> snd
    ; last <-- ptr2i res_args_ptr T.i8
    ; last <-- add last cl_used_bytes
    ; last <-- bitcast last data_t
    ] in 

  let cnt               = List.length args in
  let m, instrs         = set_fields m last (List.range 0 cnt) args in
  let m, left_args_ptr  = M.local m T.opaque "left_args_ptr" in 
  let m, used_bytes_ptr = M.local m T.opaque "left_args_ptr" in 

  let then_instrs = then_instrs @ instrs @ 
    [ left_args_ptr  <-- struct_gep res 2 
    ; last           <-- sub cl_left_args (i8 cnt)
    ; store last left_args_ptr |> snd
    ; used_bytes_ptr <-- struct_gep res 4 
    ; last           <-- add cl_used_bytes args_size 
    ; store last used_bytes_ptr |> snd
    ; last           <-- load res
    (* ; ret last *)
    ] in

  let call_args   = cl_args_ptr::(i8 cnt)::args in 
  let call_args_t = List.map call_args fst in 

  (* else branch *)
  let else_instrs = 
    [ cl_fn <-- bitcast cl_fn (T.fn closure_t call_args_t)
    ; last  <-- call cl_fn call_args
    (* ; ret last  *)
    ] in 

  entry_instrs, [ block then_b then_instrs; block else_b else_instrs ], last

(** create closure *)
[@@@warning "-8"]
let known_apply m args raw_arity full_args raw_fn =
  let args_cnt = List.length args in 

  if args_cnt = raw_arity 
  then 
    let open High_ollvm.Ast in
    let m, call_res = M.local m T.opaque "call_res" in
   
    match fst raw_fn with 
    | TYPE_Function (TYPE_Void, _) ->
      m, [call raw_fn args |> snd], call_res
    | TYPE_Function (other, _) ->
      m, [call_res <-- call raw_fn args], call_res
    | other -> failwith "expected raw_fn to be function type"
  else  
    let m, closure_ptr   = M.local m (T.ptr closure_t) "closure" in 
    let m, args_ptr      = M.local m (T.ptr T.i8) "args_ptr" in 
    let data_t           = T.structure ~packed:true full_args in
    let m, args_ptr_cast = M.local m data_t "data_ptr_cast" in 
    let m, heap_bytes    = M.local m (T.ptr data_t) "malloc_data_ptr" in 

    let else_instrs = 
      [ closure_ptr   <-- alloca closure_t
      ; args_ptr      <-- struct_gep closure_ptr 1
      ; args_ptr_cast <-- bitcast args_ptr (T.ptr data_t)
      ; heap_bytes    <-- malloc data_t
      ; store heap_bytes args_ptr |> snd
      ] in 

    let m, instrs1 = set_fields m args_ptr_cast (List.range 0 args_cnt) args in 
    let m, instrs2, [cl_arity; cl_left_args; cl_used_bytes] = 
      struct_fields m closure_ptr [3; 2; 4] in

    let else_instrs = else_instrs @ instrs1 @ instrs2 @
      [ store (i32 raw_arity) cl_arity |> snd
      ; store (i32 (raw_arity - args_cnt)) cl_left_args |> snd 
      ; store (size_of_args args |> i32) 
              cl_used_bytes |> snd
      ] in

  m, else_instrs, closure_ptr

[@@@warning "+8"]
