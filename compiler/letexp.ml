open Lang_parsing.Ast
open Llvm
open Core
open BatPervasives
open High_ollvm

open High_ollvm.Ez.Value
open High_ollvm.Ez.Instr
open High_ollvm.Ez.Block
open High_ollvm
module M = High_ollvm.Ez.Module
module T = High_ollvm.Ez.Type
module LT = Lang_types

type entry_fn_info = 
  { env_args_cnt  : Ez.Value.t 
  ; pass_args_cnt : Ez.Value.t
  ; data_ptr      : Ez.Value.t 
  ; args          : Ez.Value.t list
  ; definition    : Ez.Block.block list -> Ast.definition
  }

let closure_t = Lang_types.closure_t 

type value_entry_info = 
  { definition : Ez.Block.block list -> Ast.definition
  ; data_ptr   : Ez.Value.t 
  ; args       : Ez.Value.t list 
  ; fn         : Ez.Value.t
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

let struct_val_fields m struct_val fields_idx = 
  let rec extract m args instructions =
    function 
    | []      -> m, instructions, List.rev args
    | ix::ixs -> 
      let m, v   = M.local m T.opaque "val" in
      let val_i  = v <-- extractvalue struct_val ix in
      extract m (v::args) (val_i::instructions) ixs in

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
  in m, { definition = def; data_ptr; args; fn }

let array_of_fns m name fns = 
  let ptr_t   = T.ptr (T.fn T.void []) in 
  let fns     = List.rev fns 
                |> List.map ~f:(fun f -> !% (bitcast f ptr_t)) in
  let fns_arr = T.array (List.length fns) ptr_t, Ast.VALUE_Array fns in 
  M.global_val m fns_arr name

let entry_body_common m pref_args raw_fn data_ptr pass_args = 
  let data_t           = T.structure ~packed:true pref_args |> T.ptr in
  let m, data_ptr_cast = M.local m data_t "data_ptr_cast" in 
  
  let data_i          = data_ptr_cast <-- bitcast data_ptr data_t in
  let m, instrs, args = 
    let to_ = List.length pref_args - 1 in 
    if to_ >= 0 
    then BatList.range 0 `To to_ |> struct_fields m data_ptr_cast
    else m, [], [] in 

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

let value_entry_fns m name ret_type args raw_fn = 
  let args_cnt = List.length args in 
  (* let arg_names, arg_lang_ts = List.unzip args in
  let arg_ts = List.map arg_lang_ts (annot_to_ho_type ~fn_ptr:true) in
  let args   = List.zip_exn arg_ts arg_names in  *)

  let rec fold_args ix fns m =
    if ix > args_cnt
    then m, fns 
    else begin 
      let pref_args, args = List.split_n args ix in 
      let name    = sprintf "lang.entry.value.%s-%d" name ix in
      let m, info = define_value_entry m args name ret_type in
      let m, def  = value_entry_body m (List.map pref_args fst) raw_fn info in 
      M.definition m def |> fold_args (ix + 1) (info.fn::fns)
    end
    in

  let m, fns = fold_args 0 [] m in
  array_of_fns m (name ^ "_entry_fns") fns

type closure_entry_info = 
  { v   : value_entry_info
  (** function argument representing number of passed arguments  *)
  ; cnt : Ez.Value.t
  }

let define_closure_entry m args name =
  let m, fn, args, data_ptr = define_common m args name closure_t in

  let m, cnt = M.local m T.i8 "cnt" in 
  let def    = define fn (data_ptr::cnt::args) in 
  
  m, { v = { definition = def; data_ptr; args; fn } ; cnt }                  

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
    struct_val_fields m res [2; 3; 0; 1; 4] in
  
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
    let res_args  = T.ptr T.i8, snd res_args in 
    let args      = res_args::left_pass_args::unused_args in 
    let (_, r_fn) = res_fn in 
    let res_fn    = T.fn T.void [] |> T.ptr, r_fn in  
    let arg_ts    = List.map args fst in 
    [ res_fn  <-- load res_fn 
    ; res_fn  <-- bitcast res_fn (T.fn closure_t arg_ts |> T.ptr)
    ; ret_res <-- call res_fn args
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
    [ b_cnt_ptr <-- get_elem_ptr_raw size_pref_sums_g [i32 0; left_pass_args] 
    ; b_cnt     <-- load b_cnt_ptr
    ; data_ptr  <-- alloca data_t ] in

  let m, instrs = 
    set_fields m data_ptr (List.init unused_args_cnt identity) unused_args in

  let m, dest_ptr       = M.local m (T.ptr T.opaque) "dest_ptr" in
  let m, left_args_ptr  = M.tmp m in
  let m, new_left_args  = M.tmp m in
  let m, used_bytes_ptr = M.tmp m in
  let m, new_used_bytes = M.tmp m in 
  let m, fn_ptr         = M.local m T.opaque "fn_ptr" in
  let m, last           = M.tmp m in

  let else_instrs = else_instrs @ instrs @ 
    [ dest_ptr <-- get_elem_ptr_raw res_args [res_used_bytes]
    (* memcpy(res_args + res_used_bytes, data_ptr, b_cnt) *)
    ; dest_ptr <-- bitcast dest_ptr (T.ptr T.i8) 
    ; data_ptr <-- bitcast data_ptr (T.ptr T.i8) 
    ; memcpy data_ptr dest_ptr b_cnt |> snd

    (* res.left_args -= left_pass_args *)
    (* ; left_args_ptr <-- extractvalue res 2 *)
    ; new_left_args <-- sub res_left_args left_pass_args
    ; res <-- insertvalue res new_left_args [2] 
    
    (* increase fn pointer by number of applied args *)
    ; fn_ptr         <-- extractvalue res 0
    ; last           <-- get_elem_ptr_raw fn_ptr [left_pass_args]
    ; res <-- insertvalue res last [0]

    (* res.used_bytes += b_cnt *)
    (* ; used_bytes_ptr <-- extractvalue res 4  *)
    ; new_used_bytes <-- add res_used_bytes b_cnt
    ; res <-- insertvalue res new_used_bytes [4] 
    ; ret res ] in

  m, info.v.definition [ block entry_b entry_instrs 
                       ; block then_b then_instrs
                       ; block else_b else_instrs ]
[@@@warning "+8"]

let sum_by lst fn = List.fold lst ~init:0 ~f:(fun s x -> s + fn x)
let size_of_args args = sum_by args bs_size 

let closure_entry_fns m name full_args arity raw_fn = 
  let args   = full_args in 

  let rec fold_args ix fns m =
    if ix > arity
    then m, fns
    else begin 
      let pref_args, args = List.split_n args ix in 
      let name    = sprintf "lang.entry.closure.%s-%d" name ix in
      let m, info = define_closure_entry m args name in
      let m, def  = 
        closure_entry_body m arity (List.map pref_args fst) raw_fn info in 
      M.definition m def |> fold_args (ix + 1) (info.v.fn::fns)
    end
    in

  let m, fns = fold_args 0 [] m in 
  array_of_fns m (name ^ "_entry_fns") fns

(** apply arguments to function which returns closure *)
let value_apply ?(is_ptr=false) m closure_ptr ret_t args sink_b =  
  let m, entry_instrs, [ cl_left_args; cl_arity; cl_fn; cl_args
                       ; cl_used_bytes ] = 
    let is_ptr = false in 

    (if is_ptr 
     then struct_fields 
     else struct_val_fields
    ) m closure_ptr [2; 3; 0; 1; 4] in
    (* struct_val_fields m closure_ptr [2; 3; 0; 1; 4] in *)
  
  (* set correct types (currently type of struct field is opaque) *)
  let cl_args      = T.ptr T.i8, snd cl_args in
  let cl_left_args = T.i8, snd cl_left_args in 

  let m, then_b  = M.local m T.label "then_b" in 
  let m, else_b  = M.local m T.label "else_b" in 
  let m, last    = M.local m T.opaque "__tmp_last" in 
  let m, then_res = M.local m T.opaque "then_res" in 
  let m, else_res = M.local m T.opaque "else_res" in 
  let args_cnt_c  = List.length args |> i8 in 
  let call_args   = args @ [cl_args] in 
  let call_args_t = List.map call_args fst in 

  let entry_instrs = entry_instrs @ 
    [ last <-- eq cl_left_args args_cnt_c
    ; br last then_b else_b
    ] in

  let then_instrs = 
    [ 
      (* last     <-- cl_fn *)
      last     <-- load cl_fn
    ; last     <-- bitcast last (T.fn ret_t call_args_t |> T.ptr)
    ; then_res <-- call last call_args 
    ; br1 sink_b
    ] in

  let m, tmp = M.local m T.opaque "tmp_res" in 
  let closure_call_args   = cl_args::args_cnt_c::args in 
  let closure_call_args_t = List.map closure_call_args fst in 

  let else_instrs = 
    [ 
      last <-- load cl_fn
    ; last <-- bitcast last (T.fn closure_t closure_call_args_t |> T.ptr)
    (* ; last <-- load last *)
    ; tmp  <-- call last closure_call_args] in

  let m, instrs, [tmp_fn; tmp_env] = struct_val_fields m tmp [0; 1] in
  let tmp_env = T.ptr T.i8, snd tmp_env in 
  
  let call_args_empty   = [tmp_env] in 
  let call_args_empty_t = List.map call_args_empty fst in 

  let else_instrs = else_instrs @ instrs @ 
    [ tmp_fn   <-- load tmp_fn
    ; tmp_fn   <-- bitcast tmp_fn (T.fn ret_t call_args_empty_t |> T.ptr)
    ; else_res <-- call tmp_fn call_args_empty
    ; br1 sink_b
    ] in

  let blocks = [block then_b then_instrs; block else_b else_instrs] in 
  m, entry_instrs, blocks, phi [then_res, then_b; else_res, else_b]

(** apply arguments to function which returns value *)
let closure_apply ?(is_ptr=false) m closure_ptr args sink_block = 
  let is_ptr = false in 

  let m, entry_instrs, [ cl_left_args; cl_arity; cl_fn; cl_args
                       ; cl_used_bytes ] = 
    (if is_ptr 
     then struct_fields 
     else struct_val_fields
    ) m closure_ptr [2; 3; 0; 1; 4] in

  let cl_left_args  = T.i8, snd cl_left_args in
  let cl_used_bytes = T.i32, snd cl_used_bytes in
  let cl_args       = T.ptr T.i8, snd cl_args in 

  let m, x1     = M.local m T.i8 "cmp" in 
  let m, then_b = M.local m T.label "then_b" in 
  let m, else_b = M.local m T.label "else_b" in 
  
  let entry_instrs = entry_instrs @  
    [ x1 <-- sgt cl_left_args (List.length args |> i32)
    ; br x1 then_b else_b
    ] in 

  let m, res          = M.local m closure_t "res" in 

  (* then branch *)
  let m, res_args_ptr = M.local m (T.ptr T.i8) "res_args" in 
  let m, total_bytes  = M.local m T.i8 "total_bytes" in 
  let m, heap_bytes   = M.local m (T.ptr T.i8) "bytes_ptr" in
  let m, cl_args_ptr  = M.local m (T.ptr T.i8) "args_ptr" in 
  let m, last         = M.local m T.opaque "__any_last" in 
  let m, then_res     = M.local m closure_t "then_res" in
  let m, else_res     = M.local m closure_t "else_res" in
  let data_t = T.structure ~packed:true (List.map args fst) |> T.ptr in
  let args_size = size_of_args args |> i8 in 

  let then_instrs = 
    [ res          <-- alloca closure_t 
    ; if is_ptr 
      then memcpy closure_ptr res (t_size closure_t |> i32) |> snd 
      else store closure_ptr res |> snd
    ; res_args_ptr <-- struct_gep res 1 
    ; total_bytes  <-- add cl_used_bytes args_size
    ; heap_bytes   <-- malloc_raw total_bytes
    ; store heap_bytes res_args_ptr |> snd
    ; memcpy cl_args heap_bytes cl_used_bytes |> snd
    ; last         <-- get_elem_ptr_raw heap_bytes [cl_used_bytes] 
    (* ; last <-- add last cl_used_bytes *)
    ; last         <-- bitcast last data_t 
    (* ; then_res <-- load res *)
    ] in 

  let cnt               = List.length args in
  let m, instrs         = set_fields m last (List.range 0 cnt) args in
  let m, left_args_ptr  = M.local m T.opaque "left_args_ptr" in 
  let m, used_bytes_ptr = M.local m T.opaque "used_bytes_ptr" in 
  let m, fn_ptr = M.local m T.opaque "used_bytes_ptr" in 

  let then_instrs = then_instrs @ instrs @ 
    [ left_args_ptr  <-- struct_gep res 2 
    ; last           <-- sub cl_left_args (i8 cnt)
    ; store last left_args_ptr |> snd
    (* increase fn pointer by number of applied args *)
    ; fn_ptr         <-- struct_gep res 0 
    ; last           <-- load fn_ptr 
    ; last           <-- gep last [cnt]
    ; store last fn_ptr |> snd

    ; used_bytes_ptr <-- struct_gep res 4 
    ; last           <-- add cl_used_bytes args_size 
    ; store last used_bytes_ptr |> snd
    ; then_res       <-- load res
    ; br1 sink_block
    ] in

  let call_args   = cl_args::(i8 cnt)::args in 
  let call_args_t = List.map call_args fst in 

  (* else branch *)
  let else_instrs = 
    [ cl_fn       <-- load cl_fn
    ; cl_fn       <-- bitcast cl_fn (T.fn closure_t call_args_t |> T.ptr)
    (* ; cl_args_ptr <-- cl_args *)
    (* ; cl_fn <-- load cl_fn *)
    ; else_res    <-- call cl_fn call_args
    ; br1 sink_block
    ] in 

  let blocks = [ block then_b then_instrs; block else_b else_instrs ] in
  m, entry_instrs, blocks, phi [then_res, then_b; else_res, else_b]

(** create closure *)
[@@@warning "-8"]
let known_apply m args raw_arity full_args raw_fn entry_fns_arr =
  let args_cnt  = List.length args in 
  (* let raw_arity = List.length full_args in *)

  printf "args_cnt: %d, raw_arity: %d\n" args_cnt raw_arity;

  if args_cnt = raw_arity 
  then (* just call function in c-style *)
    let open High_ollvm.Ast in
    let m, call_res = M.local m T.opaque "call_res" in

    let new_fn_t = List.take full_args args_cnt |> T.fn (fst raw_fn) |> T.ptr in
      
    let instrs = [ call_res <-- call !%(bitcast raw_fn new_fn_t) args ] in
    m, instrs, call_res
    
  else (* here args_cnt < raw_arity, so we need to create a closure  *)
    let m, closure_ptr   = M.local m (T.ptr closure_t) "closure" in 
    let m, args_ptr      = M.local m (T.ptr T.i8) "args_ptr" in 
                    (* TODO: full_args is probably too much, try args *)
    let data_t           = T.structure ~packed:true full_args in
    (* let m, args_ptr_cast = M.local m data_t "data_ptr_cast" in  *)
    let m, heap_bytes    = M.local m (T.ptr data_t) "malloc_data_ptr" in 

    let else_instrs = 
      [ closure_ptr   <-- alloca closure_t
      ; args_ptr      <-- struct_gep closure_ptr 1
      (* ; args_ptr_cast <-- bitcast args_ptr (T.ptr data_t) *)
      ; heap_bytes    <-- malloc data_t
      ; store !%(bitcast heap_bytes (T.ptr T.i8)) args_ptr |> snd
      ] in 

    let m, instrs1 = set_fields m heap_bytes (List.range 0 args_cnt) args in 
    let m, tmp    = M.tmp m in
    let m, fn_ptr = M.tmp m in
      (* gep m closure_ptr [3; 2; 4] in *)

    let else_instrs = else_instrs @ instrs1 @
      [ tmp <-- gep closure_ptr [0; 3] 
      ; store (i8 raw_arity) tmp |> snd
      ; tmp <-- gep closure_ptr [0; 2] 
      ; store (i8 (raw_arity - args_cnt)) tmp |> snd 
      ; tmp <-- gep closure_ptr [0; 4] 
      ; store (size_of_args args |> i32) tmp |> snd
      
      (*  *)
      ; tmp    <-- gep entry_fns_arr [0; args_cnt]
      ; tmp    <-- bitcast tmp (T.ptr (T.ptr (T.fn T.void [])))
      ; fn_ptr <-- gep closure_ptr [0; 0]
      ; store tmp fn_ptr |> snd

      ; closure_ptr <-- load closure_ptr
      ] in
    m, else_instrs, closure_ptr

[@@@warning "+8"]
