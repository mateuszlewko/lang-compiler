open Ast

type env = { c : Llvm.llcontext;
             m : Llvm.llmodule;
             b : Llvm.llbuilder;

            (* TODO: Change to map *)
             globals : (string * Llvm.llvalue) list;

            (* TODO: Change to map *)
             (* llvalue/llbasicblock binded to Ast.ident*)
             mem    : (Ast.ident * Llvm.llvalue) list;
             labels : (string * Llvm.llbasicblock) list }

let create_env ctx ll_mod bd = { c = ctx; m = ll_mod; b = bd; mem = [];
                                 globals = []; labels = [] }

let (>>*) m f = f m; m

let env_of_mod ll_mod = 
  let ctx = Llvm.global_context () in 
  create_env ctx ll_mod (Llvm.builder ctx)

let string_of_ident : Ast.ident -> string = 
  function ID_Local i | ID_Global i -> i

let string_of_ident_raw : Ast.ident -> string = function
  | ID_Local i  -> i ^ "_l"
  | ID_Global i -> i ^ "_g"

let (%>) f g = fun x -> g (f x)

let print_mem env =
  List.iter (fst %> string_of_ident_raw %> fun s -> 
    Logs.debug (fun m -> m "mem got: %s\n" s)) env.mem

let print_globals env =
  List.iter (fst %> fun s -> 
    Logs.debug (fun m -> m "globals got: %s\n" s)) env.globals

let lookup env id = 
  try List.assoc id env.mem
  with e ->
  try List.assoc (string_of_ident id) env.globals 
  with e ->  
    match Llvm.lookup_function (string_of_ident id) env.m with 
    | None ->
      Logs.debug (fun m -> m "not found: %s\n" (string_of_ident id));
      print_mem env;
      print_globals env;
      Core.Out_channel.flush Core.stdout;
      raise e
    | Some f -> f

let lookup_fn env (id : Ast.ident) : Llvm.llvalue = 
  match id with
  | ID_Local i  -> 
    (* Core.sprintf "Assert local id: %s in lookup_fn" i |> failwith  *)
    lookup env (ID_Local i)
  | ID_Global i -> 
    match Llvm.lookup_function i env.m with
    | Some fn -> fn
    | _       -> 
      let open Core in 
      Logs.debug (fun m -> m "ENV mem start\n");
      env.mem |> List.iter ~f:(fst %> show_ident %> 
        fun s -> Logs.debug (fun m -> m "id: %s\n" s));
      Logs.debug (fun m -> m "---\n");
      Core.sprintf "lookup_fn not found: %s" i |> failwith

let label : env -> Ast.ident -> Llvm.llbasicblock =
  fun env id -> List.assoc (string_of_ident id) env.labels

let linkage : Ast.linkage -> Llvm.Linkage.t =
  let open Llvm.Linkage
  in function
  | LINKAGE_Private               -> Private
  | LINKAGE_Internal              -> Internal
  | LINKAGE_Available_externally  -> Available_externally
  | LINKAGE_Linkonce              -> Link_once
  | LINKAGE_Weak                  -> Weak
  | LINKAGE_Common                -> Common
  | LINKAGE_Appending             -> Appending
  | LINKAGE_Extern_weak           -> External_weak
  | LINKAGE_Linkonce_odr          -> Link_once_odr
  | LINKAGE_Weak_odr              -> Weak_odr
  | LINKAGE_External              -> External

let dll_storage : Ast.dll_storage -> Llvm.Linkage.t =
  let open Llvm.Linkage
  in function
  | DLLSTORAGE_Dllimport -> Dllimport
  | DLLSTORAGE_Dllexport -> Dllexport

let visibility : Ast.visibility -> Llvm.Visibility.t =
  let open Llvm.Visibility
  in function
  | VISIBILITY_Default   -> Default
  | VISIBILITY_Hidden    -> Hidden
  | VISIBILITY_Protected -> Protected

let cconv : Ast.cconv -> int =
  let open Llvm.CallConv
  in function
  | CC_Ccc    -> c
  | CC_Fastcc -> fast
  | CC_Coldcc -> cold
  | CC_Cc i   -> assert false

let rec ll_type : env -> Ast.raw_type -> Llvm.lltype =
  fun env ->
  let ctx = env.c in
  let open Llvm
  in function
  | TYPE_I i -> begin match i with
                      | 1  -> i1_type ctx
                      | 8  -> i8_type ctx
                      | 16 -> i16_type ctx
                      | 32 -> i32_type ctx
                      | 64 -> i64_type ctx
                      | _  -> integer_type ctx i end
  | TYPE_Pointer t         -> pointer_type (ll_type env t)
  | TYPE_Void              -> void_type ctx
  | TYPE_Half              -> assert false
  | TYPE_Float             -> float_type ctx
  | TYPE_Double            -> double_type ctx
  | TYPE_X86_fp80          -> x86fp80_type ctx
  | TYPE_Fp128             -> fp128_type ctx
  | TYPE_Ppc_fp128         -> ppc_fp128_type ctx
  | TYPE_Label             -> label_type ctx
  | TYPE_Metadata          -> assert false
  | TYPE_X86_mmx           -> x86_mmx_type ctx
  | TYPE_Array (i, t)      -> array_type (ll_type env t) i
  | TYPE_Function (r, a)   ->
     function_type (ll_type env r) (Array.of_list a |> Array.map (ll_type env))
  | TYPE_Struct s          ->
     struct_type ctx (Array.of_list s |> Array.map (ll_type env))
  | TYPE_Packed_struct s   ->
     packed_struct_type ctx (Array.of_list s |> Array.map (ll_type env))
  | TYPE_Opaque            -> void_type ctx
  | TYPE_Vector (i, t)     -> vector_type (ll_type env t) i

let icmp : Ast.icmp -> Llvm.Icmp.t =
  let open Llvm.Icmp
  in function
  | Eq  -> Eq
  | Ne  -> Ne
  | Ugt -> Ugt
  | Uge -> Uge
  | Ult -> Ult
  | Ule -> Ule
  | Sgt -> Sgt
  | Sge -> Sge
  | Slt -> Slt
  | Sle -> Sle

let fcmp : Ast.fcmp -> Llvm.Fcmp.t =
  let open Llvm.Fcmp
  in function
  | False -> False
  | Oeq   -> Oeq
  | Ogt   -> Ogt
  | Oge   -> Oge
  | Olt   -> Olt
  | Ole   -> Ole
  | One   -> One
  | Ord   -> Ord
  | Uno   -> Uno
  | Ueq   -> Ueq
  | Ugt   -> Ugt
  | Uge   -> Uge
  | Ult   -> Ult
  | Ule   -> Ule
  | Une   -> Une
  | True  -> True

let ibinop : Ast.ibinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue) =
  let open Llvm
  in function
  | Add (_, _) -> build_add
  | Sub (_, _) -> build_sub
  | Mul (_, _) -> build_mul
  | UDiv _     -> build_udiv
  | SDiv _     -> build_sdiv
  | URem       -> build_urem
  | SRem       -> build_srem
  | Shl (_, _) -> build_shl
  | LShr _     -> build_lshr
  | AShr _     -> build_ashr
  | And        -> build_and
  | Or         -> build_or
  | Xor        -> build_xor

let fbinop : Ast.fbinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue) =
  let open Llvm
  in function
  | FAdd -> build_fadd
  | FSub -> build_fsub
  | FMul -> build_fmul
  | FDiv -> build_fdiv
  | FRem -> build_frem

let conversion_type : Ast.conversion_type ->
                      (Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue) =
  let open Llvm
  in function
  | Trunc    -> const_trunc
  | Zext     -> const_zext
  | Sext     -> const_sext
  | Fptrunc  -> const_fptrunc
  | Fpext    -> const_fpext
  | Uitofp   -> const_uitofp
  | Sitofp   -> const_sitofp
  | Fptoui   -> const_fptoui
  | Fptosi   -> const_fptosi
  | Inttoptr -> const_inttoptr
  | Ptrtoint -> const_ptrtoint
  | Bitcast  -> const_bitcast

(** FIXME: should be split into const/value? *)
let rec value : env -> Ast.raw_type -> Ast.value -> env * Llvm.llvalue =
  fun env ty ->
  let open Core in 
  let open Llvm in 
  function
  | VALUE_Ident i          -> env, lookup env i
  | VALUE_Integer i        -> 
    (* printf "INT TYPE: %s\n" (show_raw_type ty);
    printf "GEN INT TYPE: %s\n" (string_of_lltype (ll_type env ty));
    printf "GEN INT VAL: %s\n" (string_of_llvalue (const_int (ll_type env ty) i)); *)
    env, const_int (ll_type env ty) i
  | VALUE_Float f          -> env, const_float (ll_type env ty) f
  | VALUE_Bool b           -> env, const_int (Llvm.i1_type env.c) (if b then 1 else 0)
  | VALUE_Null             -> env, const_null (ll_type env ty)
  | VALUE_Undef            -> env, undef (ll_type env ty)
  | VALUE_Struct s         ->
     env, const_struct env.c (Array.of_list s |> Array.map ~f:(fun (ty, v) -> value env ty v |> snd))
  | VALUE_Packed_struct s  ->
     env, const_packed_struct env.c (Array.of_list s
                                |> Array.map ~f:(fun (ty, v) -> value env ty v |> snd))
  | VALUE_Array a          ->
    Logs.debug (fun m -> m "array of %s\n" (show_raw_type ty));
    (* let tty = ty in  *)
    (* Logs.debug (fun m -> m "array") *)
    let ty = 
      match ty with 
      | TYPE_Array (_, ty) -> ty 
      | ty                 -> ty in 
      
    let arr = const_array (ll_type env ty) (Array.of_list a
        |> Array.map ~f:(fun (ty, v) -> 
            (* printf "ty of arr val: %s\n" (show_raw_type ty); *)
            value env ty v |> snd)) in 
      env, arr
      (* build_array_malloc (ll_type env tty) arr "" env.b *)
  | VALUE_Vector v         ->
     env, const_vector (Array.of_list v |> Array.map ~f:(fun (ty, v) -> value env ty v |> snd))
  | VALUE_Zero_initializer -> assert false
  | JustInstr (t, i)       -> instr env i 

and values env vs = 
  let env = ref env in 
  let res = 
    Array.of_list vs
    |> Array.map (fun (t, v) -> 
                    let e, v = value !env t v in 
                    env := e; v) in 
  !env, res  

and instr =
  fun ?(res_name="") env ->
  let open Llvm in
  function

  | INSTR_IBinop (op, ty, v1, v2)       ->
     let env, v1 = value env ty v1 in
     let env, v2 = value env ty v2 in
     let op = ibinop op in
     (env, op v1 v2 "" env.b)

  | INSTR_ICmp (cmp, ty, v1, v2)        ->
     let env, v1 = value env ty v1 in
     let env, v2 = value env ty v2 in
     let cmp = icmp cmp in
     (env, build_icmp cmp v1 v2 "" env.b)

  | INSTR_FBinop (op, _, ty, v1, v2)       ->
     let env, v1 = value env ty v1 in
     let env, v2 = value env ty v2 in
     let op = fbinop op in
     (env, op v1 v2 "" env.b)

  | INSTR_FCmp (cmp, ty, v1, v2)        ->
     let env, v1 = value env ty v1 in
     let env, v2 = value env ty v2 in
     let cmp = fcmp cmp in
     (env, build_fcmp cmp v1 v2 "" env.b)

  | INSTR_Conversion (conv, ty, v, ty') ->
     let env, v = value env ty v in
     let conv = conversion_type conv in
     (env, conv v (ll_type env ty'))

  | INSTR_GetElementPtr ((t, v), tvl)       ->
     let env, indices = values env tvl in 
     (* show_tvalue  *)
     let open Core in 
     let env, llv = value env t v in
     (* printf "gep of: %s, llv is: %s, indices: [%s], llv type: %s, v: %s\n" 
      (show_value v) 
      (string_of_llvalue llv)
      (Array.fold indices ~f:(fun a v -> a ^ "; " ^ string_of_llvalue v) ~init:"")
      (type_of llv |> string_of_lltype)
      (show_value v);

     printf "---- entire module ----\n%s\n\n" (string_of_llmodule env.m); *)

     Out_channel.flush Core.stdout;
     (env, build_gep llv indices "" env.b)

  | INSTR_ExtractElement ((ty, vec), (ty', idx))      ->
     let env, vec = value env ty vec in
     let env, idx = value env ty' idx in
     (env, build_extractelement vec idx "" env.b)

  | INSTR_InsertElement ((ty, vec), (ty', el), (ty'', idx))  ->
     let env, vec = value env ty vec in
     let env, el = value env ty' el in
     let env, idx = value env ty'' idx in
     (env, build_insertelement vec el idx "" env.b)

  | INSTR_ShuffleVector ((t, v), (t', v'), (t'', v'')) ->
     let env, v = value env t v in
     let env, v' = value env t' v' in
     let env, v'' = value env t'' v'' in
     (env, build_shufflevector v v' v'' "" env.b)

  | INSTR_ExtractValue ((t, v), idx)         ->
     let env, llv = value env t v in
     let open Core in 
      (* printf "extract value of: %s, llv is: %s, llv type: %s, v: %s\n" 
      (show_value v) 
      (string_of_llvalue llv)
      (type_of llv |> string_of_lltype)
      (show_value v);

      printf "---- entire module ----\n%s\n\n" (string_of_llmodule env.m); 
      Out_channel.flush Core.stdout; *)
     env, build_extractvalue llv idx "" env.b

  | INSTR_InsertValue ((t, vec), (t', el), idx)    ->
     (* FIXME: llvm api take an int and not a list... *)
     begin match idx with
     | [ idx ] ->
        let env, vec = value env t vec in
        let env, el = value env t' el in
        (env, build_insertvalue vec el idx "" env.b)
     | _ -> assert false end

  | INSTR_Call (tail, (t, v), args)             ->
     let env, fn = 
      match v with 
      | Ast.VALUE_Ident id -> env, lookup_fn env id 
      | v                  -> value env t v
      in

     let env, args = values env args in
     let ll_call = build_call fn args res_name env.b in 
     set_tail_call true ll_call;
     env, ll_call

  | INSTR_Alloca (ty, nb, _)          -> 
       begin
       match nb with
       | None -> env, build_alloca (ll_type env ty) "" env.b
       | Some (t, nb) ->
          let env, llv = value env t nb in 
          env, build_array_alloca (ll_type env ty) llv "" env.b
       end

  | INSTR_Load (_, (t, v), _)   ->
     let env, llv = value env t v in        
     (env, build_load llv "" env.b)

  | INSTR_Phi (t, incoming)                 ->
    let env = ref env in 
    let incoming =
       List.map (fun (v, i) -> let e, v = value !env t v in 
                               env := e; v, label !env i) incoming in

     (!env, build_phi incoming "" !env.b)

  | INSTR_Select ((t, cond), (t', thenv), (t'', elsev))        ->
     let env, cond = value env t cond in
     let env, thenv = value env t' thenv in
     let env, elsev = value env t'' elsev in
     (env, build_select cond thenv elsev "" env.b)

  | INSTR_VAArg                         -> assert false
  | INSTR_LandingPad                    -> assert false

  | INSTR_Store (_, (t, v), (_, p), _) ->
     let env, v = value env t v in
     let p = lookup env p in
     (env, build_store v p env.b)

  | INSTR_Fence                         -> assert false
  | INSTR_AtomicCmpXchg                 -> assert false
  | INSTR_AtomicRMW                     -> assert false

  | INSTR_Invoke ((t, i1), tvl, (_, i2), (_, i3))   ->
     let env, args = values env tvl in
     let fn = lookup_fn env i1 in
     (env, build_invoke fn args (label env i2) (label env i3) "" env.b)

  | INSTR_Ret (t, v)                    ->
    let env, llv = value env t v in 
     (env, build_ret llv env.b)

  | INSTR_Ret_void                      ->
     (env, build_ret_void env.b)

  | INSTR_Br ((t, v), (_, tbb), (_, fbb))   ->
     let env, cond = value env t v in
     let tbb = label env tbb in
     let fbb = label env fbb in
     (env, build_cond_br cond tbb fbb env.b)

  | INSTR_Br_1 (_, i)                   ->
     (env, build_br (label env i) env.b)

  | INSTR_Switch ((t, v), (t', i), tvtil)        ->
     let env, case = value env t v in
     let elsebb = label env i in
     let count = List.length tvtil in
     let switch = Llvm.build_switch case elsebb count env.b in
     List.iter (fun ((t, v), (t', i)) ->
                Llvm.add_case switch (value env t v |> snd) (label env i))
               tvtil ;
     (env, switch)


  | INSTR_IndirectBr ((t, v), til) ->
    let env, addr = value env t v in
    let count = List.length til in
    let indirectbr = Llvm.build_indirect_br addr count env.b in
    List.iter
      (fun (_, i) -> Llvm.add_destination indirectbr (label env i)) til;
    (env, indirectbr )

  | INSTR_Resume (t, v)                 ->
     let env, llv = value env t v in
     (env, build_resume llv env.b)

  | INSTR_Unreachable                   -> (env, build_unreachable env.b)

  | INSTR_Bitcast ((t, v), ty)          ->
    let env, llv = value env t v in 
    env, build_bitcast llv (ll_type env ty) "" env.b

  | INSTR_Assign (id, inst)             ->
     let res_name   = string_of_ident id in 
     let (env, llv) = instr ~res_name env inst in
     let env = { env with mem = (id, llv)::env.mem } in
     env, llv
  | INSTR_Memcpy ((from_t, from_ptr), (to_t, to_ptr), (len_t, len), volatile) -> 
      let i8_ptr   = i8_type %> pointer_type in
      let memcpy_t = [|i8_ptr; i8_ptr; i32_type; i32_type; i1_type|]
                      |> Array.map ((|>) env.c)
                      |> function_type (void_type env.c) in

      let memcpy   = 
        declare_function "llvm.memcpy.p0i8.p0i8.i32" memcpy_t env.m in
      let volatile = const_int (i1_type env.c) (if volatile then 1 else 0) in
      let env, from_llv = value env from_t from_ptr in
      let env, to_llv   = value env to_t to_ptr in
      let env, len_llv  = value env len_t len in 
      
      env, build_call memcpy [|to_llv; from_llv; len_llv
                             ; const_int (i32_type env.c) 8; volatile|] "" env.b 
  | INSTR_Malloc v -> env, build_malloc (ll_type env v) "" env.b
  | INSTR_MallocRaw (tv, v) ->
      let i8_ptr   = i8_type %> pointer_type in
      let malloc_t = [|i32_type env.c|]
                     |> function_type (i8_ptr env.c) in

      let malloc   = declare_function "malloc" malloc_t env.m in
      let env, len_llv  = value env (TYPE_I 32) v in 
      env, build_call malloc [|len_llv|] "" env.b 
  (* | JustValue -> value env TYPE_Opaque v *)

let global : env -> Ast.global -> env =
  fun env g ->
  let v             = Core.Option.value_exn g.g_value in 
  let env, llv_init = value env g.g_typ v in
  let name          = string_of_ident g.g_ident in
  let llv           = Llvm.define_global name llv_init env.m in
  
  Llvm.set_global_constant g.g_constant llv; 
  Logs.debug (fun m -> m "adding global: %s\n" (show_ident g.g_ident));

  {env with mem = (g.g_ident, llv) :: env.mem;
            globals = (string_of_ident g.g_ident, llv)::env.globals }

let declaration : env -> Ast.declaration -> env * Llvm.llvalue =
  fun env dc ->
  let name = (string_of_ident dc.dc_name) in
  let fn =  match Llvm.lookup_function name env.m with
    | None    -> 
      Logs.debug (fun m -> m "declaring function: %s\n" name);
      Llvm.declare_function name (ll_type env dc.dc_type) env.m ;
    | Some fn -> fn in
  ({ env with globals = (name, fn)::env.globals }, fn)

let create_block : env -> Ast.block -> Llvm.llvalue -> env =
  fun env b fn ->
  if List.mem_assoc (fst b) env.labels 
  then Logs.debug 
    (fun m -> m "WARNING: block with name: %s already exists\n" (fst b));

  let llb = Llvm.append_block env.c (fst b) fn in
  { env with labels = (fst b, llb) :: env.labels }

let block : env -> Ast.block -> env =
  fun env block ->
  let bb = List.assoc (fst block) env.labels in
  Llvm.position_at_end bb env.b;
  (* process instructions *)
  let env = List.fold_left (fun env i -> instr env i |> fst) env (snd block) in
  env

let definition : env -> Ast.definition -> env =
  fun env df ->
  let (env, fn) = declaration env df.df_prototype in
  (* Do not allow function redefinition. May change? *)
  if Array.length (Llvm.basic_blocks fn) <> 0
  then (
    Logs.debug (fun m -> m "possibly function redefinition: %s\n" 
                              (string_of_ident_raw df.df_prototype.dc_name));
    flush_all ();
    (* assert false  *)
    env
  )
  else begin

  let env =
    lookup_fn env df.df_prototype.dc_name
    |> Llvm.params
    |> Array.mapi (fun i a -> (List.nth df.df_args i, a ))
    |> Array.fold_left (fun env (i, a) ->
                        Llvm.set_value_name (string_of_ident i) a;
                        { env with mem = (i, a) :: env.mem }) env in
  (* First create needed blocks.
   * [block] function will use them when building instructions. *)
  let env =
    List.fold_left (fun env b -> create_block env b fn) env df.df_instrs in
  List.fold_left (fun env bl -> block env bl) env (df.df_instrs)
  end
(* let ll_module : Ast.modul -> env =
  fun modul ->
  let c = Llvm.global_context () in
  let m = Llvm.create_module c modul.m_name in
  let b = Llvm.builder c in 
  let Ast.TLE_Target target = modul.m_target in
  let Ast.TLE_Datalayout datalayout = modul.m_datalayout in
  Llvm.set_target_triple target m;
  Llvm.set_data_layout datalayout m;
  let env = { c = c; m = m; b = b; mem = []; labels = []; globals = [] } in
  let env = List.fold_left (fun env g -> global {env with mem=[]} g)
                           env (List.map snd modul.m_globals) in
  let env = List.fold_left (fun env dc -> fst (declaration {env with mem=[]} dc))
                           env (List.map snd modul.m_declarations) in
  let env = List.fold_left (fun env df -> definition {env with mem=[];
                                                               labels=[]} df)
                           env (List.map snd modul.m_definitions) in
  { env with mem = [] ; labels = [] } *)
  
let ll_module_in ll_mod md = 
  let c = Llvm.global_context () in
  let m = ll_mod in
  let b = Llvm.builder c in 
  let Ast.TLE_Target target = md.m_target in
  let Ast.TLE_Datalayout datalayout = md.m_datalayout in
  Llvm.set_target_triple target m;
  Llvm.set_data_layout datalayout m;
  let env = { c = c; m = m; b = b; mem = []; labels = []; globals = [] } in
 
  let env = List.fold_left (fun env dc -> fst (declaration {env with mem=[]} dc))
                           env (List.map snd md.m_declarations) in
 
  let env = List.fold_left (fun env g -> global {env with mem=[]} g)
                           env (List.map snd md.m_globals) in
 
  let env = List.fold_left (fun env df -> definition {env with mem=[];
                                                               labels=[]} df)
                           env (List.map snd md.m_definitions) in
  { env with mem = [] ; labels = [] }

let ll_module md = 
  let c = Llvm.global_context () in
  let ll_mod = Llvm.create_module c md.m_name in
  ll_module_in ll_mod md 