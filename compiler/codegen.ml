open Lang_parsing.Ast
open Llvm
open Core
open BatPervasives
open BatString

module Codegen = struct 
  open High_ollvm.Ez.Value
  open High_ollvm.Ez.Instr
  open High_ollvm.Ez.Block
  open High_ollvm

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

  type block_type = 
    | Simple of Ez.Block.block 
    | Cont   of (Ez.Value.t -> Ez.Block.block)

  type result_type = 
    | Instr     of Ast.instr 
    | Block     of block_type

  let terminates = 
    function 
    | Ast.INSTR_Br _ | Ast.INSTR_Br_1 _ | Ast.INSTR_Ret _ 
    | Ast.INSTR_Ret_void -> true 
    | other              -> false

  let result_to_blocks m res = 
    let rec merge m blocks = 
      function 
      | []    , [] -> m, blocks 
      | instrs, [] -> 
        let m, b = M.local m T.label "instrs" in 
        m, (List.rev instrs |> block b |> Simple)::blocks 
      | instrs, (Instr i)::xs -> merge m blocks (i::instrs, xs)
      | []    , (Block b)::xs -> merge m (b::blocks) ([], xs)
      | last_i::_ as instrs, (Block b2)::xs -> 
        let m, b1 = M.local m T.label "instrs" in 
        let b1 = 
          if terminates last_i 
          then List.rev instrs |> block b1 |> Simple
          else (fun next -> List.rev (br1 next :: instrs) |> block b1)
               |> Cont in
          
        merge m (b2::b1::blocks) ([], xs)
      in

    let rec apply_conts m = 
      function 
      | blocks           , []                  -> m, blocks
      | last::_ as blocks, (Cont c)::xs        ->
        let b = c (label last) in 
        apply_conts m (b :: blocks, xs) 
      | []               , ((Cont _)::_ as xs) -> 
        let m, last = M.local m T.label "last_b" in 
        let last    = block last [ ret_void ] in 
        apply_conts m ([last], xs) 
      | blocks           , (Simple b)::xs      ->
        apply_conts m (b::blocks, xs)
      in

    let m, blocks = merge m [] ([], res) in 
    apply_conts m ([], blocks)
  
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
      let res1, lhs, env = expr env lhs in
      let res2, rhs, env = expr env rhs in
      let res   = res1 @ res2 in 
      let m, v  = M.tmp env.m in
      
      (match op with
      (* operators returning int *)
      | ".+"   -> add  lhs rhs
      | ".-"   -> sub  lhs rhs
      | ".*"   -> mul  lhs rhs
      | "./"   -> sdiv lhs rhs
      (* operators returning bool *)
      | ".="   -> eq   lhs rhs
      | ".<"   -> slt  lhs rhs
      | ".<="  -> sle  lhs rhs
      | ".>"   -> sgt  lhs rhs
      | ".>="  -> sge  lhs rhs
      | ".<>"  -> ne   lhs rhs
      | ".&&"  -> and_ lhs rhs
      | ".||"  -> or_  lhs rhs
      (* raise when operator is unknown *)
      | other -> unsupp ~name:"operator" other)
      |> fun op_res -> res @ [Instr (v <-- op_res)], v, { env with m }
    | _, _ ->
      failwith "Operator is missing operands"
  
  let gen_let (env : Env.t) expr funexp ts = 
    match ts with 
    | LT.Fun ts as fn_t -> 
      let { TA.args = ta_args; name; gen_name; is_rec; body } = funexp in 


      printf "fun name: %s\n" name; 
      List.iter ta_args (TA.show_arg %> printf "arg: %s\n");

      let args_cnt = List.length ta_args in 
      let ret      = List.drop ts args_cnt in 
      let is_fun = function Some LT.Fun _ -> true | _ -> false in
      
      let ret_t = if List.length ret > 1 || is_fun (List.hd ret)
                  then Letexp.closure_t else LT.to_ollvm (List.hd_exn ret) in
      
      let m, fn = M.global env.m ret_t gen_name in
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
          Letexp.closure_entry_fns m name full_args args_cnt fn 
        else (* returns value *)
          let ret = List.hd_exn ret |> LT.to_ollvm in 
          Letexp.value_entry_fns m name ret full_args fn in 

      let env = { env with m } in
      let add_this_fn env = 
        Env.add env name (Fun { fn    = (fn, fn_t)
                              ; fns_arr
                              ; arity = args_cnt }) in

      let body_env : Env.t = 
        (if is_rec then add_this_fn env else env)
        |> fun env -> List.fold2_exn args fn_args_named ~init:env 
                           ~f:(fun env v (name, t) -> 
                                  Env.add env name (Val (v, t))) in 
    
      let iss, values = 
        List.folding_map body ~init:(body_env) 
          ~f:(fun env e -> 
                let iss, vals, env = expr env e in 
                env, (iss, vals))
        |> List.unzip  in 
    
      let iss       = List.concat iss in 
      let ret_v     = List.last_exn values in 
      let ret_i     = Ez.Instr.ret ret_v |> Instr in
      let m, blocks = result_to_blocks env.m (iss @ [ret_i]) in 
      
      printf "\n--- START OF FUN --- \n";
      List.iter blocks (show_block %> printf "block: %s\n");
      printf "--- END OF FUN --- \n";

      let df        = define fn args blocks in
      let env = { env with m = M.definition m df } in 

      (* printf "add %s to env.m\n" name; *)
      add_this_fn env
    | other -> sprintf "TODO let-value for: %s" funexp.name |> failwith


  let gen_top_value (env : Env.t) expr funexp ts = 
    let { TA.name; gen_name; is_rec; body; _ } = funexp in 

    printf "gen_top_value for: %s\n" name;

    let args   = ["unit_arg", LT.Unit] in 
    let new_ts = LT.merge [LT.Unit] ts in 
    
    printf "new_ts: %s\n" (LT.show new_ts);

    let ret_t  = LT.to_ollvm ts in 
    let tpv    = ".top_val" in 

    let new_name = name ^ tpv in 
    let new_fun  = { funexp with args; name     = new_name
                                     ; gen_name = gen_name ^ tpv } in
    let env      = gen_let env expr new_fun new_ts in

    let m, g_val = M.global_val env.m (ret_t, Ast.VALUE_Null) gen_name in 
    let env      = Env.add { env with m } name (GlobalVar (g_val, ts)) in 

    let instrs = ( TA.SetVar ( name
                                  , ( App ( 
                                      ( Var new_name
                                      , new_ts )
                                    , [Lit TA.Unit, LT.Unit])
                                    , ts ))
                         , ts )  in 

    instrs, env 

  let insert_type t (_, v) = t, v 

  let monomorphize generic_fun = failwith "TODO monomorphize"

  let gen_apply (env : Env.t) expr callee args app_t = 
    let (arg_instrs, env), args = 
      List.fold_map args ~init:([], env) 
        ~f:(fun (all, env) arg ->  
              let iss, arg, env = expr env arg in
              (iss @ all, env), arg) in

    printf "callee: \n%s;\ntype: \n%s\n" (TA.show_expr_t callee) (LT.show app_t);
    List.iter args (Ez.Value.show %> printf "arg val: %s\n");

    let typed = insert_type (LT.to_ollvm app_t) in 

    let unknown_apply (env : Env.t) iss callee = 
      let m, sink_b = M.local env.m T.label "sink_b" in 
      let m, res    = M.tmp m in 
      let m, app_iss, blocks, phi_i = 
        match app_t with 
        (* application returns closure (function) *)
        | LT.Fun app_ts -> 
          Letexp.closure_apply m callee args sink_b 
        (* application returns value *)
        | app_t         -> 
          Letexp.value_apply m callee (LT.to_ollvm app_t) args sink_b in 

      let sink_b  = (fun label -> block sink_b [res <-- phi_i; br1 label])
                    |> Cont |> Block in
      let app_iss = List.map app_iss (fun x -> Instr x) in 
      let blocks  = List.map blocks (fun x -> Block (Simple x)) @ [sink_b] in
      let instrs  = arg_instrs @ iss @ app_iss @ blocks in 
      
      instrs, typed res, { env with m } in
    let open Env in 
    let of_fun_binding name env { Env.fn = (fn, fn_t); fns_arr; arity } = 
      printf "fn named: %s, has type: %s" name (LT.show fn_t); 
      let fn_arg_ts = match fn_t with 
                      | Fun ts -> List.take ts (List.length ts - 1)   
                                  |> List.map ~f:LT.to_ollvm 
                      | _      -> [] in

      let m, instrs, res = 
        Letexp.known_apply env.m args arity fn_arg_ts fn fns_arr in
      let instrs = arg_instrs @ List.map instrs (fun x -> Instr x) in
      instrs, typed res, { env with m } in 

    match callee with 
    | TA.Var name, t -> begin 
      match Env.find env name with
      | Fun        fb    -> of_fun_binding name env fb
      | GenericFun gf    -> monomorphize gf |> uncurry (of_fun_binding name)
      | Val       (v, _) -> unknown_apply env [] v 
      | GlobalVar (v, _) -> 
        let m, g = M.local env.m (LT.to_ollvm t) (name ^ "_loaded_fn") in
        unknown_apply { env with m } [g <-- load v |> Instr] g 
      end
    | v -> 
      let iss, callee, env = expr env v in 
      unknown_apply env iss callee 

  let gen_if (env : Env.t) expr (ifexp : TA.ifexp) = 
    let cond_iss, cond_res, env = expr env ifexp.cond in 
    let then_iss, then_res, env = expr env ifexp.then_body in 
    let else_iss, else_res, env = expr env ifexp.else_body in 

    let mk_block m name = M.local m T.label name in 
    let m = env.m in 

    let m, sink_b      = mk_block m "sink_b" in 
    let m, pre_then_b  = mk_block m "pre_then_b" in 
    let m, post_then_b = mk_block m "post_then_b" in 
    let m, pre_else_b  = mk_block m "pre_else_b" in 
    let m, post_else_b = mk_block m "post_else_b" in 

    let m, res = M.local m T.opaque "if-else_res" in 

    let cond_iss = cond_iss @ [br cond_res pre_then_b pre_else_b |> Instr] in 

    let of_label label = [Cont (fun next -> block label [br1 next]) |> Block] in 
    let pre_then_iss   = of_label pre_then_b in 
    let pre_else_iss   = of_label pre_else_b in 

    let post_then = [Simple (block post_then_b [br1 sink_b]) |> Block] in 
    let post_else = [Simple (block post_else_b [br1 sink_b]) |> Block] in 
   
    let sink = 
      [(fun next -> block sink_b [ res <-- phi [ then_res, post_then_b
                                               ; else_res, post_else_b ]
                                 ; br1 next ]) 
       |> Cont |> Block] in 
    
    let blocks = cond_iss @ 
                 pre_then_iss @ then_iss @ post_then @ 
                 pre_else_iss @ else_iss @ post_else @ 
                 sink in 

    blocks, res, { env with m}

  let gen_exprs env expr es =
    let (instrs, env), args = 
      List.fold_map es ~init:([], env) 
        ~f:(fun (all, env) arg ->  
              let iss, arg, env = expr env arg in
              (iss @ all, env), arg) in
    instrs, List.last_exn args, env

  let gen_value env expr name e t = 
    let iss, v, env = expr env e in 
    let env         = Env.add env name (Env.Val (v, t)) in 
    iss, v, env

  let gen_set_var env expr name e = 
    let iss, src, env = expr env e in 
    let iss = 
      let fail s = sprintf "setting value to %s: %s is not supported" s name 
                   |> failwith in 

      match Env.find env name with 
      | Val (dest, _) | GlobalVar (dest, _) -> 
        iss @ [ Instr (store src dest |> snd) ]
      | Fun _         -> fail "fun"
      | GenericFun _  -> fail "generic fun" in

    iss, src, env

  let gen_gep_load (env : Env.t) expr e ixs t = 
    let iss, e, (env : Env.t) = expr env e in 
    let m, res                = M.local env.m (LT.to_ollvm t) "gep_load" in 
    let iss = iss @ [ Instr (res <-- gep e ixs); Instr (res <-- load res) ] in
    iss, res, { env with m }

  let gen_gep_store (env : Env.t) expr (gep_store : TA.gep_store) t = 
    let iss_src , src , env = expr env gep_store.src in 
    let iss_dest, dest, env = expr env gep_store.dest in 
    let m, ptr = M.local env.m (T.ptr T.opaque) "gep_ptr" in 
    let iss = iss_src @ iss_dest @
      [ ptr <-- gep dest gep_store.idx |> Instr
      ; store src ptr |> snd           |> Instr ] in

    iss, dest, { env with m }

  let gen_record_lit (env : Env.t) expr fields t = 
    let ot = LT.to_ollvm ~is_arg:false t in 
    let (fields_instrs, env), field_vals = 
      List.fold_map fields ~init:([], env) 
        ~f:(fun (all, env) arg ->  
              let iss, arg, env = expr env arg in
              (iss @ all, env), arg) in

    let m, rec_ptr = M.local env.m (T.ptr ot) "record_ptr" in 
    let is_ptr = function Ast.TYPE_Pointer _ -> true | _ -> false in 
    let ptrs, field_vals = 
      List.fold_mapi field_vals ~init:[] 
                                ~f:(fun i ptrs (ft, _ as f) -> 
                                      if is_ptr ft
                                      then (f, i)::ptrs, null_t ft
                                      else ptrs   , f) in 

    let s = structure ~packed:true field_vals in 
    let iss = fields_instrs @ 
      [ rec_ptr <-- malloc ot  |> Instr 
      ; store s rec_ptr |> snd |> Instr
      ] in 

    let m, isss = 
      List.fold_map ptrs ~init:m 
        ~f:(fun m (ptr, i) -> 
          let m, dest = M.tmp m in 
          let iss = [ dest <-- struct_gep rec_ptr i |> Instr 
                    ; store ptr dest |> snd         |> Instr ] in 
          m, iss) in 

    let iss = iss @ List.concat isss in 
    iss, rec_ptr, { env with m }

  let gen_clone (env : Env.t) expr e t =
    let iss, src_raw, (env : Env.t) = expr env e in 
    
    let m, dest = M.local env.m (LT.to_ollvm t) "clone_ptr" in
    let m, src  = M.local m (T.ptr T.i8) "src_ptr" in

    let t_raw   = LT.to_ollvm ~is_arg:false t in 
    let iss     = iss @ 
      List.map ~f:(fun x -> Instr x) 
        [ dest <-- malloc t_raw                        
        ; dest <-- bitcast dest (T.ptr T.i8)           
        ; src  <-- bitcast src_raw (T.ptr  T.i8)          
        ; memcpy ~src ~dest (t_size t_raw |> i32) |> snd 
        ; dest <-- bitcast dest (LT.to_ollvm t)        
        ] in 

    iss, dest, { env with m }

  let gen_var env expr var v t = 
    match Env.find env v with 
    | Fun f                -> expr env (TA.App (var, []), t)
    | GlobalVar (g_var, _) -> 
      let m, g = M.local env.m (LT.to_ollvm t) (v ^ "_loaded") in
      [g <-- load g_var |> Instr], g, { env with m }
    | Val b           -> [], fst b, env 
    | GenericFun gf   -> let env, _ = monomorphize gf in 
                         expr env (TA.App (var, []), t)

  let rec gen_expr env = 
    function 
    | TA.SetVar (name, e)   , _ -> gen_set_var env gen_expr name e 
    | (TA.Var v, t) as var      -> gen_var env gen_expr var v t 
    | Lit    l              , _ -> gen_literal env gen_expr l 
    | If ifexp              , _ -> gen_if env gen_expr ifexp 
    | Exprs es              , _ -> gen_exprs env gen_expr es 
    | Value   (name, e)     , t -> gen_value env gen_expr name e t
    | App     (callee, args), t -> gen_apply env gen_expr callee args t 
    | InfixOp (op, lhs, rhs), _ -> gen_op env gen_expr lhs rhs op
    | GepLoad (e, ixs)      , t -> gen_gep_load env gen_expr e ixs t
    | RecordLit fields      , t -> gen_record_lit env gen_expr fields t
    | Clone e               , t -> gen_clone env gen_expr e t
    | GepStore gep_s        , t -> gen_gep_store env gen_expr gep_s t
    | Substitute (subs, e)  , t -> failwith "TODO Substitute"

  [@@@warning "-8"]
  let gen_extern (env : Env.t) gen_top {TA.name; gen_name} t =
    let open TA in 
    match t with 
    | LT.Fun (_::_::_ as ts) as fn_t -> 
      let arity           = List.length ts - 1 in 
      let ast_arg_ts, [ast_ret_t] = BatList.takedrop arity ts in 
      let arg_ts, ret_t   = List.map ast_arg_ts LT.to_ollvm
                          , LT.to_ollvm ast_ret_t in 

      let m, fn    = M.global env.m ret_t gen_name in 
      let m        = declare fn arg_ts |> M.declaration m in 
      let ext_name = name ^ ".external" in
      let env      = Env.add { env with m } ext_name 
                        (Fun { fn = fn, fn_t; fns_arr = null; arity }) in 
      
      let args       = List.mapi ast_arg_ts (fun i t -> string_of_int i, t) in 
      let apply_args = List.map args (fun (a, t)     -> Var a, t) in 
      let body       = [App ((Var ext_name, fn_t), apply_args), ast_ret_t] in 
      
      gen_top env (TA.Fun ({ name = name; is_rec = false; args; body
                            ; gen_name = name ^ ".ext_wrapped" }, fn_t))
    | other  -> failwith "external values not supported"
  [@@@warning "+8"]

  let gen_main env main_exprs =
    let funexp = 
      { TA.name = "<main>"; gen_name = "main"; is_rec = false
      ; args = ["_", Unit]; body = main_exprs @ [ TA.Lit (Int 0), LT.Int ] } in  
    gen_let env gen_expr funexp (Fun [Unit; Int])

  let rec gen_top env =
    let open TA in 
    function 
    | Fun ({TA.args = []; _} as funexp, t) -> 
      let main_expr, env = gen_top_value env gen_expr funexp t in 
      env, [main_expr]
    | Fun (funexp, t)  -> gen_let env gen_expr funexp t, []
    | Extern (extern, t) -> gen_extern env gen_top extern t    
    | other -> sprintf "NOT SUPPORTED top of: %s" (TA.show_top other)
               |> failwith

  let gen_prog ?(module_name="<stdin>") top_lvl_exprs =
    let tops = TA.of_tops top_lvl_exprs in 
    (* List.iter tops (TA.show_top %> printf "top: %s\n"); *)

    let env, main_exprs = List.fold_map tops ~init:Env.empty ~f:gen_top in  
    let env             = List.concat main_exprs |> gen_main env in 

    (* printf "module: %s" (Ast.show_modul env.m.m_module); *)
    
    let llenv = LLGate.ll_module env.m.m_module in 
    llenv.m
end 