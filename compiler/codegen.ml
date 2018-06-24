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
    let res1, lhs, env = expr env lhs in
    let res2, rhs, env = expr env rhs in
    let res   = res1 @ res2 in 
    
    (match op with
    (* operators returning int *)
    | ".+"   -> add  lhs rhs, T.i32
    | ".-"   -> sub  lhs rhs, T.i32
    | ".*"   -> mul  lhs rhs, T.i32
    | "./"   -> sdiv lhs rhs, T.i32
    (* operators returning bool *)
    | ".="   -> eq   lhs rhs, T.i1
    | ".<"   -> slt  lhs rhs, T.i1
    | ".<="  -> sle  lhs rhs, T.i1
    | ".>"   -> sgt  lhs rhs, T.i1
    | ".>="  -> sge  lhs rhs, T.i1
    | ".<>"  -> ne   lhs rhs, T.i1
    | ".&&"  -> and_ lhs rhs, T.i1
    | ".||"  -> or_  lhs rhs, T.i1
    (* raise when operator is unknown *)
    | other -> unsupp ~name:"operator" other)
    |> fun (op_res, t) -> 
      let m, v  = M.local env.m t "op_res" in
      res @ [Instr (v <-- op_res)], v, { env with m }
    
  (* let clear_subs (env : Env.t) =
    env *)
    (* { env with substitutions = BatMap.empty } *)

  let gen_let_raw (env : Env.t) expr funexp fn_t ts = 
    let { TA.args = ta_args; name; gen_name; is_rec; body } = funexp in 

    printf "fun name: %s\n" name; 
    List.iter ta_args (TA.show_arg %> printf "arg: %s\n");
    LT.show_subs env.substitutions    
    |> printf "subs here: %s\n";

    printf "AND TYPE IS: %s\n" (LT.show (Fun ts));

    let args_cnt = List.length ta_args in 
    let ret      = List.drop ts args_cnt in 
    let is_fun   = function Some LT.Fun _ -> true | _ -> false in
    
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
        Letexp.closure_entry_fns m gen_name full_args args_cnt fn 
      else (* returns value *)
        let ret = List.hd_exn ret |> LT.to_ollvm in 
        Letexp.value_entry_fns m gen_name ret full_args fn in 

    let env = { env with m } in
    let f_binding = { Env.fn = (fn, fn_t)
                    ; fns_arr
                    ; arity  = args_cnt } in 

    let add_this_fn env = Env.add env name (Fun f_binding) in

    let env = 
      match body with 
      | Some body ->
        let body_env : Env.t = 
          (if is_rec then add_this_fn env else env)
          |> fun env -> List.fold2_exn args fn_args_named ~init:env 
                              ~f:(fun env v (arg_name, t) -> 
                                    Env.add env arg_name (Val (v, t))) in 
      
        let body_env, (iss, values) = 
          List.fold_map body ~init:(body_env) 
            ~f:(fun env e -> 
                  let iss, vals, env = expr env e in 
                  env, (iss, vals))
          |> fun (body_env, lists) -> body_env, List.unzip lists in 
        
        let env = { env with m = body_env.m } in 
      
        let iss       = List.concat iss in 
        let ret_v     = List.last_exn values in 
        let ret_i     = Ez.Instr.ret ret_v |> Instr in
        let m, blocks = result_to_blocks env.m (iss @ [ret_i]) in 
        
        printf "\n--- START OF FUN --- \n";
        List.iter blocks (show_block %> printf "block: %s\n");
        printf "--- END OF FUN --- \n";

        let df        = define fn args blocks in
        { env with m = M.definition m df } 
      | None -> 
        let dl = declare fn (List.map args fst) in 
        { env with m = M.declaration m dl } in

    (* printf "add %s to env.m\n" name; *)
    env, f_binding
  
  let convert_type map t = 
    match LT.find_concrete_lt map t with 
    | subs, Some t -> subs, t
    | _   , None   -> sprintf "Couldn't find concrete type \
                         for: %s.\n" (LT.show t) 
                      |> failwith

  let gen_let (env : Env.t) expr funexp ts = 
    match ts with 
    | LT.Fun ts as fn_t -> 
      let { TA.args = ta_args; name; gen_name; is_rec; body } = funexp in 
      let args, arg_ts = List.unzip ta_args in 

      LT.show_subs env.substitutions    
      |> printf "subs here gen_let: %s\n";

      if LT.is_generic fn_t
      then begin
        printf "Exists generic for: %s\n" funexp.name;

        let poli (env : Env.t) = 
          printf "Calling poli!\n";
          printf "trying to convert types for call to: %s.\n" funexp.name;
          List.iter ts (LT.show %> printf "fn t: %s\n");
          List.iter arg_ts (LT.show %> printf "arg t: %s\n");

          let subs, ts   = 
            List.fold_map ts ~init:env.substitutions ~f:convert_type in 
          let subs, fn_t = convert_type subs fn_t in 

          (* let arg_ts = List.map arg_ts (fun t -> 
              printf "converting type: %s\n" (LT.show t);
              let x = BatMap.find_default t t map in 
              printf "got: %s\n" (LT.show x);
              x) in  *)

          let subs, arg_ts = List.fold_map arg_ts ~init:subs ~f:convert_type in
          let args   = List.zip_exn args arg_ts in 
          (* let rem_invalid c = 
            if Char.is_alpha c || c = '.' || c = '_' 
            then String.of_char c 
            else if c = ' ' then "_"
            else "" in *)

          let gen_name = funexp.gen_name ^ "_" ^ LT.mangle_name fn_t in 
                         (* (List.map arg_ts (LT.show %> sprintf "%s") 
                          |> BatString.concat "-" 
                          |> BatString.replace_chars rem_invalid) in  *)

          let env = { env with substitutions = subs } in 

          gen_let_raw env expr { funexp with args; gen_name } fn_t ts in 

        let gf = Env.GenericFun { Env.poli; mono = BatMap.empty } in 
        gf, Env.add env name gf
        end
      else 
        begin
        printf "non generic fun t is: %s\n" (LT.show fn_t);

        let env, fb = gen_let_raw env expr funexp fn_t ts in
        let b       = Env.Fun fb in
        b, Env.add env funexp.name b
        end
    | other -> sprintf "TODO let-value for: %s" funexp.name |> failwith

  let gen_top_value (env : Env.t) expr funexp ts = 
    let { TA.name; gen_name; is_rec; body; _ } = funexp in 

    printf "gen_top_value for: %s\n" name;

    LT.show_subs env.substitutions    
    |> printf "subs here: %s\n";

    (* let subs, ts = convert_type env.substitutions ts in 
    let env      = { env with substitutions = subs } in  *)

    let args   = ["unit_arg", LT.Unit] in 
    let new_ts = LT.merge [LT.Unit] ts in 
    
    printf "new_ts: %s\n" (LT.show new_ts);

    let ret_t  = LT.to_ollvm ts in 
    let tpv    = ".top_val" in 

    let new_name = name ^ tpv in 
    let new_fun  = { funexp with args; name     = new_name
                                     ; gen_name = gen_name ^ tpv } in
    let env      = gen_let env expr new_fun new_ts |> snd in

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

  let monomorphize (env : Env.t) extra_subs (generic_fun : Env.generic_fun) = 
    let prev_subs = env.substitutions in 
    let substitutions = List.fold extra_subs ~init:env.substitutions 
                          ~f:(fun m (u, v, both) -> 
                                LT.add_equality ~both u v m) in 

    let env = { env with substitutions } in 
    
    printf "doing mono\n";
    LT.show_subs env.substitutions
    |> printf "subs: %s\n";

    let env, res = generic_fun.poli env in 
    { env with substitutions = prev_subs }, res

  let gen_apply (env : Env.t) expr callee args app_t = 
    LT.show_subs env.substitutions    
    |> printf "subs here: %s\n";
    printf "callee: \n%s;\ntype: \n%s\n" (TA.show_expr_t callee) (LT.show app_t);

    let (arg_instrs, env), args = 
      List.fold_map args ~init:([], env) 
        ~f:(fun (all, env) arg ->  
              let iss, arg, env = expr env arg in
              (iss @ all, env), arg) in
     
     let env, extra_subs, callee = 
      match callee with 
      | TA.Substitute (subs, callee), _ -> 
        env, subs, callee
        (* let substitutions = List.fold subs ~init:env.substitutions 
                          ~f:(fun m (u, v, both) -> 
                                LT.add_equality ~both u v m) in 

        { env with substitutions }, callee *)
      | callee                          -> env, [], callee in 
              
    (* let _, app_t = 
      let substitutions = 
        List.fold extra_subs ~init:env.substitutions 
          ~f:(fun m (u, v, both) -> 
                LT.add_equality ~both:true u v m) in 
      
      LT.show__substitutions extra_subs
      |> printf "extra subs here: %s\n";
      convert_type substitutions app_t in  *)

    let subs, app_t = convert_type env.substitutions app_t in 
    let env = { env with substitutions = subs } in 

    List.iter args (Ez.Value.show %> printf "arg val: %s\n");

    let typed t = insert_type (LT.to_ollvm app_t) t in 

    let unknown_apply ?(is_ptr=false) (env : Env.t) iss callee = 
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
      printf "fn named: %s, has type: %s\n" name (LT.show fn_t); 
      let fn_arg_ts = match fn_t with 
                      | Fun ts -> List.take ts (List.length ts - 1)   
                                  |> List.map ~f:LT.to_ollvm 
                      | _      -> [] in


      printf "-- DEFS START --\n";
      List.iter env.m.m_module.m_definitions (fst %> printf "def: %s\n");
      printf "-- DEFS END --\n";

      let m, instrs, res = 
        Letexp.known_apply env.m args arity fn_arg_ts fn fns_arr in
      let instrs = arg_instrs @ List.map instrs (fun x -> Instr x) in
      instrs, typed res, { env with m } in 

    let rec extract_found name t =
      function
      | Fun        fb    -> of_fun_binding name env fb
      | GenericFun gf    -> 
        printf "HERE1\n";
        monomorphize env extra_subs gf |> uncurry (of_fun_binding name)
      | Val       (v, _) -> 
        let is_ptr = match fst v with 
                     | Ast.TYPE_Pointer _ -> true | _ -> false in 
        
        printf "name: %s is_ptr: %b\n" name is_ptr;

        unknown_apply ~is_ptr env [] v 
      | GlobalVar (v, _) -> 
        let m, g = M.local env.m (LT.to_ollvm t) (name ^ "_loaded_fn") in
        unknown_apply { env with m } [g <-- load v |> Instr] g 
      | Class (c, type_name) ->
          LT.show_subs env.substitutions
          |> printf "Current subs: %s\n";

          match LT.find_conc env.substitutions (Generic type_name) |> snd with 
          | None        -> sprintf "Couldn't find concrete type for: %s" 
                           type_name |> failwith 
          | Some impl_t -> 
            BatMap.keys env.classes 
            |> BatEnum.iter (Env.show_instance_key %> printf "inst: %s\n");

            try BatMap.find (c, impl_t, name) env.classes 
                |> extract_found name t 
            with Not_found -> sprintf "Instance of class %s for type %s \
                                       with member %s not found.\n" 
                                c (LT.show impl_t) name |> failwith in 

    match callee with 
    | TA.Var name, t -> extract_found name t (Env.find env name)
    | v              -> let iss, callee, env = expr env v in 
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

  let gen_exprs (env : Env.t) expr es =
    let (instrs, env), args = 
      List.fold_map es ~init:([], env) 
        ~f:(fun (all, env) arg ->  
              let iss, arg, env = expr env arg in
              (all @ iss, env), arg) in
    
    printf "After exprs\n";
    Env.print_keys env.bindings;
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
    | GenericFun gf   -> printf "HERE2\n";
                         let env, _ = monomorphize env [] gf in 
                         expr env (TA.App (var, []), t)

  let gen_substitute (env : Env.t) expr subs e t = 
    Env.show_substitutions subs
    |> printf "NEW subs here before: %s\n";
    let subs = List.filter subs (fun (u, v, _) -> u <> v) in 
    Env.show_substitutions subs
    |> printf "NEW subs here after: %s\n";
    
    let substitutions = List.fold subs ~init:env.substitutions 
                          ~f:(fun m (u, v, both) -> 
                                LT.add_equality ~both u v m) in 

    expr { env with substitutions } e 

  let gen_alloca (env : Env.t) t = 
    let t     = LT.to_ollvm t in 
    let m, al = M.local env.m (T.ptr t) "allocated" in
    [al <-- alloca t |> Instr], al, { env with m }
 
  let gen_load (env : Env.t) name = 
    match Env.find env name with 
    | Val (b, _) -> 
      let b_t = match fst b with 
                | Ast.TYPE_Pointer t -> t 
                | other              -> other in 
               
      let m, res = M.local env.m b_t "loaded" in 
      let iss    = [ Instr (res <-- load b) ] in
      iss, res, { env with m }
    | other      -> sprintf "Can't load other for name %s" name |> failwith

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
    | Substitute (subs, e)  , t -> gen_substitute env gen_expr subs e t
    | Alloca t              , _ -> gen_alloca env t 
    | Load name             , _ -> gen_load env name 

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
      
      gen_top env (TA.Fun ({ name = name; is_rec = false; args; body = Some body
                            ; gen_name = name ^ ".ext_wrapped" }, fn_t))
    | other  -> failwith "external values not supported"
  [@@@warning "+8"]

  let gen_main env main_exprs =
    let funexp = 
      { TA.name = "<main>"; gen_name = "main"; is_rec = false
      ; args = ["_", Unit]
      ; body = main_exprs @ [ TA.Lit (Int 0), LT.Int ] |> Some } in  
    gen_let env gen_expr funexp (Fun [Unit; Int]) |> snd

  let gen_class env name type_name declarations = 
    (* add bindings member_name -> class *)
    List.fold declarations ~init:env 
      ~f:(fun env f -> 
            printf "adding class binding %s -> %s\n" f name;
            Env.add env f (Env.Class (name, type_name)))

  let gen_instance env expr class_name impl_t definitions = 
    (* add binging (class_name, impl_type) -> (Map from member_name -> fun value) *)
    let add env (def, t) = 
      let b, env = gen_let env expr def t in 
      env, ((class_name, impl_t, def.name), b)
      in 

    let inner_env, defs = List.fold_map definitions ~init:env ~f:add in 
    let classes         = List.fold defs ~init:env.classes 
                            ~f:(fun cs (k, v) -> BatMap.add k v cs) in 
    
    { env with m = inner_env.m; classes }

  let gen_fun_decl (env : Env.t) {TA.gen_name; name} t = 
    match t with 
    | LT.Fun (_::_::_ as ts) as fn_t -> 
      let arity                   = List.length ts - 1 in 
      let ast_arg_ts, [ast_ret_t] = BatList.takedrop arity ts in 
      let arg_ts, ret_t           = List.map ast_arg_ts LT.to_ollvm
                                  , LT.to_ollvm ast_ret_t in 

      let m, fn    = M.global env.m ret_t gen_name in 
      let m = declare fn arg_ts |> M.declaration m in
      
      printf "adding decl of %s\n" name;

      Fun { fn = fn, fn_t; fns_arr = null; arity } 
      |> Env.add { env with m } name 
    | other  -> failwith "external values not supported"

  let rec gen_top env =
    let open TA in 
    function 
    | Fun ({TA.args = []; _} as funexp, t) -> 
      let main_expr, env = gen_top_value env gen_expr funexp t in 
      env, [main_expr]
    | FunDecl (decl, t)           -> gen_fun_decl env decl t, []
    | Instance (name, t, defs)    -> gen_instance env gen_expr name t defs, []
    | Class (name, t_name, decls) -> gen_class env name t_name decls, []
    | Fun (funexp, t)             -> gen_let env gen_expr funexp t |> snd, []
    | Extern (extern, t)          -> gen_extern env gen_top extern t    
    | other -> sprintf "NOT SUPPORTED top of: %s" (TA.show_top other)
               |> failwith

  let gen_prog ?(module_name="<stdin>") top_lvl_exprs =
    let tops = TA.of_tops top_lvl_exprs in 
    List.iter tops (TA.show_top %> printf "top: %s\n");

    let env, main_exprs = List.fold_map tops ~init:Env.empty ~f:gen_top in  
    let env             = List.concat main_exprs |> gen_main env in 

    (* printf "module: %s" (Ast.show_modul env.m.m_module); *)
    
    let llenv = LLGate.ll_module env.m.m_module in 
    llenv.m
end 