open Ast
open Llvm
open Core
open BatPervasives
open BatString

module StrMap = Map.Make(String)

type bound_var = {
    ll     : llvalue
  ; of_ptr : bool
  }

type environment = {
    named_vals  : bound_var StrMap.t
  ; opened_vals : bound_var StrMap.t
  ; llmod       : llmodule
  ; builder     : llbuilder
  ; ctx         : llcontext
  (* List of top level values or side-effects.
     Each pair is a function with 0 arguments and variable
     equal to result of function call (if any) *)
  ; top_vals    : (bound_var * bound_var option) list
  (* current module prefix *)
  ; mod_prefix  : string
  }

module Env =
struct

  (* Creates top-level env *)
  let create module_name =
    let ctx = create_context () in
    { named_vals  = StrMap.empty
    ; opened_vals = StrMap.empty
    ; ctx         = ctx
    ; builder     = builder ctx
    ; mod_prefix  = ""
    ; top_vals    = []
    ; llmod       = create_module ctx module_name }

  let print env =
    printf "Env:\n";
    StrMap.iter_keys env.named_vals ~f:(printf "+ %s\n");
    printf "\n";
    flush_all ()

  let name_of env raw_name = env.mod_prefix ^ raw_name

  let find_bound_var env name =
    match StrMap.find env.opened_vals (name_of env name) with
    | None  -> StrMap.find env.named_vals (name_of env name)
    | other -> other

  let find_var env name =
    find_bound_var env name |> Option.map ~f:(fun bv -> bv.ll)

  let add_var env raw_name ?of_ptr:(of_ptr=false) var =
    let name    = name_of env raw_name in
    let var     = { ll = var; of_ptr = of_ptr } in
    let add map = StrMap.set map ~key:name ~data:var in
    { env with named_vals  = add env.named_vals
             ; opened_vals = add env.opened_vals }

end

let skip_void_vals =
  Array.filter ~f:(type_of %> classify_type %> (<>) TypeKind.Void)

let kind_of = type_of %> classify_type

(* Converts type annotation to lltype *)
let annot_to_lltype ctx ?func_as_ptr:(func_as_ptr=false) =
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
                let ts  = List.filter ts ((<>) "()") in
                let ret = single_type (List.hd_exn last_t) in
                let ft  = function_type ret (Array.of_list_map ts single_type)
                in if func_as_ptr
                   then pointer_type ft
                   else ft

let get_literal ctx =
  function
  | Int i  -> const_int (i32_type ctx) i
  | Bool b -> const_int (i1_type ctx) (BatBool.to_int b)
  | Unit   -> undef (void_type ctx)
  | other  -> show_literal other |> sprintf "Unsupported literal: %s"
              |> failwith

let get_var env var_name =
  match Env.find_bound_var env var_name with
  | Some bv ->
    if bv.of_ptr
    then build_load bv.ll "load_res" env.builder
    else bv.ll
  | None   -> Env.print env;
              sprintf "Unbound variable %s" var_name
              |> failwith