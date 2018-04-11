open Ast
open Llvm
open Core
open BatPervasives
open BatString

module StrMap = Map.Make(String)

type val_type = {
  annot : type_annot
}

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
  (** List of top level values or side-effects.
     Each pair is a function with 0 arguments and variable
     equal to result of function call (if any) *)
  ; top_vals    : (bound_var * bound_var option) list
  (** current module prefix *)
  ; mod_prefix  : string
  }

let (>>*) v f = f v; v

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

  (* Print named_vals and opened_vals from environment *)
  let print env =
    printf "Env:\nnamed_vals\n";
    StrMap.iter_keys env.named_vals ~f:(printf "+ %s\n");
    printf "opened_vals\n";
    StrMap.iter_keys env.opened_vals ~f:(printf "* %s\n");
    printf "\n";
    flush_all ()

  let name_of env raw_name = env.mod_prefix ^ raw_name

  let find_bound_var env name =
    match StrMap.find env.opened_vals name with
    | None  -> StrMap.find env.named_vals (name_of env name)
    | other -> other

  let find_var env name =
    find_bound_var env name |> Option.map ~f:(fun bv -> bv.ll)

  let add_var env raw_name ?(of_ptr=false) var =
    let name    = name_of env raw_name in
    let var     = { ll = var; of_ptr = of_ptr } in
    let add map name = StrMap.set map ~key:name ~data:var in
    { env with named_vals  = add env.named_vals name
             ; opened_vals = add env.opened_vals raw_name }
end

let skip_void_vals =
  Array.filter ~f:(type_of %> classify_type %> (<>) TypeKind.Void)
let kind_of = type_of %> classify_type
let undef_val = undef (void_type (global_context ()))
let array_ptr = array_type (global_context () |> i32_type) 0 |> pointer_type

let size_of_lang =
  function
  | None | Some [["int"]]   -> 4
  | Some [["bool"]]         -> 1
  | Some [["int"; "array"]] -> 8
  | Some (_::_::_)          -> 8
  | Some other              ->
    List.map other (String.concat ?sep:(Some " "))
    |> String.concat ?sep:(Some " -> " ) 
    |> sprintf "Unknown type: %s" |> failwith

(* Converts type annotation to lltype *)
let annot_to_lltype ctx ?(func_as_ptr=false) =
  let single_type =
    function
    | ["int"]          -> i32_type ctx
    | ["bool"]         -> i1_type ctx
    | ["int"; "array"] -> array_ptr
      (* begin
      match BatInt32.of_string_opt x with
        | Some x -> array_type (i32_type ctx) (Int.of_int32_exn x)
        | None   -> failwith "intArray type needs integer parameter (length).
                              \nTry '4 intArray' for an array of length 4."
      end *)
    | ["()"]           -> void_type ctx
    | other            -> BatString.concat " " other
                          |> sprintf "Unsupported type: %s" |> failwith
  in
  function
  | None     -> i32_type ctx
                (* TODO: This will probably be incorrect when I start
                         implementing currying *)
  | Some []  -> failwith "Empty type (??)"
  | Some [t] -> single_type t
  | Some ts  -> let ts, last_t = List.split_n ts (List.length ts - 1) in
                let ts  = List.filter ts ((<>) ["()"]) in
                let ret = single_type (List.hd_exn last_t) in
                let ft  = function_type ret (Array.of_list_map ts single_type)
                in if func_as_ptr
                   then pointer_type ft
                   else ft

let get_var env var_name =
  match Env.find_bound_var env var_name with
  | Some bv ->
    if bv.of_ptr
    then build_load bv.ll "load_res" env.builder
    else bv.ll
  | None   -> (*Env.print env;*)
              sprintf "Unbound variable %s" var_name
              |> failwith

let gen_open env path =
  let merge ~key = function
                   | `Both (l, r)       -> Some r
                   | `Left x | `Right x -> Some x in

  let all_vars = StrMap.merge env.named_vals env.opened_vals ~f:merge in
  let opened   =
    (* Select symbols that will be opened *)
    StrMap.filter_keys all_vars ~f:(flip starts_with (path ^ "."))
    (* Remove path prefix from selected symbols *)
    |> StrMap.fold ~init:StrMap.empty
        ~f:(fun ~key ~data res ->
          let path_len = length path + 1 in
          let new_name = sub key path_len (length key - path_len) in
          StrMap.set res ~key:new_name ~data:data)
    (* Merge with previously opened symbols, possibly overwriting
       some of them *)
    |> StrMap.merge env.opened_vals ~f:merge

  in undef_val, { env with opened_vals = opened }

let global_const llmod ll_val = 
  let g = define_global "literal" ll_val llmod in
  g |> set_global_constant true;
  g

module Const = struct 
  let i32 = const_int (i32_type (global_context ()))
  let i8 = const_int (i8_type (global_context ()))

  let g_i32 bd ll_mod = i32 %> global_const ll_mod
                     %> fun v -> build_load v "g_load" bd
  let g_i8 bd ll_mod = i8 %> global_const ll_mod
                     %> fun v -> build_load v "g_load" bd
end

(** declare void @llvm.memcpy.p0i8.p0i8.i32(i8* <dest>, i8* <src>,
                                            i32 <len>, i1 <isvolatile>)
*)

(** declares: 
      declare void @llvm.memcpy.p0i8.p0i8.i32(i8* <dest>, i8* <src>,
                                              i32 <len>, i1 <isvolatile>)
  and builds call to memcpy
*)
let build_memcpy ?(is_volatile=false) dest src len builder llmod =
  let ctx      = global_context () in
  let i8_ptr   = i8_type %> pointer_type in
  let memcpy_t = [|i8_ptr; i8_ptr; i32_type; i1_type|]
                 |> Array.map ~f:((|>) ctx) 
                 |> function_type (void_type ctx) in

  let memcpy      = declare_function "llvm.memcpy.p0i8.p0i8.i32" memcpy_t 
                                     llmod in
  let is_volatile = const_int (i1_type ctx) (if is_volatile then 1 else 0) in

  build_call memcpy [|dest; src; len; is_volatile|] "" builder |> ignore

let struct_gep_load ll_val ix name bd = 
  build_struct_gep ll_val ix (name ^ "_ptr") bd
  |> fun v -> build_load v name bd
