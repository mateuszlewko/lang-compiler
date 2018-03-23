open Ast
open Llvm
open Core
open BatPervasives
open BatString
open CodegenUtils 
open Core.Option

(* module RecordGen : sig
  val create : string -> context -> unit
end = 
struct
  let message = "Hello"
  let hello () = print_endline message
end *)

type lang_record = 
  { name   : string 
  ; fields : int StrMap.t  
  }

module RecordGen = struct 
  
  let build_create name ctx ?(packed=false) fields = 
    let st = named_struct_type ctx name in 
    let f_names, f_types = Array.unzip fields in
    struct_set_body st f_types packed;

    let fields = Array.mapi f_names (fun i n -> n, i) |> Array.to_list in
    { name = name; fields = StrMap.of_alist_exn fields }

  let build_get_field ?(of_ptr=false) ?(out_name="") record value field_name 
                      builder = 
    StrMap.find record.fields field_name
    >>| fun ix -> let get = if of_ptr then build_struct_gep 
                                      else build_extractvalue
                  in get value ix out_name builder 
                  
end