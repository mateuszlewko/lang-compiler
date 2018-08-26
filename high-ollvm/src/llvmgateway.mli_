(** Llvmgateway function. Convert Ollvm module to Llvm module. *)

(** [env] gather classic modules needed when creating a LLVM module
    and local environment. *)
type env = { c: Llvm.llcontext;
             m: Llvm.llmodule;
             b: Llvm.llbuilder;

             (** [llvalues] binded to [ident] (local variables) *)
             mem: (Ast.ident * Llvm.llvalue) list;
             (** [llbasicblocks] binded to [string] (local labels) *)
             labels: (string * Llvm.llbasicblock) list }

val linkage : Ast.linkage -> Llvm.Linkage.t

val dll_storage : Ast.dll_storage -> Llvm.Linkage.t

val visibility : Ast.visibility -> Llvm.Visibility.t

val cconv : Ast.cconv -> int

val typ : env -> Ast.typ -> Llvm.lltype

val icmp : Ast.icmp -> Llvm.Icmp.t

val fcmp : Ast.fcmp -> Llvm.Fcmp.t

val ibinop : Ast.ibinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue)

val fbinop : Ast.fbinop -> (Llvm.llvalue -> Llvm.llvalue -> string ->
                                Llvm.llbuilder -> Llvm.llvalue)

val conversion_type : Ast.conversion_type ->
                      (Llvm.llvalue -> Llvm.lltype -> Llvm.llvalue)

val value : env -> Ast.typ -> Ast.value -> Llvm.llvalue

(** [instr env i] returns [(env', v)] where [env'] is [env]
    with instruction [i] added to module [env.m] and [v] if the resulting
    value of this instruction in the [Llvm] world. *)
val instr : env -> Ast.instr -> (env * Llvm.llvalue)

val global : env -> Ast.global -> env

val declaration : env -> Ast.declaration -> env * Llvm.llvalue

(** [create_block env b fn] appends a llbasicblock to [fn] and update [env]
    to keep this block in [env.labels].
    see [definition] see [block]. *)
val create_block : env -> Ast.block -> Llvm.llvalue -> env

(** [block env b] fetch the llbasicblock corresponding previously
    associated to [b] with [create_block] function, set the position
    of the builder to add instructions to this block, and finally
    build instructions of [b].
    see [create_block] see [instr] *)
val block : env -> Ast.block -> env

(** [definition enf df] define the function [df].
    Current implementation does not allow function redefinition.
    It means that defining a function with the same name than an
    other one will produce a crash. *)
val definition : env -> Ast.definition -> env

val modul : Ast.modul -> env
