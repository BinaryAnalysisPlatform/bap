open Bap.Std
open Type
open Value

val piqi_of_intent : intent option -> Ir_piqi.intent option
val piqi_of_typeid : typeid -> Ir_piqi.typeid
val piqi_of_program_term : Program.t -> Ir_piqi.program_term
val piqi_of_blk_term : Blk.t -> Ir_piqi.blk_term 
val piqi_of_arg_term : Arg.t -> Ir_piqi.arg_term 
val piqi_of_sub_term : Sub.t -> Ir_piqi.sub_term 
val piqi_of_phi_term : Phi.t -> Ir_piqi.phi_term
val piqi_of_def_term : Def.t -> Ir_piqi.def_term
val piqi_of_jmp_term : Jmp.t -> Ir_piqi.jmp_term
val piqi_of_call : call -> Ir_piqi.call
val piqi_of_label : label -> Ir_piqi.label
val piqi_of_jmp_kind : jmp_kind -> Ir_piqi.jmp_kind 

val program_of_piqi : Ir_piqi.program_term -> Program.t
val intent_of_piqi : Ir_piqi.intent option -> intent option
val typeid_of_piqi :  Ir_piqi.typeid -> typeid
val blk_of_piqi : Ir_piqi.blk_term -> Blk.t
val arg_of_piqi : Ir_piqi.arg_term -> Arg.t
val sub_of_piqi : Ir_piqi.sub_term -> Sub.t
val phi_of_piqi : Ir_piqi.phi_term -> Phi.t
val jmp_of_piqi : Ir_piqi.jmp_term -> Jmp.t
val def_of_piqi : Ir_piqi.def_term -> Def.t
val call_of_piqi : Ir_piqi.call -> call
val label_of_piqi : Ir_piqi.label -> label
val jmp_kind_of_piqi : Ir_piqi.jmp_kind -> jmp_kind


val pb_of_program : Program.t -> string
val pb_of_blk : Blk.t -> string
val pb_of_arg : Arg.t -> string
val pb_of_sub : Sub.t -> string
val pb_of_phi : Phi.t -> string
val pb_of_def : Def.t -> string
val pb_of_jmp : Jmp.t -> string
val pb_of_intent : intent option -> string
val pb_of_typeid : typeid -> string
val pb_of_call  : call -> string
val pb_of_label : label -> string
val pb_of_jmp_kind : jmp_kind -> string

val program_of_pb : string -> Program.t
val intent_of_pb : string -> intent option
val typeid_of_pb :  string -> typeid
val blk_of_pb : string -> Blk.t
val arg_of_pb : string -> Arg.t
val sub_of_pb : string -> Sub.t
val phi_of_pb : string -> Phi.t
val jmp_of_pb : string -> Jmp.t
val def_of_pb : string -> Def.t
val call_of_pb : string -> call
val label_of_pb : string -> label
val jmp_kind_of_pb : string -> jmp_kind
