open Bap.Std
open Type
open Value

module Bil : sig
  (** [to_pb p] converts [p] to protobuffer format. *)
  val to_pb : stmt -> string

  (** [to_json p] converts [p] to JSON format. *)
  val to_json : stmt -> string

  (** [to_xml p] converts [p] to XML format. *)
  val to_xml : stmt -> string

  val pb_of_stmts : stmt list -> string
  val json_of_stmts : stmt list -> string
  val xml_of_stmts : stmt list -> string

  (** all three of these functions take the name of a file
   *  previously output by Bil_piqi and read it in to get
   *  a BIL program *)
  val bil_of_pb : string -> stmt list
  val bil_of_json : string -> stmt list
  val bil_of_xml : string -> stmt list
end

module Bir : sig
  val pb_of_program : Program.t -> string
  val pb_of_blk : Blk.t -> string
  val pb_of_arg : Arg.t -> string
  val pb_of_sub : Sub.t -> string
  val pb_of_phi : Phi.t -> string
  val pb_of_def : Def.t -> string
  val pb_of_jmp : Jmp.t -> string
  val pb_of_intent : intent option -> string
  val pb_of_typeid : typeid -> string
  val pb_of_call : call -> string
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
end
