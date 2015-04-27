open Bap.Std

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
