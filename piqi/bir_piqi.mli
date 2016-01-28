open Bap.Std

type fmt = [ `json | `pb | `piq | `pib | `xml ]


val program_of_string : fmt -> string -> program term
val sub_of_string     : fmt -> string -> sub term
val blk_of_string     : fmt -> string -> blk term
val arg_of_string     : fmt -> string -> arg term
val phi_of_string     : fmt -> string -> phi term
val def_of_string     : fmt -> string -> def term

val string_of_program : fmt -> program term -> string
val string_of_sub     : fmt -> sub term -> string
val string_of_blk     : fmt -> blk term -> string
val string_of_arg     : fmt -> arg term -> string
val string_of_phi     : fmt -> phi term -> string
val string_of_def     : fmt -> def term -> string
