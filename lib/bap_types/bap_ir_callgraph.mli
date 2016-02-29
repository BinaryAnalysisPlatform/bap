open Graphlib.Std
open Bap_ir

include Graph with type node = tid
               and type Node.label = tid
               and type Edge.label = jmp term list


val create : program term -> t
val pp : Format.formatter -> t -> unit
