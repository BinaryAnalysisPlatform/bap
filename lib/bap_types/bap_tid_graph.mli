open Graphlib.Std
open Bap_ir

include Graph with type node = tid
               and type Node.label = tid
               and type Edge.label = tid

val start : node
val exit : node
val create : sub term -> t
