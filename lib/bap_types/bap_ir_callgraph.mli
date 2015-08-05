open Bap_graph_intf
open Bap_graph_regular_intf
open Bap_ir

include Graph with type node = tid
               and type Node.label = tid
               and type Edge.label = jmp term list


val create : program term -> t
val pp : Format.formatter -> t -> unit

include Aux with type node := node
