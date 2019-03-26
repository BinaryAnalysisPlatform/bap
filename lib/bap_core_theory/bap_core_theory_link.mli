open Bap_knowledge

type cls

type t = cls Knowledge.value [@@deriving bin_io, compare, sexp]

val cls : cls Knowledge.cls
val addr : (cls, Bitvec.t option) Knowledge.slot
val name : (cls, string option) Knowledge.slot
val ivec : (cls, int option) Knowledge.slot

include Knowledge.Value.S
  with type t := t
   and type comparator_witness = cls Knowledge.Value.ord
