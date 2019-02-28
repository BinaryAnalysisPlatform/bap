open Bap_knowledge

type t

val addr : (t,Bitvec.t) Knowledge.slot
val name : (t,string) Knowledge.slot
val ivec : (t,int) Knowledge.slot
