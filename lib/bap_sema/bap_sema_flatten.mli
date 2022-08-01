open Bap_ir
open Bap_knowledge

val flatten_sub : sub term -> sub term
val flatten_blk : blk term -> blk term

module KB : sig
  val flatten_sub : sub term -> sub term knowledge
  val flatten_blk : blk term -> blk term knowledge
end
