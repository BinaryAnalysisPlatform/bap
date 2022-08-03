open Bap_ir
open Bap_knowledge

val sub : sub term -> sub term
val is_transformed : sub term -> bool

module KB : sig
  val sub : sub term -> sub term knowledge
end
