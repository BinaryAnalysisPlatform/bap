
open Bap_common
open Bap_bil

module Apply : sig
  val binop : binop -> word -> word -> word
  val unop : unop -> word -> word
  val cast : cast -> int -> word -> word
end

val bil  : ?ignore:Bap_eff.t list -> stmt list -> stmt list
val stmt : ?ignore:Bap_eff.t list -> stmt -> stmt list
val exp  : ?ignore:Bap_eff.t list -> exp -> exp
