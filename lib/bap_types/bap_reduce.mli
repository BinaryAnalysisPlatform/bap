open Bap_bil

module Like_exp : sig
  val group_like : exp -> exp
end

val group_like : bil -> bil
val reduce_consts : bil -> bil
