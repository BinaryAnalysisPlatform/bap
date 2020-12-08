open Bap_core_theory

type exp = X86_legacy_bil.Ast.exp

module Binary : sig
  val fextract :
    ?bias:exp -> Theory.IEEE754.parameters -> exp -> hi:int -> lo:int -> exp
  val unbiased_exponent : Theory.IEEE754.parameters -> exp -> exp
  val fraction : Theory.IEEE754.parameters -> exp -> exp
  val sign : Theory.IEEE754.parameters -> exp -> exp
  val is_nan : Theory.IEEE754.parameters -> exp -> exp
  val mk_nan : Theory.IEEE754.parameters -> exp -> exp
end

val init : unit -> unit
