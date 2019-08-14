open Sexplib0

type t = Bitvec.t

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t

module Functions : sig
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end
