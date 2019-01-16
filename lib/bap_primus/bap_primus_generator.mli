open Bap_primus_types

type t [@@deriving sexp_of]

module type Generator = functor (M : Machine) -> sig
  val next : word M.t
end

val static : word -> t
val random : ?min:word -> ?max:word -> ?seed:word -> int -> t
val custom : min:word -> max:word -> int -> (module Generator) -> t

module Make(M : Machine) : sig
  val next : t -> word M.t
end
