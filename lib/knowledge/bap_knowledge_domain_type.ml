open Sexplib

module Order = struct
  type partial = LE | EQ | GE | NC
end

module type S = sig
  type t

  val empty : t
  val partial : t -> t -> Order.partial
  val inspect : t -> Sexp.t
end

module type Eq = sig
  type t
  val equal : t -> t -> bool
end

module type Chain = sig
  type t
  val empty : t
  val inspect : t -> Sexp.t
  val compare : t -> t -> int
end
