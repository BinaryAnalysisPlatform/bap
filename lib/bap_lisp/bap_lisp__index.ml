open Core_kernel

module type S = sig
  type t [@@deriving sexp_of]
  val null : t
  val next : t -> t
  val pp : Format.formatter -> t -> unit
  include Comparable.S_plain with type t := t
end

module Make() : S = struct
  let null = Int63.zero
  let next = Int63.succ
  include Int63
end

type ('a,'i,'e) interned = {
  data : 'a;
  id : 'i;
  eq : 'e;
}
