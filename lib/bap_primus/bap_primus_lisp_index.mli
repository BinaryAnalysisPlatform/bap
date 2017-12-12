open Core_kernel

type ('a,'i,'e) interned = {
  data : 'a;
  id : 'i;
  eq : 'e;
}

module type S = sig
  type t
  val null : t
  val next : t -> t
  val pp : Format.formatter -> t -> unit
  include Comparable.S_plain with type t := t
end

module Make() : S
