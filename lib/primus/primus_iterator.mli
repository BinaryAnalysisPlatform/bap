open Core_kernel.Std

module type Base = sig
  type t
  type dom
  val min : dom
  val max : dom
  val value : t -> dom
end

type ('a,'e) t


module Finite : sig
  module type S = sig
    include Base
    val next : t -> t option
  end

  val create : (module S with type t = 'a and type dom = 'e) -> ('a,'e) t
end

module Infinite : sig
  module type S = sig
    include Base
    val next : t -> t
  end

  val create : (module S with type t = 'a and type dom = 'e) -> ('a,'e) t
end


val enum : ('a,'e) t -> 'a -> 'e Sequence.t
val min : ('a,'e) t -> 'e
val max : ('a,'e) t -> 'e
val value : ('a,'e) t -> 'a -> 'e
