open Core_kernel.Std
open Primus_generator_types

module type Base = Iterator.Base

type ('a,'e) t


module Finite : sig
  module type S = Iterator.Finite

  val create : (module S with type t = 'a and type dom = 'e) -> ('a,'e) t
end

module Infinite : sig
  module type S = Iterator.Infinite

  val create : (module S with type t = 'a and type dom = 'e) -> ('a,'e) t
end


val enum : ('a,'e) t -> 'a -> 'e Sequence.t
val min : ('a,'e) t -> 'e
val max : ('a,'e) t -> 'e
val value : ('a,'e) t -> 'a -> 'e
