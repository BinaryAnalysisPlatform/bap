open Core_kernel.Std

module type Generator = sig
  type t
  type dom
  val next : t -> t
  val value : t -> dom
end

module type S = sig
  include Generator
  val min : dom
  val max : dom
  val next  : t -> t
  val value : t -> dom
end

module LCG : sig
  include S with type dom = int
  val create : dom -> t
end

module Unit(U : S with type dom = int) : sig
  include S with type dom = float
  val create : U.t -> t
end


module Geometric (Dom : sig
    include Floatable
    val max_value : t
  end)(U : S with type dom = int) : sig
  include S with type dom = Dom.t
  val create : p:float -> U.t -> t
  val param : t -> float
end
