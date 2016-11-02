open Core_kernel.Std
open Primus_types

module type Generator = sig
  type t
  type dom
  val next : t -> t
  val value : t -> dom
end

module type Rng = sig
  include Generator
  val min : dom
  val max : dom
  val next  : t -> t
  val value : t -> dom
end

module type S = Rng


module LCG : sig
  include Rng with type dom = int
  val create : dom -> t
end= struct
  type t = int
  type dom = int
  type seed = int
  let a = 1103515245
  let c = 12345
  let m = 31
  let min = 0
  let max = 1 lsl m - 1
  let next x = (a * x + c) mod (max + 1)
  let create seed = next seed
  let value = ident
end


module Unit(Rng : Rng with type dom = int) : sig
  include Rng with type dom = float
  val create : Rng.t -> t
end = struct
  type dom = float

  type t = {
    base : Rng.t;
    value : float;
  }
  let factor = 1. /. (float Rng.max -. float Rng.min +. 1.)

  let rec next {base;value} =
    let base = Rng.next base in
    let value =
      (float (Rng.value base) -. float Rng.min) *. factor in
    if value < 1. then {base; value}
    else next {base;value}

  let value t = t.value
  let min = 0.0
  let max = 1.0
  let create base = next {base ; value=1.0}
end

module Geometric(Dom : sig
    include Floatable
    val  max_value : t
  end)(Uniform : Rng with type dom = int) : sig
  include Rng with type dom = Dom.t
  val create : p:float -> Uniform.t -> t
  val param : t -> float
end = struct
  type dom = Dom.t
  module Unit = Unit(Uniform)
  type t = {
    rng : Unit.t;
    value  : dom;
    log1mp  : float;
    p : float;
  }

  let next t =
    let rng = Unit.next t.rng in
    let x = 1.0 -. Unit.value rng in
    let y = Float.round (log x /. t.log1mp) in
    if y >= Dom.to_float Dom.max_value
    then {t with rng; value = Dom.max_value}
    else {t with rng; value = Dom.of_float y}

  let create ~p rng =
    if p < 0. || p > 1.
    then invalid_arg
        "Geometric distribution: \
         parameter p must be in (0,1] interval";
    let log1mp = log1p(-.p) in
    if log1mp = 0.0 then invalid_arg
        "Geometric distribution: \
        parameter p is too small";
    next {
      rng = Unit.create rng;
      value = Dom.of_float 0.;
      log1mp;
      p;
    }

  let value t = t.value

  let min = Dom.of_float 0.
  let max = Dom.max_value
  let param t = t.p
end


let enum (type t) (type dom)
    (module Rng : Generator with type t = t and type dom = dom) rng =
  Sequence.unfold ~init:rng ~f:(fun t -> Some (Rng.value t, Rng.next t))
