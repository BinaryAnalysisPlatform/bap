open Core_kernel.Std
open Bap_primus_types

module Iterator = Bap_primus_iterator
module type S = Iterator.Infinite.S

module LCG = struct
  module type S = sig
    include S with type dom = int
    val create : dom -> t
  end
  module M31 = struct
    type t = int
    type dom = int
    type seed = int
    let a = 0x41C64E6D
    let c = 12345
    let m = 31
    let min = 0
    let max = 1 lsl m - 1
    let next x = (a * x + c) mod (max + 1)
    let create seed = next seed
    let value = ident
  end
  let t = Iterator.Infinite.create (module M31)
  include M31
end

module Unit = struct

  module type S = sig
    include S with type dom = float
    type u
    val create : u -> t
  end
  module Make(Rng : Iterator.Infinite.S with type dom = int)
    : S with type u = Rng.t =
  struct
    module Gen = struct
      type dom = float
      type u = Rng.t
      type t = {
        base : u;
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
    let t = Iterator.Infinite.create (module Gen)
    include Gen
  end
  include Make(LCG)
end

module Geometric = struct

  module type Dom = sig
    include Floatable
    val  max_value : t
  end

  module type S = sig
    include S
    type u
    val create : p:float -> u -> t
    val param : t -> float
  end

  module Make
      (Dom : Dom)
      (Uniform : Iterator.Infinite.S with type dom = int) :
    S with type u = Uniform.t
       and type dom = Dom.t = struct
    module Gen = struct
      type dom = Dom.t
      type u = Uniform.t
      module Unit = Unit.Make(Uniform)
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
        let log1mp = Float.log1p(-.p) in
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
    let t = Iterator.Infinite.create (module Gen)
    include Gen
  end

  module Float = Make(Float)(LCG)
  module Int   = Make(Int)(LCG)
end


module Byte = struct
  module type S = sig
    include Iterator.Infinite.S with type dom = int
    type rng
    val create : rng -> t
  end
  module Make(Rng : Iterator.Infinite.S with type dom = int)
  = struct
    type dom = int
    type rng = Rng.t
    type t = Rng.t

    let min = 0
    let max = 255
    let size = max - min + 1
    let create = ident
    let next = Rng.next
    let value rng = Rng.value rng mod size
  end
  module Basic = Make(LCG)
  include Basic
end
