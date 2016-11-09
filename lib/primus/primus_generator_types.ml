open Bap.Std

module Iterator = struct
  module type Base = sig
    type t
    type dom
    val min : dom
    val max : dom
    val value : t -> dom
  end

  module type Finite = sig
    include Base
    val next : t -> t option
  end

  module type Infinite = sig
    include Base
    val next : t -> t
  end
end


module Generator = struct
  (** Generate values from a finite domain *)
  module type Progress = sig
    type t
    val coverage : t -> float
  end

  module type Byte = sig
    type rng
    include Iterator.Infinite with type dom = int
    val create : rng -> t
  end
end
