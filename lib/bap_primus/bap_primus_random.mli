open Core_kernel

module Iterator = Bap_primus_iterator


module LCG : sig
  module type S = sig
    include Iterator.Infinite.S with type dom = int
    val create : dom -> t
  end
  include S
end


module Unit : sig
  module type S = sig
    include Iterator.Infinite.S with type dom = float
    type u
    val create : u -> t
  end
  module Make (U : Iterator.Infinite.S with type dom = int)
    : S with type u = U.t
  include S with type u = LCG.t
end

module Geometric : sig
  module type S = sig
    include Iterator.Infinite.S
    type u
    val create : p:float -> u -> t
    val param : t -> float
  end

  module type Dom = sig
    include Floatable
    val max_value : t
  end

  module Make(D : Dom)(U : Iterator.Infinite.S with type dom = int) :
    S with type dom = D.t and type u = U.t

  module Float : S with type dom = float and type u = LCG.t
  module Int   : S with type dom = int   and type u = LCG.t
end



module Byte : sig
  module type S = sig
    include Iterator.Infinite.S with type dom = int
    type rng
    val create : rng -> t
  end
  module Make(Rng : Iterator.Infinite.S with type dom = int)
    : S with type rng = Rng.t
  include S with type rng = LCG.t
end
