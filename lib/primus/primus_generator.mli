
open Primus_types

module Iterator = Primus_iterator
module Random   = Primus_random

(** Generate values from a finite domain *)
module type Progress = sig
  type t
  val coverage : t -> float
end



module Uniform : sig
  module Byte : sig
    module type S = sig
      type rng
      include Iterator.Infinite.S with type dom = int
      val create : rng -> t
    end

    module Make(Rng : Random.S with type dom = int)
      : S with type rng = Rng.t

    include S with type rng = Random.LCG.t
  end
end


module Make( Machine : Machine) : sig
  type t

  val lcg : int -> t

  val byte : int -> t

  val create :
    (module Iterator.Infinite.S
      with type t = 'a
       and type dom = int) -> 'a -> t

  val with_init :
    (module Iterator.Infinite.S
      with type t = 'a
       and type dom = int) -> (Context.t -> 'a) -> t


  val next : t -> (int,#Context.t) Machine.t
end
