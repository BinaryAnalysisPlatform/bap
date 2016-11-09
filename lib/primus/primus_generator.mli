open Primus_generator_types
open Primus_types

module Random = Primus_random

(** Generate values from a finite domain *)
module type Progress = sig
  type t
  val coverage : t -> float
end

module Uniform : sig
  module Byte : sig
    module type S = Generator.Byte
    module Make(Rng : Random.S with type dom = int)
      : S with type rng = Rng.t
    include S with type rng = Random.LCG.t
  end
end

module type S = Generator

module Make( Machine : Machine) : S
  with type ('a,'e) m := ('a,'e) Machine.t
