open Bap.Std
open Primus_types

module Iterator = Primus_iterator
module Random   = Primus_random


(** Generate values from a finite domain *)
module type Progress = sig
  type t
  val coverage : t -> float
end

module type Byte = sig
  type rng
  include Iterator.Infinite.S with type dom = int
  val create : rng -> t
end

module type S = sig
  type t
  type ('a,'e) m
  type policy = [`random of t option | `static of word]
                [@@deriving sexp_of]

  val lcg : int -> t

  val byte : int -> t



  module Seeded : sig
    val lcg : unit -> (t,'e) m
    val byte : unit -> (t,'e) m
  end

  val create :
    (module Iterator.Infinite.S
      with type t = 'a
       and type dom = int) -> 'a -> t

  val with_init :
    (module Iterator.Infinite.S
      with type t = 'a
       and type dom = int) -> (Context.t -> 'a) -> t


  val next : t -> (int,#Context.t) m
end
