open Bap.Std
open Bap_primus_generator_types
open Bap_primus_types

type t [@@deriving sexp_of]

val create :
  (module Iterator.Infinite
    with type t = 'a
     and type dom = int) -> 'a -> t

val static : int -> t

val unfold : ?min:int -> ?max:int -> ?seed:int ->
  f:('a * int -> 'a * int) -> 'a -> t

module Random : sig
  val lcg : ?min:int -> ?max:int -> int -> t
  val byte : int -> t
  module Seeded : sig
    val create : (int -> t) -> t
    val lcg : ?min:int -> ?max:int -> unit -> t
    val byte : t
  end
end

module Make( Machine : Machine) : sig
  val next : t -> int Machine.t
end
