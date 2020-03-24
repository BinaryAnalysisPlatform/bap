open Bap.Std
open Bap_primus_generator_types
open Bap_primus_types

type t [@@deriving sexp_of]

val of_iterator :
  ?width:int ->
  ?seed:(int -> 'a) ->
  to_bitvec:('d -> Bitvec.t) ->
  (module Iterator.Infinite
    with type t = 'a
     and type dom = 'd) -> 'a ->
  t

val create :
  ?width:int ->
  (module Iterator.Infinite
    with type t = 'a
     and type dom = int) -> 'a -> t

val static : ?width:int -> int -> t

val width : t -> int

val unfold : ?width:int -> ?min:int -> ?max:int -> ?seed:int ->
  f:('a * int -> 'a * int) -> 'a -> t

module Random : sig
  val lcg : ?width:int -> ?min:int -> ?max:int -> int -> t
  val byte : int -> t

  module Seeded : sig
    val create : ?width:int -> (int -> t) -> t
    val lcg : ?width:int -> ?min:int -> ?max:int -> unit -> t
    val byte : t
  end
end

module Make( Machine : Machine) : sig
  val next : t -> int Machine.t
  val word : t -> int -> word Machine.t
end
