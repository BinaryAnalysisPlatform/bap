open Core_kernel
open Monads.Std

type 'a knowledge
type semantics [@@deriving bin_io, compare, sexp]
type 'a content
type 'a domain
type label
type state
type conflict = ..

module Knowledge : sig

  type 'a t = 'a knowledge

  val collect : 'a content -> label -> 'a t
  val provide : 'a content -> label -> 'a -> unit t
  val promise : 'a content -> (label -> 'a t) -> unit

  val run : 'a t -> state -> ('a * state, conflict) result
  val empty : state

  val domain : 'a content -> 'a domain
  val declare : ?public:bool -> ?desc:string -> name:string -> 'a domain -> 'a content


  include Monad.S with type 'a t := 'a t
  include Monad.Fail.S with type 'a t := 'a t
                        and type 'a error = conflict

end

module Label : sig
  type t = label
  val root : t

  include Base.Comparable.S with type t := label

  module Generator : sig
    val fresh : label knowledge
  end
end

module Domain : sig
  module Order : sig
    type partial = LE | EQ | GE | NC
  end

  module type S = sig
    type t

    (** no information.
        aka undefined, aka the bottom element of the domain.
        For all values [x] of the domain it should be true that
        [empty <= x], where (<=) defines the partial order *)
    val empty : t

    (** [partial x y] establishes a partial order between [x] and [y],

        Given a partial order relation [<=],
        - [partial x y = LE] iff [x<=y && not(y <= x)]
        - [partial x y = EQ] iff [x <= y && y <= x]
        - [partial x y = GE] iff [not (x <= y) && (y <= x)]
        - [partial x y = NC] iff [not (x <= y) && not (y <= x)]

        The induced partial order shall be:

        - reflexive: a <= a
        - transitive: a <= b /\ b <= c -> a <= c
        - antisymmetric: a <= b /\ b <= a -> a = b
    *)
    val partial : t -> t -> Order.partial

    (** [inspect x] returns an overview of information stored in [x],
        for introspection purposes.  *)
    val inspect : t -> Base.Sexp.t
  end


  module Map : sig
    module type Eq = sig
      type t
      val equal : t -> t -> bool
    end

    module Make(K : Base.Comparator.S)(V : Eq) :
      S with type t = V.t Base.Map.M(K).t
  end

  module Chain : sig
    module type T = sig
      type t
      val empty : t
      val inspect : t -> Base.Sexp.t
      val compare : t -> t -> int
    end

    module Make(Chain : T) : S with type t = Chain.t
  end

  module Counter : sig
    type t
    val zero : t
    val succ : t -> t
    val pp : Format.formatter -> t -> unit
    include S with type t := t
    include Base.Comparable.S with type t := t
  end

  module Label : S with type t = label
end


module Semantics : sig
  type t = semantics [@@deriving bin_io, compare, sexp]

  val declare :
    ?serializer:(module Binable.S with type t = 'a) ->
    name:string -> (module Domain.S with type t = 'a) -> 'a domain

  val empty : t
  val create : 'a domain -> 'a -> t
  val get : 'a domain -> t -> 'a
  val put : 'a domain -> t -> 'a -> t
  val merge : t -> t -> t

  include Domain.S with type t := t

  module Sorted(Sort : T1) : sig
    type 'a t                      (* semantic value *)

    val create : 'a Sort.t -> semantics -> 'a t
    val empty : 'a Sort.t -> 'a t
    val get : 'b domain -> 'a t -> 'b
    val put : 'b domain -> 'a t -> 'b -> 'a t
    val kind : 'a t -> 'a Sort.t
    val partial : 'a t -> 'a t -> Domain.Order.partial
    val merge : 'a t -> 'a t -> 'a t
    val semantics : 'a t -> semantics
  end

  val domains : unit -> string list
  val pp : Format.formatter -> t -> unit
  val pp_domains : string list -> Format.formatter -> t -> unit
end
