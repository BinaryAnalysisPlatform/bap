open Core_kernel
open Monads.Std

type 'a knowledge

module Knowledge : sig
  type 'a t = 'a knowledge

  type +'a cls
  type +'a obj
  type 'a value
  type ('a,'p) slot
  type 'p property
  type state
  type conflict = ..

  val collect : ('a,'p) slot -> 'a obj -> 'p t
  val provide : ('a,'p) slot -> 'a obj -> 'p -> unit t
  val promise : ('a,'p) slot -> ('a obj -> 'p t) -> unit t


  val run : 'a obj t -> state -> ('a value * state, conflict) result

  val empty : state

  include Monad.S with type 'a t := 'a t
  include Monad.Fail.S with type 'a t := 'a t
                        and type 'a error = conflict

  module Class : sig
    val declare : ?desc:string -> string -> 'a cls
    val derive : ?desc:string -> string -> 'a cls -> ('b -> 'a) cls
    val parent : (_ -> 'a) cls -> 'a cls


    type 'a ord
    val comparator : 'a cls -> (module Base.Comparable.S
                                 with type t = 'a obj
                                  and type comparator_witness = 'a ord)
  end

  module Object : sig
    type 'a t = 'a obj

    (** [create] is a fresh new object with an idefinite extent.  *)
    val create : 'a cls -> 'a obj t knowledge

    (** [scoped scope] pass a fresh new object to [scope].

        The extend of the passed object is limited with the extend
        of the function [scope].*)
    val scoped : 'a cls -> ('a obj t -> 'b knowledge) -> 'b knowledge

    (** [repr x] returns a textual representation of the object [x] *)
    val repr : 'a cls -> 'a t -> string knowledge

    (** [read s] returns an object [x] such that [repr x = s].  *)
    val read : 'a cls -> string -> 'a t knowledge
  end

  module Slot : sig
    val declare : ?public:bool -> ?desc:string ->
      string -> 'a cls -> 'p property -> ('a,'p) slot

    val get : ('a,'p) slot -> 'a value -> 'p

    val put : ('a,'p) slot -> 'a value -> 'p -> 'a value
  end

  module Value : sig
    type 'a t = 'a value [@@deriving bin_io, sexp]
    val empty : 'a cls -> 'a value
    val merge : 'a value -> 'a value -> 'a value
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
    type 'a t = (module S with type t = 'a)

    val chain : ?inspect:('a -> Base.Sexp.t) -> ('a -> 'a -> int) -> 'a t

    val map : equal:('d -> 'd -> bool) -> ('a,) Map.comparator ->
      'd Map.M()
  end


  module Property : sig
    val property : ?public:bool -> ?desc:string -> name:string ->
      'c Object.Class.t -> 'a domain -> ('c,'a) slot

  end

  module Delete_me : sig
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
    type t = semantics

    val declare :
      ?serializer:(module Binable.S with type t = 'a) ->
      name:string -> (module Domain.S with type t = 'a) -> 'a domain

    val empty : t
    val create : 'a domain -> 'a -> t
    val get : 'a domain -> t -> 'a
    val put : 'a domain -> t -> 'a -> t
    val merge : t -> t -> t

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t

    include Domain.S with type t := t
    include Binable.S with type t := t

    module Sorted(Sort : sig type 'a t end) : sig
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
end
