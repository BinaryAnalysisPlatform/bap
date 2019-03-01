open Core_kernel
open Monads.Std

type 'a knowledge
module Knowledge : sig
  type 'a t = 'a knowledge

  type 'a cls
  type 'a obj
  type 'a value
  type ('a,'p) slot
  type 'p domain
  type state
  type conflict = ..

  val empty : state


  val collect : ('a,'p) slot -> 'a obj -> 'p t
  val provide : ('a,'p) slot -> 'a obj -> 'p -> unit t
  val promise : ('a,'p) slot -> ('a obj -> 'p t) -> unit

  val run : unit t -> 'a cls -> 'a obj -> state -> ('a value * state, conflict) result


  include Monad.S with type 'a t := 'a t
  include Monad.Fail.S with type 'a t := 'a t
                        and type 'a error = conflict

  module Class : sig
    type 'a t = 'a cls
    type top = unit
    include Type_equal.Injective with type 'a t := 'a t

    val declare : ?desc:string -> ?package:string -> string -> 'a -> ('a -> top) cls
    val derived : ?desc:string -> ?package:string -> string -> 'a cls -> 'b -> ('b -> 'a) cls
    val upcast : (_ -> 'a) cls -> 'a cls
    val refine : 'a cls -> 'b -> ('b -> 'a) cls

    val same : 'a cls -> 'b cls -> bool

    val equal : 'a cls -> 'b cls -> ('a obj, 'b obj) Type_equal.t option
    val assert_equal : 'a cls -> 'b cls -> ('a obj, 'b obj) Type_equal.t

    val property :
      ?desc:string ->
      ?persistent:(module Binable.S with type t = 'p) ->
      ?package:string ->
      'a cls -> string -> 'p domain -> ('a,'p) slot


    val name : 'a cls -> string
    val package : 'a cls -> string
    val fullname : 'a cls -> string

    val data : ('b -> 'a) cls -> 'b

  end

  module Object : sig
    type 'a t = 'a obj
    type 'a ord

    (** [create] is a fresh new object with an idefinite extent.  *)
    val create : 'a cls -> 'a obj knowledge

    (** [scoped scope] pass a fresh new object to [scope].

        The extent of the created object is limited with the extent
        of the function [scope].*)
    val scoped : 'a cls -> ('a obj -> 'b knowledge) -> 'b knowledge

    (** [repr x] returns a textual representation of the object [x] *)
    val repr : 'a cls -> 'a t -> string knowledge

    (** [read s] returns an object [x] such that [repr x = s].  *)
    val read : 'a cls -> string -> 'a t knowledge


    (** [cast class_equality x] changes the type of an object.

        Provided with an equality of two object types, returns
        the same object [x] with a new type.

        The type equality of two object types could be obtained
        through [Class.equal] or [Class.assert_equal]. Note, this
        function doesn't do any magic, this is just the
        [Type_equal.conv], lifted into the [Object] module for
        covenience.
    *)
    val cast : ('a obj, 'b obj) Type_equal.t -> 'a obj -> 'b obj

    val comparator : 'a cls -> (module Base.Comparator.S
                                 with type t = 'a obj
                                  and type comparator_witness = 'a ord)

  end

  module Value : sig
    type 'a t = 'a value
    type 'a ord
    include Type_equal.Injective with type 'a t := 'a t

    val empty : 'a cls -> 'a value
    val join : 'a value -> 'a value -> ('a value,conflict) result
    val merge : ?on_conflict:[
      | `drop_old
      | `drop_new
      | `drop_right
      | `drop_left
      | `drop_both
    ] -> 'a value -> 'a value -> 'a value

    val cls : 'a value -> 'a cls
    val get : ('a,'p) slot -> 'a value -> 'p
    val put : ('a,'p) slot -> 'a value -> 'p -> 'a value

    val clone : 'a cls -> _ value -> 'a value

    module type S = sig
      type t [@@deriving sexp]
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end

    val derive : 'a cls -> (module S with type t = 'a t and type comparator_witness = 'a ord)
  end

  module Order : sig
    type partial = LT | EQ | GT | NC
  end

  module Domain : sig
    type 'a t = 'a domain


    (** [define ~inspect ~empty ~order name] defines a domain for the
        type ['a].

        The [empty] value denotes the representation of an absence of
        information, or an undefined value, or the default value, or
        the least possible value in the chain, etc. It's only required
        that for all possible values [x], [empty <= x], where [<=]
        is the partial order defined by the [order] parameter

        The [order] function defines the partial order for the given
        domain, such that
          - [partial x y = LT] iff [x<=y && not(y <= x)]
          - [partial x y = EQ] iff [x <= y && y <= x]
          - [partial x y = GT] iff [not (x <= y) && (y <= x)]
          - [partial x y = NC] iff [not (x <= y) && not (y <= x)].

        The optional [inspect] function enables introspection, and may
        return any representation of the domain value.
    *)
    val define :
      ?inspect:('a -> Base.Sexp.t) ->
      empty:'a ->
      order:('a -> 'a -> Order.partial) -> string -> 'a domain


    val total :
      ?inspect:('a -> Base.Sexp.t) ->
      empty:'a ->
      order:('a -> 'a -> int) ->
      string -> 'a domain

    val mapping : ?equal:('d -> 'd -> bool) ->
      ('a,'e) Map.comparator ->
      string ->
      ('a,'d,'e) Map.t domain

    val optional :
      ?inspect:('a -> Base.Sexp.t) ->
      order:('a -> 'a -> int) ->
      string -> 'a option domain

    val string : string domain
  end
end
