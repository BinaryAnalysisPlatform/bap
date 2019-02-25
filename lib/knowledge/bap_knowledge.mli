open Core_kernel
open Monads.Std

type 'a knowledge
module Knowledge : sig
  type 'a t = 'a knowledge

  type +'a cls
  type +'a obj
  type 'a value
  type ('a,'p) slot
  type 'p domain
  type state
  type conflict = ..

  val empty : state


  val collect : ('a,'p) slot -> 'a obj -> 'p t
  val provide : ('a,'p) slot -> 'a obj -> 'p -> unit t
  val promise : ('a,'p) slot -> ('a obj -> 'p t) -> unit t

  val run : unit t -> (state, conflict) result

  val extract : state -> 'a cls -> 'a obj -> 'a value

  include Monad.S with type 'a t := 'a t
  include Monad.Fail.S with type 'a t := 'a t
                        and type 'a error = conflict

  module Class : sig
    val declare : ?desc:string -> ?package:string -> string -> 'a cls
    val derived : ?desc:string -> ?package:string -> string -> 'a cls -> ('b -> 'a) cls
    val parent : (_ -> 'a) cls -> 'a cls

    val name : 'a cls -> string
    val package : 'a cls -> string
    val fqname : 'a cls -> string

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

        The extent of the created object is limited with the extent
        of the function [scope].*)
    val scoped : 'a cls -> ('a obj t -> 'b knowledge) -> 'b knowledge

    (** [repr x] returns a textual representation of the object [x] *)
    val repr : 'a cls -> 'a t -> string knowledge

    (** [read s] returns an object [x] such that [repr x = s].  *)
    val read : 'a cls -> string -> 'a t knowledge
  end

  module Slot : sig
    val declare :
      ?desc:string ->
      ?public:bool ->
      ?persistent:(module Binable.S with type t = 'p) ->
      ?package:string ->
      'a cls -> string -> 'p domain -> ('a,'p) slot

    val get : ('a,'p) slot -> 'a value -> 'p
    val put : ('a,'p) slot -> 'a value -> 'p -> 'a value
  end

  module Value : sig
    type 'a t = 'a value [@@deriving bin_io, compare, sexp]
    val empty : 'a cls -> 'a value
    val merge : 'a value -> 'a value -> 'a value
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
      ?serialize : (module Binable.S with type t = 'a) ->
      empty:'a ->
      order:('a -> 'a -> Order.partial) -> string -> 'a domain


    val total :
      ?inspect:('a -> Base.Sexp.t) ->
      ?serialize : (module Binable.S with type t = 'a) ->
      empty:'a ->
      order:('a -> 'a -> int) ->
      string -> 'a domain

    val mapping : ?equal:('d -> 'd -> bool) ->
      ('a,'e) Map.comparator ->
      string ->
      ('a,'d,'e) Map.t domain

    val int : int t
    val int63 : Int63.t
    val string : string domain
  end
end
