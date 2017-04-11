(** OGRE - Open Generic Representation.


    OGRE is a self-describing data storage format.

 *)

open Core_kernel.Std
open Monads.Std

type doc
type entry
type ('a,'k) typeinfo constraint 'k = _ -> _
type ('a,'k) attribute = unit -> ('a,'k) typeinfo


(** type that describes an attribute.


    The two type variables describe the constructor and destructor
    interface. The ['a] variable, the accessor, describes how an
    attribute can be constructed. The ['s] variable, describes how an
    attribute can be packed in the database. These two types come
    along and differ only in a return type. The general form of a type
    variable is [('a -> 'r) -> 'r], where ['r] is the return type (a type of
    attribute for instance), and ['a] variable is extended every time a
    new field is added to a scheme.
*)
type ('f,'k) scheme
  constraint 'f = _ -> _
  constraint 'k = _ -> _

module Type : sig
  type 'a t
  type 'a field



  (** [scheme field] defines a scheme with one field.   *)
  val scheme : 'a field -> (('a -> 'r) -> 'r, ('a -> 'p) -> 'p) scheme


  (** [scheme $field] adds a [field] to a [scheme].

      The [scheme] had type [(('a -> 'r) -> 'r, ('a -> 'p) -> 'p) scheme],
      then the type of a resulting scheme would be
      [[(('a -> 'b -> 'r) -> 'r, ('a -> 'b -> 'p) -> 'p) scheme]], i.e., a type
      of [$field] will be attached to the scheme.


  *)
  val ($) :
    ('a -> 'b -> 'r, 'd -> 'b -> 'p) scheme -> 'b field ->
    ('a -> 'r, 'd -> 'p) scheme


  val int : int64 t
  val bool : bool t
  val string : string t
  val enum : string list -> string t

  val def  : string -> 'a t -> 'a field
  val (%:) : string -> 'a t -> 'a field
end


module Attribute : sig
  type ('a,'k) t = ('a,'k) attribute

  val define :
    desc:string ->
    name:string ->
    ('f -> 'a, 'c -> 'd) scheme -> 'f -> ('a, 'c -> 'd) typeinfo
end

(** Monadic interface to the loader docification.


    Provides a simple monadic query language with which it is possible
    to construct first class queries and computations, that can be
    applied to loader docifications.

    {[
      let segments () =
        foreach loadable >>=
        List.map ~f:(fun (loc,off) ->
            require name ~that:(belongs loc) >>= fun name ->
            request permision ~that:(belong loc) >>= fun perm ->
            return {name; off; perm; location = loc})


      let image () =
        require arch >>= fun arch ->
        segment () >>= fun seg ->



    ]}


*)
module Monad : sig
  type 'a t
  include Monad.S with type 'a t := 'a t


  (** [provide] provides the given attribute. If an attribute is
      provided several times with the same value, then it is the same as
      providing it once. If an attribute is provided several times
      with different values, then all values are stored in the
      docification. *)
  val provide : (_, 'a -> unit t) attribute -> 'a


  (** [require attr ~that:sat] requires a mandatory attribute that
      satisfies predicate [sat]. It is an error, if such attribute
      does't exist *)
  val require : ?that:('a -> bool) -> ('a,_) attribute -> 'a t

  (** [request attr ~that:sat] looks up for an attribute that
      satisfies the given predicate [sat]. It is not an error, if the
      attribute is either not provided or doesn't satisfy the
      predicate.  *)
  val request : ?that:('a -> bool) -> ('a,_) attribute -> 'a t


  (** [foreach attr ~that:sat] retrieves a list of attributes that
      satisfy the predicate [sat]. It is not an error, if such
      attributes are not provided, or none of the provided satisfy the
      predicate.  *)
  val foreach : ?that:('a -> bool) -> ('a,_) attribute -> 'a list t

  (** [run comp doc] runs a computation [comp] agains [doc]. Returns
      the value computed by the computation, on an error if it fails.  *)
  val run : 'a t -> doc -> ('a * doc) option Or_error.t

  val eval : 'a t -> doc -> 'a option Or_error.t
  val exec : 'a t -> doc -> doc option Or_error.t
end

module Doc : sig
  type t  = doc

  val empty : doc

  val load : in_channel -> doc Or_error.t

  val save : doc -> out_channel -> unit

  val from_file : string -> doc Or_error.t

  val from_string : string -> doc Or_error.t

  val to_string : doc -> string

  val to_file : doc -> string -> unit Or_error.t

  val pp : Format.formatter -> doc -> unit

  val put : ((doc -> doc Or_error.t) -> 'a) -> ('b,'c -> 'a) attribute -> 'c

  val get : doc -> ('a,_) attribute -> 'a list Or_error.t
end
