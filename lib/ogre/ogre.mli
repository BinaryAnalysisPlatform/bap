(** OGRE - Open Generic Representation.


    OGRE is a self-describing data storage format.

 *)


open Core_kernel.Std
open Monads.Std

type t
type spec = t
type entry
type 'a attribute

type ('a,'b) scheme

module Type : sig
  type 'a t
  type 'a field

  val scheme : 'a field -> ('a -> 'b,'b) scheme
  val ($) : ('a,'b -> 'c) scheme -> 'b field -> ('a,'c) scheme


  val int : int64 t
  val bool : bool t
  val string : string t
  val enum : string list -> string t

  val def  : string -> 'a t -> 'a field
  val (%:) : string -> 'a t -> 'a field

end


module Attribute : sig
  type 'a t = 'a attribute
  type uuid

  val register :
    desc:string ->
    name:string ->
    ('f,'r) scheme -> 'f -> 'r t
end

val empty : t
val get : t -> 'a attribute -> 'a list
val add : t -> 'a attribute -> 'a -> t



(** Monadic interface to the loader specification.


    Provides a simple monadic query language with which it is possible
    to construct first class queries and computations, that can be
    applied to loader specifications.

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
      specification. *)
  val provide : 'a attribute -> 'a -> unit t


  (** [require attr ~that:sat] requires a mandatory attribute that
      satisfies predicate [sat]. It is an error, if such attribute
      does't exist *)
  val require : ?that:('a -> bool) -> 'a attribute -> 'a t


  (** [request attr ~that:sat] looks up for an attribute that
      satisfies the given predicate [sat]. It is not an error, if the
      attribute is either not provided or doesn't satisfy the
      predicate.  *)
  val request : ?that:('a -> bool) -> 'a attribute -> 'a t


  (** [foreach attr ~that:sat] retrieves a list of attributes that
      satisfy the predicate [sat]. It is not an error, if such
      attributes are not provided, or none of the provided satisfy the
      predicate.  *)
  val foreach : ?that:('a -> bool) -> 'a attribute -> 'a list t

  (** [run comp spec] runs a computation [comp] agains [spec]. Returns
      the value computed by the computation, on an error if it fails.  *)
  val run : 'a t -> spec -> 'a Or_error.t
end
