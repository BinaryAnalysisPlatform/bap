(** OGRE - Open Generic Representation.


    OGRE is a self-describing data storage format.

 *)

open Core_kernel.Std
open Monads.Std
open Format

type doc
type entry
type ('a,'k) typeinfo constraint 'k = _ -> _
type ('a,'k) attribute = unit -> ('a,'k) typeinfo
type 'a seq = 'a Sequence.t
type 'a field
type 'a query


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
  val str : string t
  val float : float t

  val def  : string -> 'a t -> 'a field
  val (%:) : string -> 'a t -> 'a field
end



val declare :
  ?desc:string ->
  name:string ->
  ('f -> 'a, 'c -> 'd) scheme -> 'f -> ('a, 'c -> 'd) typeinfo


module Query : sig

  type 'a t = 'a query
  type exp
  type join
  type 'a tables

  val from : ('a,_) attribute -> (('a -> 'r) -> 'r) tables

  val (@) : _ field -> (_,_) attribute -> exp

  val select :
    ?where:exp ->
    ?join:join list list ->
    'a tables -> 'a t

  val field : ?from: (_,_) attribute -> _ field -> join
  val ($) : ('a -> 'b -> 'r) tables -> ('b,_) attribute -> ('a -> 'r) tables

  val int : int64 -> exp
  val bool : bool -> exp
  val float : float -> exp

  val (&&) : exp -> exp -> exp
  val (||) : exp -> exp -> exp
  val (==>) : exp -> exp -> exp
  val not : exp -> exp
  val (<) : exp -> exp -> exp
  val (>) : exp -> exp -> exp
  val (=) : exp -> exp -> exp
  val (<>) : exp -> exp -> exp
  val (<=) : exp -> exp -> exp
  val (>=) : exp -> exp -> exp
  val (+) : exp -> exp -> exp
  val (-) : exp -> exp -> exp
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
end



(** Monadic interface.

*)
module type S = sig
  include Monad.S
  include Monad.Trans.S with type 'a t := 'a t

  (** [require attr ~that:sat] requires a mandatory attribute that
      satisfies predicate [sat]. It is an error, if such attribute
      does't exist *)
  val require : ?that:('a -> bool) -> ('a,_) attribute -> 'a t

  (** [request attr ~that:sat] looks up for an attribute that
      satisfies the given predicate [sat]. It is not an error, if the
      attribute is either not provided or doesn't satisfy the
      predicate.  *)
  val request : ?that:('a -> bool) -> ('a,_) attribute -> 'a option t


  (** [foreach query ~f:action] *)
  val foreach : ('a -> 'b t) query -> f:'a -> 'b seq t


  (** [provide] provides the given attribute. If an attribute is
      provided several times with the same value, then it is the same as
      providing it once. If an attribute is provided several times
      with different values, then all values are stored in the
      docification. *)
  val provide : (_, 'a -> unit t) attribute -> 'a



  (** [run comp doc] runs a computation [comp] agains [doc]. Returns
      the value computed by the computation, on an error if it fails.  *)
  val run : 'a t -> doc -> ('a * doc) Or_error.t m

  val failf : ('a, formatter, unit, unit -> 'b t) format4 -> 'a

  val eval : 'a t -> doc -> 'a  Or_error.t m
  val exec : 'a t -> doc -> doc Or_error.t m
end

module Make(M : Monad.S) : S with type 'a m := 'a M.t

include S with type 'a m = 'a
           and type 'a t = 'a Make(Monad.Ident).t
           and type 'a e = doc -> ('a * doc) Or_error.t
