(** OGRE - Open Generic Representation.

    OGRE is a self-describing data storage. It is open for
    extensibility, i.e., adding new types of knowledge doesn't break
    the storage. It also has a well-specified open representation, so
    that any tool written in any language (or even a human itself) can
    create, modify and understand the contents (like XML). Ogre
    provides data persitance and, more importantly, a type safe way of
    querying and updating the data. The query language is rich enough,
    and supports joins and boolean constraints

    It can be seen as a document NoSQL database engine. As a backing
    storage Ogre uses S-Expressions, and the structure of a document
    is close to JSON. In fact it is a restricted subset of JSON, where
    only scalar values are allowed.

    A database, called a "document" (or just "doc") in Ogre parlance,
    is a set of facts. Each fact is described with a proposition
    having the following syntax,

    {v (<attribute-name> <v1> <v2> ... <vM> ) v}

    where [<attribute-name>] is a name of a proposition and [<vN>] is
    the value of [N]'th object (or subject) of a proposition. For
    example,

    {v (student (name Joe) (gpa 3.5)) v}

    is a proposition that a student named [Joe] has a GPA rate
    [3.5]. Thus a proposition is a tuple with named fields. All
    propositions must be well-typed, so the predicate [student] should
    be declared before used. The field values maybe stored (and are by
    default) without the names in the order in which they are
    specified in the declaration, e.g., the following definition is
    equivalent to the previous one:

    {v (student Joe 3.5) v}

    Given, that the predicate is declared as:

    {v (declare student (name str) (gpa float))v}

    where the declaration has the following syntax:

    {v
       <declaration ::= (declare <attribute-name> <field> <field>..)
       <field> ::= (<field-name> <field-type>)
       <field-type> ::= int | str | bool | float
    v}

    Each declaration declare an attribute, that defines a type of the
    propositions. Unlike the SQL, we denote each tuple type with the
    word attribute, as under our model each document describes some
    knowledge (an attribute) about some abstract entity. For example,
    a document "college.ogre" that contains definitions of attributes
    named [student], [teacher], [class], [assignments] is a set of
    knowledge about a college. Thus an attribute maps to a SQL notion
    of table (or a relvar). Correspondingly, a column of a table (that
    is usually referred as an attribute in the relational model), maps
    to Ogre's field.



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
  val ($) : ('a -> 'b -> 'r) tables -> ('b,_) attribute -> ('a -> 'r) tables


  val select :
    ?where:exp ->
    ?join:join list list ->
    'a tables -> 'a t

  val field : ?from: (_,_) attribute -> _ field -> join

  module Array : sig
    val get : (_,_) attribute -> _ field -> exp
  end

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


(** Monadic interface to the document.

    This interface exposes actual setters and getters, that should be
    used to query data or to store it in a document. The interface is
    provided by a monad transformer functor, that wraps the Ogre monad
    into an arbitrary user monad.

    Underneath the hood, the Ogre monad is a state monad wrapped into
    an error monad. The state holds the document, so that it is not
    needed to pass it along all the functions, and the error monad
    facilitates error propagation. We do not expose the [catch] function
    from the standard Error monad interface, as we are concerned that
    it will be abused (we don't see any legitimate usage of this
    function in the use cases of Ogre that we perceive).

    The interface is built around 3 accessors functions for data
    querying and one mutator for inserting data into a document.

    {3 Data querying}

    In our model, a document is a set of facts, that describe some
    abstract entity. We're making a deriviations based on facts. To
    make a deriviation we need some facts, the particular set of
    required facts, as well as the modality of the requirement (i.e.,
    how much it is needed), depends on the particular deriviation. For
    example, we might {!require} a fact to be present or, if it is
    optional, we might {!request} it, finally, we may query for a
    collection of facts, and apply our deriviation {!foreach} fact. In
    other terms, [require attribute] mandates that there is one and
    only one value of the given [attribute], [request attribute] asks
    for zero or one value, and [foreach attribute] requires zero or more
    values of the [attribute].
*)
module type S = sig
  include Monad.S
  include Monad.Trans.S with type 'a t := 'a t

  (** [require a ~that:p] requires that an attribute [a] has one and
      only one value that satisfies a predicate [p]. It is an error,
      if there are no such values, or if there are more than one value.*)
  val require : ?that:('a -> bool) -> ('a,_) attribute -> 'a t

  (** [request a ~that:p] request no more than one value of an
      attribute [a], that satisfies a predicate [p]. The returned
      value is wrapped in an option. If there are more than one
      satisfying value, then it is an error. *)
  val request : ?that:('a -> bool) -> ('a,_) attribute -> 'a option t

  (** [foreach query ~f:action] applies an [action] for each value of
      an attributes specified in the query. The [query] value is built
      using a domain specific language embedded into OCaml. This
      language is very similiar to SQL, and has join and where
      clauses, e.g.,

      {[
        let better_than_average_students =
          foreach Query.(begin
              select
                ~where:(students.(gpa) > float 3.5)
                ~join:[
                  [field classid];
                  [
                    field teacher ~from:students;
                    field id ~from:teachers
                  ]]
                (from students $ teachers)
            end)
            ~f:(fun s t -> return (s,t))
      ]}

      The type of the [query] value encodes the type of the function
      [f]. A well formed query has a type of form
      [(t1 -> t2 -> .. -> tm -> 'a t) -> 'a t], where [t1] till [tm] are
      types of attributes enumerated in the [from clause] (in that
      particular order).

      See the [Query] module documentation for more information about
      the query EDSL.*)
  val foreach : ('a -> 'b t) query -> f:'a -> 'b seq t


  (** [provide attr v1 v2 ... vm] stores the constituents of an
      attribute value in the document. An attribute type encodes not
      only the type of an attribute value, but also a type and the
      order of the fields. Thus, the [attribute] itself captures a
      format of the attribute representation, the same as [format] is
      used in printf-like functions. In that sense, the [provide]
      function is variadic, where the first argument (the attribute)
      defines the type and the arity of the function.*)
  val provide : (_, 'a -> unit t) attribute -> 'a


  (** [fail error] aborts an inference process with the specified
      [error].  *)
  val fail : Error.t -> 'a t


  (** [failf fmt args... ()] constructs an error based on the
      specified format [fmt] and arguments, terminated by the unit value
      [()]. Example:

      {[
        failf "the file type %s is unsupported" name ()
      ]}

      Note: don't forget to terminate a sequence of arguments with an
      extra unit value. See the corresponding [invalid_argf] and
      [failwithf] function for the reason, why this extra argument is
      needed.*)
  val failf : ('a, formatter, unit, unit -> 'b t) format4 -> 'a

  (** [eval property document] makes an inference of a [property] based
      on facts stored in a [document]. If all requirements are
      satisfied and no errors occured the inferred result.

      For example, given the property [names_of_best_students],
      defined as,

      {[
        let names_of_best_students =
          foreach Query.(select (from students)
                           ~where:(students.(gpa) > float 3.8))
            ~f:(fun s -> return (Student.name s))

      ]}

      we can evaluate this property, with

      {[eval names_of_best_students]}

      to get a sequence (possibly empty) of all students that have the
      GPA score greater than 3.8.

  *)
  val eval : 'a t -> doc -> 'a  Or_error.t m

  (** [exec op doc] executes an operation [op] that, presumabely,
      updates the document [doc], returns an updated version.*)
  val exec : 'a t -> doc -> doc Or_error.t m

  (** [run op doc] runs an operation [op] that does some inference as
      well as may update the document. This function is a usual part
      of a generic state monad interface, and is provided for the
      consistency. Usually, it is a bad idea, or a notion of a bad
      style to use this function. *)
  val run : 'a t -> doc -> ('a * doc) Or_error.t m
end


(** [Make(M)] returns an Ogre monad implementation wrapped in a monad [M]. *)
module Make(M : Monad.S) : S with type 'a m := 'a M.t


(** Default implementation of the Orge monad, that is not wrapped into
    any other monads (in other words, that is wrapped into the identity)*)
include S with type 'a m = 'a
           and type 'a t = 'a Make(Monad.Ident).t
           and type 'a e = doc -> ('a * doc) Or_error.t
