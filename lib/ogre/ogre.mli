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

    {v (declare student (name str) (gpa float)) v}

    where the declaration has the following syntax:

    {v
     declaration ::= ( declare <attribute-name> <field> <field> ... )
     field ::= ( <field-name> <field-type> )
     field-type ::= int | str | bool | float
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

open Core_kernel
open Monads.Std
open Format


(** the document  *)
type doc


(** type information associated with an attribute  *)
type ('a,'k) typeinfo constraint 'k = _ -> _

(** a descriptor of an attribute.

    Used to construct attribute values, and to query
    documents. Created with [declare] function.

    Note, that due to a value restriction, an attribute should be
    defined as a function returning a type information.*)
type ('a,'k) attribute = unit -> ('a,'k) typeinfo


(** [t field] a descriptor of an attribute field.

    Used to construct attributes, and to construct variables that
    reference particular fields of an attribute.

    the type variable [t] range is [float], [int64], [string] or
    [bool].*)
type 'a field

(** [attrs query] constructs a query type.

    Created using the [Query] module. The [attrs] type variable
    encodes the types of requested attributes. For example,

    [((student -> teacher -> 'a) -> 'a) query]

    represents a query for two attributes of type [student] and
    [teacher] correspondingly. It is represented as a continuation,
    denoting the fact, that the query can be executed later for an
    arbitrary result.*)
type 'a query

(** a result of a selection.  *)
type 'a seq = 'a Sequence.t

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


(** [let attr () = declare ~name scheme] declares an attribute named
    with [name], and having a type described by the [scheme].

    Due to a value restriction, each attribute should be defined as
    a thunk (a function).

*)
val declare : name:string -> ('f -> 'a, 'k) scheme -> 'f -> ('a, 'k) typeinfo



(** Ogre type system.

    Ogre type system is extremely simple, it has only four types:
    1. boolean - a logical type that has only two values (true,false);
    2. int - an integral number that maps to OCaml's [int64];
    3. float - a real number that maps to OCaml's [float];
    4. str - an arbitrary string, represented as [string] in OCaml.


    A scheme is a first class value, that  describes how to construct
    attributes from fields, and how to deconstruct them.

    A field is a pair of a name and a type, and is used to reference
    to construct attributes, and to reference to attribute objects in
    queries.
*)
module Type : sig


  (** type descriptor.  *)
  type 'a t


  (** [int] is represented with OCaml's int64, and has a
      corresponding representation *)
  val int : int64 t


  (** [bool] is either [true] or [false].  *)
  val bool : bool t

  (** [str] represented by a sequence of characters. If the sequence
      contains whitespaces or parenthesis then the sequence should be
      delimited with quotes. Note, the requirements are for the
      database backend implementations. A user of the library
      shouldn't be bothered by the representation.*)
  val str : string t
  val float : float t


  (** [scheme field] defines a scheme with one field.   *)
  val scheme : 'a field -> (('a -> 'r) -> 'r, ('a -> 'p) -> 'p) scheme


  (** [scm $field] adds a [field] to a scheme [scm].

      Usually, all the fields are combined in a one expression,
      starting with the call to a function [scheme], e.g.,

      [scheme name $ age $ salary]

      The [scheme] had type [(('a -> 'r) -> 'r, ('a -> 'p) -> 'p) scheme], then
      the type of a resulting scheme would be [[(('a -> 'b -> 'r) -> 'r, ('a ->
      'b -> 'p) -> 'p) scheme]], i.e., a type of [$field] will be attached
      to the scheme.

      For example, the [scheme name $age $salary] expression will have
      a type (assuming, that name, age and salary are represented with
      string, int, and int, correspondingly):

      {[(string -> int64 -> int64 -> 'a) -> 'a,
        (string -> int64 -> int64 -> 'b) -> 'b]}

      The scheme is used to construct an attribute. See below.*)
  val ($) :
    ('a -> 'b -> 'r, 'd -> 'b -> 'p) scheme -> 'b field ->
    ('a -> 'r, 'd -> 'p) scheme



  (** [def name t] defines a field with the give [name] and type [t].  *)
  val def  : string -> 'a t -> 'a field


  (** [name : t] is the same as [def name t]  *)
  val (%:) : string -> 'a t -> 'a field
end

(** Domain specific language for constructing queries.

    Currently only a select query is supported.

    Currently, the expression language permits construction arithmetic
    and logical expressions on the base types (int, float, str and
    bool).


*)
module Query : sig

  type 'a t = 'a query


  (** logical expression language, defined as
      {v
      exp ::= str `string`
            | int `int64`
            | float `float`
            | bool `bool`
            | `'a attribute`.(`'b field`)
            | `'b field`.[`int`]
            | exp <bop> exp
            | <uop> exp

      bop ::=  <aop> | <lop> | <cop>
      uop ::=  not
      aop ::= + | -
      lop ::= || | && | ==>
      cop ::= < | > | = | <> | <= | >=
      v}

      In the grammar above, the names delimited with backticks represent
      types of OCaml values, that should be passed at these syntactic
      locations (sort of an unquoting), for example, [str "hello"] is
      an expression, as well as [student.(gpa)] assuming that
      [student] is a value of type ['a attribute] and [gpa] is a field
      of type ['b field].

      Not all expressions are well-formed, as they also must obey to
      the typing rules. The typing rules are simple (informally):

      0. [x <bop> y] is wff if [x] and [y] are of the same type;
      1. [x.(y)] is wff if [y] is a field of attribute [x];
      2. [x <aop> y] is wff if [x] and [y] are float or int;
      3. [not x] is wff if [x] is bool
      4. [x <lop> y] is wff if [x] and [y] are bool
      5. [x <cop> y] has type bool.
  *)
  type exp


  (** join statement.

      The [join] statement is a list of equality classes. Each
      equality class defines a query constraint, requiring all
      elements of the class to be equal. The elements of the class are
      field variables, constructed with the [field] function. There
      are two kinds of the field variables:

      - A fully qualified field variable, defined with the expression
        [field y ~from:x].

      - An unqualified field variable, defined as [field y].


      A fully qualified variable matches only with the corresponding
      field expression, e.g., an equality class

      [[field teacher ~from:student; field id ~from:teacher]]

      emposes a constraint [student.(teacher) = teacher.(id)], and is
      roughly equivalent to the SQL's

      [INNER JOIN teacher ON student.id = teacher.id]

      Note:  it is OK to use [where] clause instead of the [join]
      clause to join attributes, if it makes the query more
      readable. There is no performance penalty.

      The unqualified variable matches with the same fields ignoring
      the attribute name, for example, an equality class [field
      classid], will impose an equality constraint on values from all
      [classid] fields of the selected attributes. Given a concrete
      select query:

      {[select (from student $teacher) ~join:[[field classid]]]}

      a constraint [student.(classid) = teacher.(classid)] is
      constructed. Another way to construct the same selection is:

      {[select (from student $teacher)
          ~where:student.(classid) = teacher.(classid)
      ]} *)
  type join


  (** a selection of attributes.

      The tables clause can be constructed using the following grammar:

      {v tables ::= from attr | <tables> $ attr v}

      In other words, there are two constructors, a prefix [from
      attr], and an infix [attr1 $ attr2], e.g.,

      [from students $ teachers $ classes] *)
  type 'a tables

  (** [select ~where ~join (from t1 t2 ... tm)] selects attributes
      [t1], [t2], ..., [tm], join them by the fields specified in the
      [join] clause, and filters those that satisfy the condition
      defined with the [where] clause.

      Examples:

      Select all students that has the GPA rate greater than 3.8.
      {[
        select
          ~where:(student.(gpa) > float 3.8)
          (from students)
      ]}


      Select all students and their corresponding teachers, that have
      a GPA greater than 3.8 (assuming that teacher is a foreign key
      to the table of teachers).
      {[
        select
          ~where:(student.(gpa) > float 3.8)
          ~join:[[field teacher ~from:student; field id ~from:teacher]]
          (from students)
      ]}


      You may notice, that the [select] query lacks the SQL's [WHAT]
      clause, i.e., it is not possible or needed to specify columns. The
      reason for this, is that the query used as a value that is
      passed to some command constructor, (e.g.,[foreach]), that can
      work with fields individually, e.g., the following is a complete
      correspondence of the SQL's:

      {v SELECT name FROM students WHERE gpa > 3.5 v}

      {[
        foreach Query.(select
                         ~where:(student.(gpa) > float 3.8)
                         (from students))
          ~f:(fun s -> return (Student.name s))
      ]}

      It is nearly three times as long, but in return it is type-safe,
      and composable.*)
  val select :
    ?where:exp ->
    ?join:join list list ->
    'a tables -> 'a t


  (** [from attr] adds an attribute [attr] to the query. An attribute
      can be referenced in the query if it occurs in the from
      clause. Otherwise the query is not well-formed.  *)
  val from : ('a,_) attribute -> (('a -> 'r) -> 'r) tables


  (** [attrs $ attr] appends an attribute [attr] to the sequence of
      chosen attributes [attrs].*)
  val ($) : ('a -> 'b -> 'r) tables -> ('b,_) attribute -> ('a -> 'r) tables

  (** [field name] creates an unqualified join variable.
      [field name ~from:attr] creates a qualified join variable.

      See the {!join} type description, for the explanation of the
      [join] expressions and joining.  *)
  val field : ?from: (_,_) attribute -> _ field -> join


  (** Defines a subscripting syntax for creating field variables.

      An OCaml expression [x.(y)] is a syntactic sugar to
      [Array.get x y], thus an expression [x.(y)] in the scope of the
      [Query] module takes an attribute as [x] and a field as [y] and
      returns an [exp] that denotes a field variable of attribute [x]
      that ranges over the values of field [y].*)
  module Array : sig


    (** [attr.(field)] creates a field variable that ranges over
        values of [field] that belongs to the attribute [attr].

        See the module description if you don't understand how it works.*)
    val get : (_,_) attribute -> _ field -> exp
  end


  (** Defines field subscripting syntax.

      It is also possible to reference a field of a  column by
      the column's number in the selection. For example, in a query

      {[select (from students $ students)
          ~join:[[field name]]
          ~where:(id.[0] <> id.[1])]}

      a variable [id.[0]] will be bound to the field [id] of the
      first column of a selection, and [id.[1]] will be bound to the
      second one.*)
  module String : sig
    (** [filed.(n)] creates a field variable that ranges over values
        of [field] that belongs to the n'th attribute of a selection.*)
    val get : _ field -> int -> exp
  end


  (** [str x] creates a string constant.  *)
  val str : string -> exp


  (** [int x] creates an integer constant  *)
  val int : int64 -> exp

  (** [bool x] creates a logic constant  *)
  val bool : bool -> exp


  (** [float x] creates a real number constant.  *)
  val float : float -> exp


  (** [x && y] conjunction  *)
  val (&&) : exp -> exp -> exp


  (** [x || y] disjunction  *)
  val (||) : exp -> exp -> exp


  (** [x ==> y] implication.

      Be aware that the precedence of OCaml operator ==> is higher than
      a common precedence of the implication operator in mathematics.

      That means, that an expression [x && y ==> x && z] is parsed as
      [x && (y ==> x) && z].

      The rule of the thumb is to always put parenthesis in an
      expression, that has an implication, as even if you're aware of
      the precedence issue, it is not known to a reader of your code,
      whether you were aware, or wrote this code by a mistake.*)
  val (==>) : exp -> exp -> exp


  (** [not x] logical negation.  *)
  val not : exp -> exp


  (** [x < y] less than  *)
  val (<) : exp -> exp -> exp


  (** [x > y] greater than  *)
  val (>) : exp -> exp -> exp


  (** [x = y] equality  *)
  val (=) : exp -> exp -> exp


  (** [x <> y] nonequality  *)
  val (<>) : exp -> exp -> exp


  (** [x <= y] less than or equal  *)
  val (<=) : exp -> exp -> exp


  (** [x >= y] greater or equal *)
  val (>=) : exp -> exp -> exp


  (** [x + y] summation  *)
  val (+) : exp -> exp -> exp


  (** [x - y] subtracting  *)
  val (-) : exp -> exp -> exp
end



(** An Ogre document.

    A concrete representation of a database.

*)
module Doc : sig
  type t = doc [@@deriving bin_io, compare, sexp]


  (** [empty] creates an empty document  *)
  val empty : doc


  (** [is_empty x] is true iff [x] is [empty].
      @since 2.2.0 *)
  val is_empty : doc -> bool


  (** [merge d1 d2] merges two documents in one. Returns an error,
      if documents contain inconsistent declarations.*)
  val merge : doc -> doc -> doc Or_error.t


  (** [load chan] loads a document from a channel, returns an error if
      a document is not well-formed, raises an exception if a system error
      has occurred.  *)
  val load : In_channel.t -> doc Or_error.t


  (** [save doc out] stores the document in a channel. Raises an
      exception in case of a system error. *)
  val save : doc -> Out_channel.t -> unit


  (** [from_file name] reads a document from a file with the given
      [name], returns an error, if a document is not well-formed, raises
      an exception if a system error has occurred. *)
  val from_file : string -> doc Or_error.t


  (** [from_string data] parses document from [data]. Returns an error
      if a document is not well-formed. *)
  val from_string : string -> doc Or_error.t


  (** [to_string doc] returns a textual representation of a document  *)
  val to_string : doc -> string


  (** [to_file doc name] stores a document to a file with the given name.  *)
  val to_file : doc -> string -> unit

  (** [pp ppf doc] prints a [doc] in the specified formatter [ppf]  *)
  val pp : Format.formatter -> doc -> unit

  (** [pp_yamp ppf doc] prints doc to [ppf] in the YAML format.

      @since 2.3.0  *)
  val pp_yaml : Format.formatter -> doc -> unit


  (** [clear doc] removes all facts from the document.

      Useful for extracting scheme from a document.*)
  val clear : doc -> doc


  (** [declarations doc] returns the number of declarations in the
      document *)
  val declarations : doc -> int


  (** [definitions doc] returns the number of facts that defined in the
      document. *)
  val definitions : doc -> int
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
      language is very similar to SQL, and has join and where
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
  val foreach : ('a -> 'b) query -> f:'a -> 'b seq t

  (** [collect query] is the same as [foreach query ~f:ident] *)
  val collect : (('a -> 'a) -> 'b) query -> 'b seq t

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
      satisfied and no errors occurred the inferred result.

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

  (** [exec op doc] executes an operation [op] that, presumably,
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
