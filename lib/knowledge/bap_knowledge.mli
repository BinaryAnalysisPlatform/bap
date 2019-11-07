open Core_kernel
open Monads.Std


(** The Knowledge Representation Library.

    A library for building knowledge representation and
    reasoning systems. *)


(** a knowledge dependent computation.  *)
type 'a knowledge


(** The Knowledge Representation Library.

    {2 Introduction}

    The library provides facilities for storing, accumulating, and
    computing knowledge. The knowledge could be represented indirectly,
    in the Knowledge Base, or directly as knowledge values. The
    library focuses on representing knowledge that is partial and
    provides mechanisms for knowledge accumulation and
    refinement. The knowledge representation library leverages the
    powerful type system of the OCaml language to facilitate
    development of complex knowledge representation and reasoning systems.

    {2 Knowledge Taxonomy}

    For a given knowledge system, the domain of discourse is a set of
    objects, optionally partitioned into sorts. Therefore, an {i object}
    is fundamental building block of a knowledge system.

    An object {i class} defines a set of possible properties
    of that objects. A snapshot of all properties of an object is
    called {i value}. A set of values belonging to a particular class
    could be partitioned into sorts, to facilitate the design of
    strongly typed interfaces.

    Properties of objects and values are stored in {i slots}.
    The data type of any property is required to be an instance of the
    {i domain} structure, i.e., it should be a set with a special [empty]
    value and the [order] operation, that orders elements of this set
    by their informational content, so that [empty] is the least
    element.

    The knowledge could be represented directly as a value, or
    indirectly as a set of objects in a knowledge base.


    {2 Values}

    A value is an ordered tuple of slots that holds all properties of
    an object with which this value is associated. Additionally, a
    value is attributed with a sort value, which is shared by all
    values belonging to that sort. The sort value could be a concrete
    value, holding some information that is common for all elements of
    the sort or it could be just a type index that witnesses that the
    value belongs to a certain set of values.

    Properties of a value could be accessed using the [Value.get]
    operator. A new value of a property could be put into the slot
    using the [Value.put] operator.

    Values are instances of the domain type class and therefore a
    property of an object or another value could also be a value.

    The set of slots of a given value is defined by its class, and
    this set is extensible, i.e., it is possible to add more slots.

    {2 Knowledge Base}

    The knowledge base maintains a consistent set of facts about
    object properties. An object is a unit of identity. The value of
    an object is defined by its properties. However, the knowledge
    base doesn't provide the direct access to the object value.

    Object properties could be accessed using the [collect] operator
    and set using the [provide] operator. The knowledge base maintains
    the consistency by disallowing changing an object property to a
    value that has less informational contents than the previous
    value, so that no information is never lost.

    Object properties could also be computed on demand using the
    [promise] operator, which effectively stores a procedure in the
    knowledge base. Several procedures could be provided for a
    property computation, and the procedures themselves could access
    other properties, including the property being computed. The
    knowledge base will ensure that the least fixed point of all
    procedures involved in the property computation is reached.

    All Knowledge Base operators return a computation of type
    ['a knowledge] which is a monad, that denotes a computation that
    is knowledge dependent, i.e., it either accesses the knowledge
    base, or modifies it, or both.

    The knowledge computation may lead to an inconsistent state, in
    other words, it is not guaranteed that the computation will reach
    the normal form. A diverging computation will yield a value of
    type [conflict] when run.

    To prevent unnecessary conflicts, it is possible to represent
    object properties as opinions instead of facts. Opinions are facts
    that are attributed with the name of an agent that provided this
    fact. In case if mutiple agents provide conflicting opinions, the
    [resolve] operator will compute the consensus, based on agents
    predefined trustworthiness. Opinions are introduced using the
    [suggest] operator or promised using the [propose] operator.

    Finally, the knowledge base is partially persistent. It is
    possible to make some slots persistent, so that properties, stored
    in them are preserved between program runs.

*)
module Knowledge : sig

  (** a knowledge monad  *)
  type 'a t = 'a knowledge

  (** a sort ['s] of class ['k].   *)
  type (+'k,+'s) cls

  (** an object of class ['k]  *)
  type +'k obj

  (** a value of class ['c = ('k,'s) cls]  *)
  type +'c value

  (** a slot holding a property ['p] of a class ['k] object. *)
  type (+'k,'p) slot

  (** an instance of the domain type class  *)
  type 'p domain

  (** an instance of the persistance type class  *)
  type 'a persistent

  (** the knowledge base state  *)
  type state

  (** a set of possible conflicts  *)
  type conflict = ..

  (** an information provider  *)
  type agent

  (** an opinion based fact of type ['a]  *)
  type 'a opinions

  (** a fully qualified name  *)
  type name

  (** [collect p x] collects the value of the property [p].

      If the object [x] doesn't have a value for the property [p] and
      there are promises registered in the knowledge system, to compute
      the property [p] then they will be invoked, otherwise the empty
      value of the property domain is returned as the result.  *)
  val collect : ('a,'p) slot -> 'a obj -> 'p t


  (** [resolve p x] resolves the multi-opinion property [p]

      Finds a common resolution for the property [p] using
      the current resolution strategy.

      This function is the same as [collect] except it collects
      a value from the opinions domain and computes the current
      consensus.
  *)
  val resolve : ('a,'p opinions) slot -> 'a obj -> 'p t

  (** [provide p x v] provides the value [v] for the property [p].

      If the object [x] already had a value [v'] then the provided
      value [v] then the result value of [p] is [join v v'] provided
      such exists, where [join] is [Domain.join (Slot.domain p)].

      If [join v v'] doesn't exist (i.e., it is [Error conflict])
      then [provide p x v] diverges into a conflict.
  *)
  val provide : ('a,'p) slot -> 'a obj -> 'p -> unit t


  (** [suggest a p x v] suggests [v] as the value for the property [p].

      The same as [provide] except the provided value is predicated by
      the agent identity.
  *)
  val suggest : agent -> ('a,'p opinions) slot -> 'a obj -> 'p -> unit t

  (** [promise p f] promises to compute the property [p].

      If no knowledge exists about the property [p] of
      an object [x], then [f x] is invoked to provide an
      initial value.

      If there are more than one promises, then they all must
      provide a consistent answer. The function [f] may refer
      to the property [p] directly or indirectly. In that case
      the least fixed point solution of all functions [g] involved
      in the property computation is computed.
  *)
  val promise : ('a,'p) slot -> ('a obj -> 'p t) -> unit

  (** [propose p f] proposes the opinion computation.

      The same as [promise] except that it promises a value for
      an opinion based property.
  *)
  val propose : agent -> ('a, 'p opinions) slot -> ('a obj -> 'p t) -> unit


  (** state with no knowledge  *)
  val empty : state



  (** [of_bigstring data] loads state from [data] *)
  val of_bigstring : Bigstring.t -> state


  (** [to_bigstring state] serializes state into a binary representation.  *)
  val to_bigstring : state -> Bigstring.t


  (** prints the state of the knowledge base.  *)
  val pp_state : Format.formatter -> state -> unit


  (** [run cls comp init] computes the value of the object [obj] given

      Evaluates the knowledge dependent computation [comp] using the
      initial set of facts [init].

      The computation must evaluate to an object [p] of the class
      [cls]. The [run] function computes all properties of [p], which
      will trigger all promises associated with the slots.

      The result of evaluation is either a conflict, or a pair of
      value, which contains all properties of the object, and the
      knowledge accumulated during the computation.

  *)
  val run : ('k,'s) cls -> 'k obj t -> state -> (('k,'s) cls value * state, conflict) result

  module Syntax : sig
    include Monad.Syntax.S with type 'a t := 'a t


    (** [x-->p] is [collect p x] *)
    val (-->) : 'a obj -> ('a,'p) slot -> 'p t


    (** [p <-- f] is [promise p f]  *)
    val (<--) : ('a,'p) slot -> ('a obj -> 'p t) -> unit


    (** [c // s] is [Object.read c s]  *)
    val (//) : ('a,_) cls -> string -> 'a obj t
  end


  include Monad.S with type 'a t := 'a t
                   and module Syntax := Syntax

  include Monad.Fail.S with type 'a t := 'a t
                        and type 'a error = conflict

  (** Orders knowledge by its information content.

      [Order.partial] is a generalization of the total order,
      which is used to compare the amount of information in two
      specifications of knowledge.

      Note: The information content of a value ordering shall not be
      confused with any other intrinsic ordering of the associated value
      type. For example, if we will take age, measured in natural numbers,
      then the order natural numbers has nothing to do with the amount
      of information associated with each age. From the knowledge
      representation perspective. We either do not know the age, or
      know it to some certainty. Therefore, the domain representation
      could be either [int option], with [None] being the minimal
      element (denoting an absence of knowledge about the age of an
      object) or [Some x], s.t.,
      [order (Some x) (Some y) = NC iff x <> y], i.e., neither is
      having more or less knowledge than another.

  *)
  module Order : sig

    (** partial ordering for two way comparison.

        The semantics of constructors:
        - [LT] - strictly less information
        - [GT] - strictly more information
        - [EQ] - equal informational content
        - [NC] - non-comparable entities
    *)
    type partial = LT | EQ | GT | NC


    module type S = sig

      (** a partially ordered type  *)
      type t

      (** defines a partial order relationship between two entities.

          Given a partial ordering relation [<=]
          - [order x y = LT iff x <= y && not (y <= x)]
          - [order x y = GT iff y <= x && not (x <= y)]
          - [order x y = EQ iff x <= y && y <= x]
          - [order x y = NC iff not (x <= y) && not (y <= x)]
      *)
      val order : t -> t -> partial
    end
  end


  (** Class is a collection of sorts.

      A class [k] is denoted by an indexed type [(k,s) cls], where
      [s] is a sort.

      A class denotes a set of properties that describe objects and
      values of the given class. Thus, a class fully defines the
      structure of an object or a value. Sorts could be used to
      further partition the class into subsets, if needed.

      Classes are declarative and each class should have a unique
      name. To prevent name clashing the Knowledge Library employs a
      Common Lisp style namespaces, where each name belongs to a
      package.

      The type index, associated with the class, should be protected
      by the module that declares the class. For example, it is a bad
      idea to use the [unit] type as an index of a class.

  *)
  module Class : sig
    type (+'k,'s) t = ('k,'s) cls


    (** [declare name sort] declares a new
        class with the given [name] and [sort] index.

        Since classes are declarative each new declaration creates a
        new class that is not equal to any other.

        The [package] and [name] pair should be unique. If there is
        already a class with the given [package:name] then the
        declaration fails.
    *)
    val declare : ?desc:string -> ?package:string -> string -> 's -> ('k,'s) cls


    (** [refine cls s] refines the [sort] of class ['k] to [s].   *)
    val refine : ('k,_) cls -> 's -> ('k,'s) cls

    (** [same x y] is true if [x] and [y] denote the same class [k] *)
    val same : ('a,_) cls -> ('b,_) cls -> bool

    (** [equal x y] constructs a type witness of classes equality.

        The witness could be used to cast objects of the same class,
        e.g.,

        {[
          match equal bitv abs with
          | Some t -> Object.cast t x y
          | _ -> ...
        ]}

        Note that the equality is reflexive, so the obtained witness
        could be used in both direction, for upcasting and downcasting.
    *)
    val equal : ('a,_) cls -> ('b,_) cls -> ('a obj, 'b obj) Type_equal.t option


    (** [assert_equal x y] asserts the equality of two classes.

        Usefull, in the context where the class is known for sure,
        (e.g., constrained by the module signature), but has to be
        recreated. The [let T = assert_equal x y] expression,
        establishes a type equality between objects in the typing
        context, so there is no need to invoke [Object.cast].

        {[
          let add : value obj -> value obj -> value obj = fun x y ->
            let T = assert_equal bitv value in
            x + y (* where (+) has type [bitv obj -> bitv obj -> bit obj] *)
        ]}
    *)
    val assert_equal : ('a,_) cls -> ('b,_) cls -> ('a obj, 'b obj) Type_equal.t


    (** [property cls name dom] declares
        a new property of all instances of class [k].

        Each property should have a unique name. If a property with
        the given name was already, then the declaration fails.

        The returned slot can be used to access the declarated
        property. If the property is expected to be public then it
        should be published via a library interface.

        @param persistent is an instance of the [persistent] class that
        will be used to persist the property.

        @param desc is a property documentation.
    *)
    val property :
      ?desc:string ->
      ?persistent:'p persistent ->
      ?package:string ->
      ('k,_) cls -> string -> 'p domain -> ('k,'p) slot

    (** [name cls] is the class name.  *)
    val name : ('a,_) cls -> name

    (** [sort cls] returns the sort index of the class [k].  *)
    val sort : ('k,'s) cls -> 's
  end


  (** Knowledge Base Objects.

      The knowledge base stores object properties. The
      object itself is an identifier, and could be seen as a pointer
      or a database key.
  *)
  module Object : sig
    type 'a t = 'a obj
    type 'a ord

    (** [create] is a fresh new object with an idefinite extent.  *)
    val create : ('a,_) cls -> 'a obj knowledge

    (** [scoped scope] pass a fresh new object to [scope].

        The extent of the created object is limited with the extent
        of the function [scope]. The object passed to the [scope]
        function is automatically deleted after the computation,
        returned by [scope] evaluates to a value. The identity of an
        object could be reused, which may lead to hard detected bugs,
        thus care should be taken to prevent the object value from
        escaping the scope of the function.
    *)
    val scoped : ('a,_) cls -> ('a obj -> 'b knowledge) -> 'b knowledge

    (** [repr x] returns a textual representation of the object [x] *)
    val repr : ('a,_) cls -> 'a t -> string knowledge

    (** [read s] returns an object [x] such that [repr x = s].  *)
    val read : ('a,_) cls -> string -> 'a t knowledge


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


    (** [id obj] returns the internal representation of an object.   *)
    val id : 'a obj -> Int63.t


    (** Ordered and persistent data types.  *)
    module type S = sig
      type t [@@deriving sexp]
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end


    (** [derive cls] a module of type [S] for the given class [cls].*)
    val derive : ('a,'d) cls -> (module S
                                  with type t = 'a obj
                                   and type comparator_witness = 'a ord)
  end


  (** Knowledge Values.

      A value is a concrete representation of knowledge. It is a
      snapshot of all properties associated with some object, i.e., an
      ordered tuple of slots. The value has no identity and that
      differs it from the object, as the value is essentially a
      property of an object - not an object itself.

      From another perspective, a value is an extensible record, with
      fields reified into first-class slots, the record type into the
      [cls] values, and an additional constraint that all field types
      must be an instance of the domain type class (i.e., have a
      default and an ordering).

      Each value has a class. The class value itself is indexed with
      the class and sort indices. The first index denotes the set of
      properties that could be associated with the value. The second
      index further partitions values of that class into subsets, to
      so that domain specific relations between values could be
      expressed explicitly with the host language type system.

      The value sort is an arbitrary value, which can also be used to
      store additional static information about the value, i.e., the
      information that is common to all instances of the sort.


      {3 Total ordering and age}

      In addition to be partially ordered by their information
      content, values are also totally ordered, so that they could be
      organized into finite sets and maps.

      The total order is induced from the information order, so that
      if a value [x] is ordered before [y] in the information order,
      then it will be also ordered before [y] in the total order. And
      if [x] and [y] have the same information content, then they are
      considered equal. Values with non-comparable information content
      are ordered by their time-stamp.

      Every time a new value created it is assigned a
      time-stamp. A new value is created by all functions that has
      ['a value] return type, except the [refine] function. Each
      time-stamp is unique and no two values could have the same
      time-stamps unless they are physically the same, or are
      de-serializations of the same value, or are refinements of the
      same value. In other words, values with equal time-stamps are
      guaranteed to bear the same information.

      Time-stamp values correlate with the order of evaluation. A
      value that was evaluated more recently will have a higher
      time-stamp. Therefore time-stamps define an age of a value. A
      value which has a smaller time-stamp is younger than a value
      that has a larger time-stamp.

  *)
  module Value : sig
    type 'a t = 'a value


    (** a witness of the ordering  *)
    type 'a ord
    include Type_equal.Injective with type 'a t := 'a t


    (** [empty cls] the empty value of class [cls].

        The empty value has the least information content, i.e., all
        slots are empty.
    *)
    val empty : ('a,'b) cls -> ('a,'b) cls value


    (** [order x y] orders [x] and [y] by their information content.  *)
    val order : 'a value -> 'a value -> Order.partial


    (** [join x y] joins pairwise all slots of [x] and [y].

        Each slot of [x] and [y] are joined using the [Domain.join]
        function. The result is either a pairwise join or a conflict
        if any of the joins ended up with a conflict.
    *)
    val join : 'a value -> 'a value -> ('a value,conflict) result


    (** [merge x y] joins [x] and [y] and resolves conflicts.

        Performs a pairwise join of [x] and [y] and in case if
        any of the joins yields a conflict uses the provided strategy
        to resolve it.

        @param on_conflict specifies the conflict resolution strategy
        (see below). Defaults to [`drop_old]

        {3 Conflict resolution strategies}

        The following conflict resolution strategies are currently
        supported (more strategies could be added later):

        - [`drop_old] a conflicting property of an older object is
          ignored (an object is older if its time-stamp is less than
          or equal the time-stamp of other object);
        - [`drop_new] a conflicting property of the newer object is
          ignored (an object is newer if its time-stamp is greater than
          or equal the time-stamp of other object);
        - [`drop_right] a conflicting property of [y] is ignored;
        - [`drop_left] a conflicting property of [x] is ignored.
    *)
    val merge : ?on_conflict:[
      | `drop_old
      | `drop_new
      | `drop_right
      | `drop_left
    ] -> 'a value -> 'a value -> 'a value


    (** [cls x] is the class of [x].   *)
    val cls : ('k,'s) cls value -> ('k,'s) cls


    (** [get p v] gets a value of the property [p].  *)
    val get : ('k,'p) slot -> ('k,_) cls value -> 'p


    (** [put p v x] sets a value of the property [p].  *)
    val put : ('k,'p) slot -> ('k,'s) cls value -> 'p -> ('k,'s) cls value

    (** [refine v s] refines the sort of [v] to [s].

        Since, this function doesn't change the information stored in
        the value, the time-stamp of the returned value is the same,
        therefore [v = refine v s].
    *)
    val refine : ('k,_) cls value -> 's -> ('k,'s) cls value

    module type S = sig
      type t [@@deriving sexp]
      val empty : t
      val domain : t domain
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end


    (** [derive cls] derives the implementation of the [S] structure.   *)
    val derive : ('a,'s) cls ->
      (module S
        with type t = ('a,'s) cls t
         and type comparator_witness = ('a,'s) cls ord)


    (** [pp ppf v] outputs [v] to the formatter [ppf].

        Prints all slots of the value [v].
    *)
    val pp : Format.formatter -> 'a value -> unit


    (** [pp_slots slots ppf v] prints the specified set of slots.

        Prints only slots that has a name in [slots].*)
    val pp_slots : string list -> Format.formatter -> 'a value -> unit
  end


  (** Property accessor.

      Properties are accessed by slots. A slot is a first-class
      record field. Slots are declarative, each new declaration of
      a slot creates a new instance. Slot names should be unique - it
      is not allowed to have two slots with the same name, even if
      they belong to different classes.

      To declare a new slot of a class use the [Class.property]
      function. This module enables [slot] introspection.

  *)
  module Slot : sig
    type ('a,'p) t = ('a,'p) slot

    (** [domain slot] the [slot] domain.  *)
    val domain : ('a,'p) slot -> 'p domain

    (** [cls slot] slot's class.  *)
    val cls : ('a,_) slot -> ('a, unit) cls

    (** [name slot] the slot name.  *)
    val name : ('a,'p) slot -> name

    (** [desc slot] the slot documentation.  *)
    val desc : ('a,'p) slot -> string
  end


  (** A symbol is an object with a unique name.

      Sometimes it is necessary to refer to an object by name, so that
      a chosen name will always identify the same object. Finding or
      creating an object by name is called "interning" it. A symbol
      that has a name is called an "interned symbol". However we
      stretch the boundaries of the symbol idea, by treating all other
      objects as "uninterned symbols". So that any object could be
      treated as a symbol.

      To prevent name clashing, that introduces unwanted equalities,
      we employ the system of packages, where each symbol belongs
      to a package, called its home package. The large system design
      is leveraged due to the mechanism of symbol importing, where the
      same symbol could be referenced from different packages (see
      [import] and [in_package] functions, for more information).

      {3 Symbol syntax}

      The [read] function enables translation of the symbol textual
      representation to an object.  The symbol syntax is designed to
      be verstatile so it can allow arbitrary sets of characters, to
      enable support for modeling different knowledge domains. Only
      two characters has the special meaning for the symbol reader,
      the [:] character acts as a separator between the package and
      the name constituent, and the [\\] symbol escapes any special
      treatment of a symbol that follows it (including the [\\]
      itself). When a symbol is read, an absence of the package is
      treated the same as if the [package] parameter of the [create]
      function wasn't set, e.g.,
      [read c "x"] is the same as [create c "x"], while an empty package
      denotes the [keyword] package, e.g.,
      [read c ":x"] is the same as [create ~package:keyword c "x"].


      {3 Name equality}

      The equality of two names is defined by equality of their
      byte representation. Hence, symbols which differ in register
      will be treated differently, e.g., [Foo <> foo].
  *)
  module Symbol : sig

    (** [intern ?public ?desc ?package name cls] interns a symbol in
        a package.

        If a symbol with the given name is already interned in a
        package, then returns its value, otherwise creates a new
        object.

        If symbol is [public] then it might be advertised and be
        accessible during the introspection. It is recommeneded to
        provide a description string if a symbol is public. Note, a
        non-public symbol still could be obtained by anyone who knows
        the name.

        If the function is called in the scope of one or more
        [in_package pkg<N>], then the [package] parameter defaults to
        [pkg], otherwise it defaults to ["user"]. See also the
        [keyword] package for the special package that holds constants
        and keywords.

        The [desc], [package], and [name] parameters could be
        arbitrary strings (including empty). Any occurence of the
        package separator symbol ([:]) will be escaped and won't be
        treated as a package/name separator.
    *)
    val intern : ?public:bool -> ?desc:string -> ?package:string -> string ->
      ('a,_) cls -> 'a obj knowledge

    (** [keyword = "keyword"] is the special name for the package
        that contains keywords. Basically, keywords are special kinds
        of symbols whose meaning is defined by their names and nothing
        else. *)
    val keyword : string

    (** [in_package pkg f] makes [pkg] the default package in [f].

        Every reference to an unqualified symbol in the scope of the
        [f] function will be treated as it is qualified with the
        package [pkg]. This function will affect both the reader and
        the pretty printer, thus [in_package "pkg" @@ Obj.repr buf] will
        yield something like [#<buf 123>], instead of [#<pkg:buf 123>].
    *)
    val in_package : string -> (unit -> 'a knowledge) -> 'a knowledge


    (** [import ?strict ?package:p names] imports all [names] into [p].

        The [names] elements could be either package names or
        qualified names. If an element is a package, then all public
        names from this package are imported into the package [p]. If
        an element is a qualified symbol then it is imported into [p],
        even if it is not public in the package from which it is being
        imported.

        If any of the elements of the [names] list doesn't represent a
        known package or known symbol, then a conflict is raised,
        either [Not_a_package] or [Not_a_symbol].

        If [strict] is [true] then no name can change its value during
        the import. Otherwise, if the name is alredy in present in
        package [p] with a different value, then it will be
        overwritten with the new value, i.e., shadowed.

        All names are processed in order, so names imported from
        packages that are in the beginning of the list could be
        shadowed by the names that are in the end of the list (unless
        [strict] is [true], of course). Thus,
        {[
          import [x] >>= fun () ->
          import [y]
        ]}

        is the same as [import [x;y]].

        Note, all imported names are added as not public.

        If the [package] parameter is not specified, then names are
        imported into the current package, as set by the [in_package]
        function.
    *)
    val import : ?strict:bool -> ?package:string -> string list -> unit knowledge
  end

  (** An information provider.

      A piece of information could be predicated with its source to
      denote the trustworthiness of the information an/or to
      enable information provenance.

      An agent is a registry entity with associated name, description,
      and reliability. The [name] and [description] are used for
      introspection. The reliability could be used for conflict
      resolution.

      Using [Domain.opinions] it is easy to embed any data into the
      domain structure, by pairing the elements of this set with the
      information about its source, i.e., with the agent. *)
  module Agent : sig
    type t = agent


    (** the agent's id.  *)
    type id


    (** abstract ordered type to quantify reliability of agents.*)
    type reliability


    (** [register name] registers a new agent.

        The provided [package] and [name] pair should be unique. If
        an agent with the given [package:name] is already registered,
        the registration of the new agent will fail.

        @param desc a description of the registered agent.
        @param package a namespace of the registered agent.
        @param reliability is the amount of agent's trustworthiness
        (defaults to trustworthy).
    *)
    val register :
      ?desc:string ->
      ?package:string ->
      ?reliability:reliability -> string -> agent


    (** [registry ()] is the current registry of agents.

        Each registered agent is represented by its ID, which could be
        inspected or used to change the reliability.
    *)
    val registry : unit -> id list


    (** [name id] is the name of the agent.  *)
    val name : id -> string


    (** [desc id] is the overall description of the agent.  *)
    val desc : id -> string


    (** [reliability id] is the current reliability of the agent  *)
    val reliability : id -> reliability


    (** [set_reliability id] changes the reliability of the agent.  *)
    val set_reliability : id -> reliability -> unit


    (** {3 Reliability Levels} *)


    (** The highest level of reliability.

        The information was obtained from a source in which we trust
        absolutely. There are no possible doubts that this information
        is incorrect.

        The authorative source is definitional and is always
        preferred to any other information.

        It could be compared to the information obtained from the
        official legislative document, mathematical textbook, etc.

    *)
    val authorative : reliability


    (** A highly reliable source.

        This source rarely, if ever, provides inaccurate results.

        It could be compared to the information obtained from a
        well-known expert, peer-reviewed journal, etc. *)
    val reliable    : reliability


    (** A very reliable source.

        This source provides accurate results most of the time. Only
        in exceptional cases it could provide an inaccurate result.

        It could be compared to the information obtained from a field
        expert.
    *)
    val trustworthy : reliability


    (** Nearly reliable source.

        This source sometimes provides inaccurate information or relies
        on doubtful methods. It is still providing useful and accurate
        information more often than not.

        It could be compared to the information obtained from a
        knowledgeable person.
    *)
    val doubtful    : reliability


    (** A not worthwhile source.

        This source provides accurate information slightly more often
        than inaccurate. Though it is a bad source of information,
        many unreliable source could be combined to.

        It could be compared to the knowledge obtained from gossips, i.e.,
        from not knowledgeable persons close to the knowledge.
    *)
    val unreliable  : reliability


    (** prints the agent information.  *)
    val pp : Format.formatter -> t -> unit


    (** prints the agent's id.  *)
    val pp_id : Format.formatter -> id -> unit


    (** prints the reliability level.  *)
    val pp_reliability : Format.formatter -> reliability -> unit
  end


  (** Partially ordered sets with the least element.

      The Domain is fundamental structure for the Knowledge
      representation as domains are used to represent property
      values. Basically, all information is represented with
      domains. Domains capture the idea of partial information or
      an approximation of information.

      A domain is a set equipped with the partial order, which orders
      elements of this set by their information content, and the least
      element [empty] or [bot], which is not greater than any other
      element of this set. The [empty] element indicates an absence of
      knowledge.

      The resulting structure is not strictly a domain, the only
      missing element is the maximal element. The role of the is taken
      by values of type conflict, which denote the [top] element
      equipped with the diagnostic information. Therefore, type
      [('a,conflict) result] is a directed-complete partial order, or
      a Scott-Ershov domain, or just a domain..

      The domain structure is very general and the [join] operator is
      induced by the specified [order]. However, it is possible to
      upgrade the structure, used to represent a property, to lattice
      or lattice-like structure, by providing a custom [join] function.

  *)
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

        The returned value is an instance of the domain type class
        that is used for declaration of a property. The instance is
        not nominal and is purely structural, the [name] argument is
        used only for introspection and better error messages.
    *)
    val define :
      ?inspect:('a -> Base.Sexp.t) ->
      ?join:('a -> 'a -> ('a,conflict) result) ->
      empty:'a ->
      order:('a -> 'a -> Order.partial) -> string -> 'a domain


    (** [total empty order name] defines a domain from the total [order].

        Defines an ordinal domain, where the partial order is inferred
        from the total order, so that [a <= b iff a < b].

        The Hasse diagram of the ordinal domain:

        {v
             o xN
             |
             :
             |
             o x2
             |
             o x1
             |
             o bot
         v}
    *)
    val total :
      ?inspect:('a -> Base.Sexp.t) ->
      ?join:('a -> 'a -> ('a,conflict) result) ->
      empty:'a ->
      order:('a -> 'a -> int) ->
      string -> 'a domain


    (** [flat empty equal name] defines a flat domain.

        A flat domain has one element that is less than all other
        elements except itself and any other two elements that are
        not empty are non-comparable.

        The Hasse diagram of the flat domain:

        {v
             x1 x2  ... xN
             o  o       o
             |  |       |
             +--+-+...--+
                  |
                  o
                 bot
        v}

    *)
    val flat :
      ?inspect:('a -> Base.Sexp.t) ->
      ?join:('a -> 'a -> ('a,conflict) result) ->
      empty:'a ->
      equal:('a -> 'a -> bool) ->
      string -> 'a domain


    (** [optional ~equal name] a flat domain with [None] as bot.

        Wrapping any data type into the option data type yields a flat
        domain. For example,

        {[
          let tribool = optional ~equal:bool_equal "tribool"
        ]}

        is a tribool domain, where the property value could be either
        unknown ([None]), true ([Some true]), or false ([Some false].
    *)
    val optional :
      ?inspect:('a -> Base.Sexp.t) ->
      ?join:('a -> 'a -> ('a,conflict) result) ->
      equal:('a -> 'a -> bool) ->
      string -> 'a option domain


    (** [mapping total_order data_equal name] a point-wise mapping domain.

        Finite mapping naturally form domains, if every key in the
        mapping is considered an independent kind of information, and
        an absence of a key indicates the absence of that kind of
        information.


        The upper bound of two mapping is the point-wise union of
        them, unless there is a key, which is present in both mapping
        with different values (compared with [data_equal]). In the
        latter case, the upper bound is the [conflict].

        The partial order between [x] and [y] is defined as follows:
        - [EQ] iff mappings are structurally equal;
        - [LT] iff [y] contains all bindings of [x] and [x <> y];
        - [GT] iff [x] contains all bindings of [y] and [x <> y];
        - [NC] iff neither of the above rules applicable.
    *)
    val mapping :
      ('a,'e) Map.comparator ->
      ?inspect:('d -> Base.Sexp.t) ->
      equal:('d -> 'd -> bool) ->
      string ->
      ('a,'d,'e) Map.t domain


    (** [powerset total name] defines a set of all subsets domain.

        Sets ordered by inclusion is a natural domain. The join
        operator is the set union, and the order operator is the
        [is_subset] function.
    *)
    val powerset : ('a,'e) Set.comparator ->
      ?inspect:('a -> Sexp.t) ->
      string ->
      ('a,'e) Set.t domain


    (** [opinions empty equal name] defines an opinionated domain.

        In the opinionated domain, the order of elements is defined by
        the total reliability of agents that support this
        information. The more trustworthy the agent and the more
        agents support data, the higher the data will be in the chain.

        See corresponding [suggest], [propose], and [resolve] operators.
    *)
    val opinions :
      ?inspect:('a -> Sexp.t) ->
      empty:'a ->
      equal:('a -> 'a -> bool) ->
      string ->
      'a opinions domain


    (** [string] is flat domain with an empty string at the bottom.  *)
    val string : string domain


    (** [bool] is the tribool domain. *)
    val bool : bool option domain


    (** [obj] is a flat domain with a nil object at the bottom.  *)
    val obj : ('a,_) cls -> 'a obj domain


    (** [empty domain] is the bottom of the [domain].  *)
    val empty : 'a t -> 'a


    (** [is_empty domain x] is [true] if [x] is [empty domain].  *)
    val is_empty : 'a t -> 'a -> bool


    (** [order domain x y] orders [x] and [y] according to the [domain] order.  *)
    val order : 'a t -> 'a -> 'a -> Order.partial


    (** [join domain x y] is the upper bound of [x] and [y].  *)
    val join  : 'a t -> 'a -> 'a -> ('a,conflict) result


    (** [inspect domain x] introspects [x].  *)
    val inspect : 'a t -> 'a -> Base.Sexp.t


    (** [name domain] is the domain name.  *)
    val name : 'a t -> string
  end


  (** Persistence type class.

      A instance of the Persistent type class could be provided to the
      property declaration to make this property persistent. See the
      [Class.property] function.
  *)
  module Persistent : sig
    type 'a t = 'a persistent


    (** [define to_string of_string] derives an instance of [persistent].

        Uses the provided [of_string] and [to_string] to deserialize
        and serialize properties.
    *)
    val define :
      to_string:('a -> string) ->
      of_string:(string -> 'a) ->
      'a persistent


    (** [derive to_persistent of_persistent] derives an instance from
        other instance. *)
    val derive :
      to_persistent:('a -> 'b) ->
      of_persistent:('b -> 'a) ->
      'b persistent -> 'a persistent

    (** [of_binable t] derives [persistent] from the binable instance [t].  *)
    val of_binable : (module Binable.S with type t = 'a) -> 'a persistent


    (** string is a persistent data type.  *)
    val string : string persistent

    (** [list t] derives persistence for a list.  *)
    val list : 'a persistent -> 'a list persistent

    (** [sequence t] derives persistent for a sequence.  *)
    val sequence : 'a persistent -> 'a Sequence.t persistent

    (** [array t] derives persistent for an array.  *)
    val array : 'a persistent -> 'a array persistent

    (** [set order t] derives persistent for a set.  *)
    val set : ('a,'c) Set.comparator -> 'a t -> ('a,'c) Set.t persistent


    (** [map order t] derives persistent for a map.  *)
    val map : ('k,'c) Map.comparator -> 'k t -> 'd t -> ('k,'d,'c) Map.t persistent
  end

  (** Conflicting information.

      Conflicts occur when two conflicting values are provided for a
      property. When conflict happens the knowledge dependent
      computation diverges and evaluation stops with the value of type
      [conflict] (unless it is intercepted).

      The conflict value, essentially serves as the upper bound to all
      user provided domains, thus closing the poset structure and
      turning it into a real domain. Although there could be many
      values of type [conflict] it is better to think of them as one
      value [top], equipped with diagnostic information.
  *)
  module Conflict : sig
    type t = conflict = ..


    (** prints the conflict  *)
    val pp : Format.formatter -> conflict -> unit


    (** the s-expression denoting the conflict. *)
    val sexp_of_t : t -> Sexp.t


    (** registers a printer for user specified extension of the conflict type.

        The function shall return [Some s] for the variant added by
        the user and [None] for all other variants.
    *)
    val register_printer : (t -> string option) -> unit
  end


  (** Fully qualified names.  *)
  module Name : sig
    type t = name [@@deriving bin_io, compare, sexp]


    (** [create ?package name] creates a fully qualified name.

        The [package] and [name] strings can contain any characters
        all treated literally, e.g., the [:] character
        won't be treated as a separator neither it will break
        anything.

        If [package] is not specified, then it defaults to the
        ["user"] package.
    *)
    val create : ?package:string -> string -> t


    (** [read ?package input] reads a full name from input.

        This function will parse the [input] and return a
        fully-qualified name that corresponds to the input, using
        [package] as the currently opened package. The input syntax
        is {v
          name = string, ":", string
               | ":", string
               | string
          string = ?a sequence of any characters?
        v}


        Not all characters in the [input] string are treated
        literally, the following two characters have a special
        interpretation:
        - ['\\'] the escape character;
        - [':'] the package separator character.

        The escape character disables a special interpretation of the
        consequent character. The package separator denotes the place
        in the input where the package name ends and the name part
        starts.

        If [package] is specified then it is treated literally (as in
        the [create] function). The same as in [create] it defaults to
        the ["user"] package.

        If [input] doesn't denote a fully qualified name (i.e., there
        is no [':'] special character in [input], then the read name
        is qualified with the passed [package], otherwise the package
        is defined by the [input].

        The function is expected to work with the output of the [show]
        function, so that for all [n], [read (show n) = n]. However,
        it is robust enough to accept any user inputs, even if it is
        not a well-formed input, e.g., when an escape character is
        used to escaped a non-special character or when input contains
        more than one unescaped separators. In case of invalid input,
        all special characters that doesn't make sense are treated
        literally and the first special [':'] denotes the end of the
        package field. If the input is not valid, then it is possible
        that [show (read s) <> s], since the output of [show] is
        always valid, e.g.

        [show@@read "hello:cruel:world" = "hello:cruel\\:world"]
    *)
    val read : ?package:string -> string -> t


    (** [show name] is the readable representation of [name].

        The name is represented as [<package>:<name>], with all
        special characters escaped. See the {!read} function for
        more information.
    *)
    val show : t -> string

    (** [unqualified name] is the unqualified name.

        Returns the name without the package specifier.
    *)
    val unqualified : t -> string

    (** [package name] is the package of the [name].  *)
    val package : t -> string

    (** [str () x = show x] shows [x].

        This function is useful with printf-style functions that
        output to a string. *)
    val str : unit -> t -> string

    (** [hash name] the [name] hash. *)
    val hash : t -> int

    include Base.Comparable.S with type t := t
    include Binable.S with type t := t
    include Stringable.S with type t := t
    include Pretty_printer.S with type t := t
  end

  (** the s-expression denoting the conflict. *)
  val sexp_of_conflict : conflict -> Sexp.t
end
