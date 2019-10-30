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
      confused with any other natural ordering of the associated value
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
    val name : ('a,_) cls -> string

    (** [package cls] is the class package.  *)
    val package : ('a,_) cls -> string


    (** [fullname cls] is a fully qualified class name.  *)
    val fullname : ('a,_) cls -> string

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
    val merge : ?on_conflict:[<
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
    val name : ('a,'p) slot -> string

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


    (** [registry ()] is the current registry of agents.*)
    val registry : unit -> id list

    val name : id -> string
    val desc : id -> string
    val reliability : id -> reliability
    val set_reliability : id -> reliability -> unit

    val authorative : reliability
    val reliable    : reliability
    val trustworthy : reliability
    val doubtful    : reliability
    val unreliable  : reliability

    val pp : Format.formatter -> t -> unit
    val pp_id : Format.formatter -> id -> unit
    val pp_reliability : Format.formatter -> reliability -> unit
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
      ?join:('a -> 'a -> ('a,conflict) result) ->
      empty:'a ->
      order:('a -> 'a -> Order.partial) -> string -> 'a domain

    val total :
      ?inspect:('a -> Base.Sexp.t) ->
      ?join:('a -> 'a -> ('a,conflict) result) ->
      empty:'a ->
      order:('a -> 'a -> int) ->
      string -> 'a domain

    val flat :
      ?inspect:('a -> Base.Sexp.t) ->
      ?join:('a -> 'a -> ('a,conflict) result) ->
      empty:'a ->
      equal:('a -> 'a -> bool) ->
      string -> 'a domain

    val optional :
      ?inspect:('a -> Base.Sexp.t) ->
      ?join:('a -> 'a -> ('a,conflict) result) ->
      equal:('a -> 'a -> bool) ->
      string -> 'a option domain

    val mapping :
      ('a,'e) Map.comparator ->
      ?inspect:('d -> Base.Sexp.t) ->
      equal:('d -> 'd -> bool) ->
      string ->
      ('a,'d,'e) Map.t domain

    val powerset : ('a,'e) Set.comparator ->
      ?inspect:('a -> Sexp.t) ->
      string ->
      ('a,'e) Set.t domain

    val opinions :
      ?inspect:('a -> Sexp.t) ->
      empty:'a ->
      equal:('a -> 'a -> bool) ->
      string ->
      'a opinions domain

    val string : string domain
    val bool : bool option domain

    val obj : ('a,_) cls -> 'a obj domain


    val empty : 'a t -> 'a
    val is_empty : 'a t -> 'a -> bool
    val order : 'a t -> 'a -> 'a -> Order.partial
    val join  : 'a t -> 'a -> 'a -> ('a,conflict) result
    val inspect : 'a t -> 'a -> Base.Sexp.t
    val name : 'a t -> string
  end

  module Persistent : sig
    type 'a t = 'a persistent

    val define :
      to_string:('a -> string) ->
      of_string:(string -> 'a) ->
      'a persistent

    val derive :
      to_persistent:('a -> 'b) ->
      of_persistent:('b -> 'a) ->
      'b persistent -> 'a persistent

    val of_binable : (module Binable.S with type t = 'a) -> 'a persistent

    val string : string persistent

    val list : 'a persistent -> 'a list persistent
    val sequence : 'a persistent -> 'a Sequence.t persistent
    val array : 'a persistent -> 'a array persistent

    val set : ('a,'c) Set.comparator -> 'a t -> ('a,'c) Set.t persistent
    val map : ('k,'c) Map.comparator -> 'k t -> 'd t -> ('k,'d,'c) Map.t persistent
  end

  module Data : sig
    type +'a t
    type 'a ord

    val atom : ('a,_) cls -> 'a obj -> 'a t knowledge
    val cons : ('a,_) cls -> 'a t -> 'a t -> 'a t knowledge

    val case : ('a,_) cls -> 'a t ->
      null:'r knowledge ->
      atom:('a obj -> 'r knowledge) ->
      cons:('a t -> 'a t -> 'r knowledge) -> 'r knowledge


    val id : 'a obj -> Int63.t


    module type S = sig
      type t [@@deriving sexp]
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end

    val derive : ('a,_) cls -> (module S
                                 with type t = 'a t
                                  and type comparator_witness = 'a ord)
  end

  module Conflict : sig
    type t = conflict = ..
    val pp : Format.formatter -> conflict -> unit
    val sexp_of_t : t -> Sexp.t
  end

  val sexp_of_conflict : conflict -> Sexp.t
end
