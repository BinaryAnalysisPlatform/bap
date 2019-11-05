open Core_kernel
open Monads.Std

type 'a knowledge
module Knowledge : sig
  type 'a t = 'a knowledge
  type (+'k,+'s) cls
  type +'a obj
  type +'a value
  type (+'a,'p) slot
  type 'p domain
  type 'a persistent
  type state
  type conflict = ..

  type agent
  type 'a opinions

  (** state with no knowledge  *)
  val empty : state

  val of_bigstring : Bigstring.t -> state

  val to_bigstring : state -> Bigstring.t

  val pp_state : Format.formatter -> state -> unit

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

  (** Orders knowledge by information content.

      The [Order.partial] is a generalization of the total order,
      which is used to compare the amount of information in two
      specifications of knowledge.

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
  *)
  module Class : sig
    type (+'k,'s) t = ('k,'s) cls


    (** [declare ?desc ?package name sort] declares a new
        class with the given [name] and [sort] index. *)
    val declare : ?desc:string -> ?package:string -> string ->
      's -> ('k,'s) cls


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


    (** [property ?desc ?persistent ?package cls name dom] declares
        a new property of all instances of class [k].

        Returns a slot, that is used to access this property.
    *)
    val property :
      ?desc:string ->
      ?persistent:'p persistent ->
      ?package:string ->
      ('k,_) cls -> string -> 'p domain -> ('k,'p) slot

    val name : ('a,_) cls -> string
    val package : ('a,_) cls -> string
    val fullname : ('a,_) cls -> string


    (** [sort cls] returns the sort index of the class [k].  *)
    val sort : ('k,'s) cls -> 's
  end

  module Object : sig
    type 'a t = 'a obj
    type 'a ord

    (** [create] is a fresh new object with an idefinite extent.  *)
    val create : ('a,_) cls -> 'a obj knowledge

    (** [scoped scope] pass a fresh new object to [scope].

        The extent of the created object is limited with the extent
        of the function [scope].*)
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


    val id : 'a obj -> Int63.t

    module type S = sig
      type t [@@deriving sexp]
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end

    val derive : ('a,'d) cls -> (module S
                                  with type t = 'a obj
                                   and type comparator_witness = 'a ord)
  end

  module Value : sig
    type 'a t = 'a value
    type 'a ord
    include Type_equal.Injective with type 'a t := 'a t

    val empty : ('a,'b) cls -> ('a,'b) cls value
    val order : 'a value -> 'a value -> Order.partial
    val join : 'a value -> 'a value -> ('a value,conflict) result
    val merge : ?on_conflict:[
      | `drop_old
      | `drop_new
      | `drop_right
      | `drop_left
    ] -> 'a value -> 'a value -> 'a value


    (** [cls x] is the class of [x]   *)
    val cls : ('k,'s) cls value -> ('k,'s) cls
    val get : ('k,'p) slot -> ('k,_) cls value -> 'p
    val put : ('k,'p) slot -> ('k,'s) cls value -> 'p -> ('k,'s) cls value

    (** [refine v s] refines the sort of [v] to [s].  *)
    val refine : ('k,_) cls value -> 's -> ('k,'s) cls value

    module type S = sig
      type t [@@deriving sexp]
      val empty : t
      val domain : t domain
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end

    val derive : ('a,'s) cls ->
      (module S
        with type t = ('a,'s) cls t
         and type comparator_witness = ('a,'s) cls ord)

    val pp : Format.formatter -> 'a value -> unit

    val pp_slots : string list -> Format.formatter -> 'a value -> unit
  end

  module Slot : sig
    type ('a,'p) t = ('a,'p) slot

    val domain : ('a,'p) slot -> 'p domain
    val cls : ('a,_) slot -> ('a, unit) cls
    val name : ('a,'p) slot -> string
    val fullname : ('a,'p) slot -> string
    val desc : ('a,'p) slot -> string
  end


  (** A symbol is an object with unique name.

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

  module Agent : sig
    type t = agent
    type id
    type reliability

    val register :
      ?desc:string ->
      ?package:string ->
      ?reliability:reliability -> string -> agent

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
