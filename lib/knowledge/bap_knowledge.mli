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

  val run : 'a cls -> 'a obj t -> state -> ('a value * state, conflict) result

  module Syntax : sig
    include Monad.Syntax.S with type 'a t := 'a t


    (** [x-->p] is [collect p x] *)
    val (-->) : 'a obj -> ('a,'p) slot -> 'p t


    (** [p >>> f] is [promise p f]  *)
    val (>>>) : ('a,'p) slot -> ('a obj -> 'p t) -> unit


    (** [c // s] is [Object.read c s]  *)
    val (//) : 'a cls -> string -> 'a obj t
  end


  include Monad.S with type 'a t := 'a t
                   and module Syntax := Syntax
  include Monad.Fail.S with type 'a t := 'a t
                        and type 'a error = conflict


  module Class : sig
    type 'a t = 'a cls
    type top = unit
    include Type_equal.Injective with type 'a t := 'a t

    val declare : ?desc:string -> ?package:string -> string -> 'a -> ('a -> top) cls
    val derived : ?desc:string -> ?package:string -> string -> 'a cls -> 'b -> ('b -> 'a) cls
    val abstract : (_ -> 'a) cls -> 'a cls
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


    val id : 'a obj -> Int63.t

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


  (** A symbol is an object with unique name.

      Sometimes it is necessary to refer to an object by name, so that
      a chosen name will always identify the same object. Finding or
      creating an object by name is called "interning" it. A symbol
      that has a name is called an "interned symbol". However we
      stretch the boundaries of the symbol idea, by treating all other
      objects as "uninterned symbols". So that any object could be
      treated as a symbol.

      To prevent name clashing, that introduce unwanted equalities,
      we employ the system of packages, where each symbol belongs
      to a package, called its home package. The large system design
      is leveraged due to the mechanism of symbol importing, where the
      same symbol could be referenced from different packages (see
      [import] and [in_package] functions, for more information).

      {3 Symbol syntax}

      The [read] function enables translation from symbol textual
      representation into an object.  The symbol syntax is designed to
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
      'a cls -> 'a obj knowledge

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

    val obj : 'a cls -> 'a obj domain
  end
end
