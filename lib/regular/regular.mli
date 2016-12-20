(** Regular Library.   *)

open Core_kernel.Std

(** Interface that should support any regular data type.

    {2 Overview}

    A regular type is catch all term for all data types that are
    regular, like numbers, characters, strings and their algebraic
    closure. A proper term for such types would be inductive types. A
    regular type always has a concrete representation, it is
    printable, comparable (with total order), hashable, etc.

    To contrast, functions, closures, proxies, descriptors, and any
    co-inductive types are non-regular. The main difference, is that
    regular type is self contained, where non-regular types, usually
    represent something that can be only observed, but not really
    represented with the data type.

    On the border line we have [Opaque] data types. These are types,
    that pretend to be [Regular], but we can't actually inspect their
    representation.


    {2 Features}

    So what the library actually provides. First of all it defines the
    {!Regular} interface, that each regular data type is expected to
    implement. This includes a full set of [Io] functions, that allows
    one to read, write and serialize values of the type (see {!Data}
    interface). It also provides an inteface for creating different
    collections from the values of that type, including trees,
    hashtables, maps, sets, etc. Also, it describes the whole algebra
    of comparison functions. For the opaque data types an interface
    called {!Opaque} is provided, that is a proper subset of the
    {!Regular}. It doesn't provide printing and serialization, but has
    everything that can be derived for a type with a total order
    (e.g., containers, comparison function, etc).

    A functor is provided for each interface, that requires the
    minimal implementation, and derives the rest.

    Finally, a {!Bytes} module is provided that facilitates the
    transfer from mutable to immutable {!Strings}. It is the extension
    of OCaml standard Bytes module, enhanced with the expected set of
    functions.

    @see <
    https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/>
    Core Kernel Documentation
*)
module Std : sig

  (** {3 Type definitions}  *)

  (** bytes is a mutable sequence of bytes that has a fixed length.*)
  type bytes = Bytes.t
  [@@deriving bin_io, compare, sexp]


  (** {4 Reader and Writer type classes}

      Each type class is an abstraction of a tuple of function.*)

  (** an interface for reading a value from the input. *)
  type 'a reader

  (** an interface for writing a value to the output.  *)
  type 'a writer


  (** a string that digests data  *)
  type digest [@@deriving bin_io, compare, sexp]

  (** ['a printer] constructs a printer type for arbitrary type ['a].

      A value of type ['a printer] is a function that is expected by
      the [%a] specifier of the [Format.printf] family of functions.*)
  type 'a printer = Format.formatter -> 'a -> unit

  (** Printable data structures.  *)
  module Printable : sig

    (** Interface for printing data.

        Printable interface is implemented by a significant amount of BAP
        types.

        The most common way of using it, is to use the [%a] format
        specifier in the [Format.printf] family of functions. The
        following code is idiomatic:

        {v
          open Format
          printf "bil = %a\n" Bil.pp prog
        v}
    *)
    module type S = sig

      (** type of printable  *)
      type t

      (** [to_string x] returns a human-readable representation of [x]  *)
      val to_string : t -> string

      (** [str () t] is formatted output function that matches "%a"
          conversion format specifier in functions, that prints to string,
          e.g., [sprintf], [failwithf], [errorf] and, suprisingly all
          [Lwt] printing function, including [Lwt_io.printf] and logging
          (or any other function with type ('a,unit,string,...)
          formatN`. Example:

          {[Or_error.errorf "type %a is not valid for %a"
            Type.str ty Exp.str exp]} *)
      val str : unit -> t -> string

      (** synonym for [str]  *)
      val pps : unit -> t -> string

      (** will print to a standard [output_channel], useful for using in
          [printf], [fprintf], etc. *)
      val ppo : out_channel -> t -> unit

      (** prints a sequence of values of type [t] *)
      val pp_seq : Format.formatter -> t Sequence.t -> unit


      (** this will include [pp] function from [Core] that has type
          {{!printer}[t printer]}, and can be used in [Format.printf]
          family of functions *)
      include Pretty_printer.S     with type t := t
    end

    (** Implement [Printable] interface from the minimum implementation.


        Takes a module [T], that provides a function [pp : t printer]
        and a [module_name], and implements the rest of the
        [Printable] interface.

    *)
    module Make (M : sig
        include Pretty_printer.S
        val module_name : string option
      end) : S with type t := M.t
  end

  (** Lazy sequence.

      This is the extension of Core_kernel's [Sequence] module

      @see
      <https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/Std/Sequence.mod/>
      [Sequence]

  *)
  module Seq : sig
    type 'a t = 'a Sequence.t [@@deriving bin_io, compare, sexp]
    include module type of Sequence with type 'a t := 'a t

    (** for compatibility with Core <= 111.28  *)
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int

    val of_array : 'a array -> 'a t

    val cons : 'a -> 'a t -> 'a t

    val is_empty : 'a t -> bool

    val pp : 'a printer -> 'a t printer
  end

  (** Abbreviation for ['a Sequence.t]  *)
  type 'a seq = 'a Seq.t [@@deriving bin_io, compare, sexp]

  (** [x ^:: xs] is a consing operator for sequences  *)
  val (^::) : 'a -> 'a seq -> 'a seq


  (** Data types support module.

      Data is a collection of bits, that can be stored and loaded on
      some media. This module provides functions for doing this in a
      generic way.

      To be on a safe side, we insist on explicit versioning of each
      data representation. Otherwise, any OCaml value is suitable for
      this module.

      The module defines interfaces {!Data.S} that describes the
      expected set of functions for data. It also defines a {!Cache.S}
      interface, that is useful for caching data that is hard to
      obtain.*)
  module Data : sig


    (** [copy buf obj pos] is a method to copy object [obj] into a buffer
        [buf], starting from a position [pos] and will return the number
        of bytes written.  XXX: we need the amount of bytes beforehand.
    *)
    type ('a,'b) copy = 'b -> 'a -> int -> unit


    (** [dump out obj] a type of functions that outputs a value [obj]
        into output [out] *)
    type ('a,'b) dump = 'b -> 'a -> unit

    type lexbuf  = Lexing.lexbuf
    type scanbuf = Scanf.Scanning.scanbuf

    (** Versioned interfaces  *)
    module Versioned : sig

      (** a data type with a version  *)
      module type S = sig
        (** type of data  *)
        type t

        (** version of data representation  *)
        val version : string
      end
    end

    (** [marshal_reader (module T)] returns a reader in OCaml
        [Marshal] format.  *)
    val marshal_reader : (module T with type t = 'a) -> 'a reader

    (** [marshal_writer (module T)] returns a writer in OCaml
        [Marshal] format.  *)
    val marshal_writer : (module T with type t = 'a) ->
      'a writer

    (** [sexp_reader (module T)] returns a reader in  sexp format.  *)
    val sexp_reader : (module Sexpable with type t = 'a) -> 'a reader
    (** [sexp_writer (module T)] returns a writer in  sexp format.  *)
    val sexp_writer : (module Sexpable with type t = 'a) -> 'a writer

    (** [bin_reader (module T)] returns a reader in  binprot format.  *)
    val bin_reader : (module Binable with type t = 'a) -> 'a reader

    (** [bin_writer (module T)] returns a writer in  binprot format.  *)
    val bin_writer : (module Binable with type t = 'a) -> 'a writer

    (** [pretty_printer (module T] creates a writer that uses [T.pp]
        function for outputting.  *)
    val pretty_writer : (module Pretty_printer.S with type t = 'a) -> 'a writer


    (** Data type interface.

        Types that implement this interface have a concrete
        representation, that can be serialized and deserialized.

        The only requirement is to provide a [version] value, that
        represents a version number of the representation. Every time
        the representation changes, the version should be incremented.

        The interface allows to implement more than one format and
        several versions of the same format. By default, the OCaml
        Marshal format is used.

        All [Regular] modules include this interface. The [Regular]
        modules provide [sexp] and [bin_io] formats in addition to
        the [Marshal]. They may provide more formats.

        {3 Example}

        Given a module [Bil] that implements [Regular] interface, it
        is possible to read a value of type [Bil.t] as simple, as:

        {[
          let prog = Bil.Io.read filename
        ]}
    *)
    module type S = sig

      (** type constructor  *)
      type t

      (** [name,Ver v,desc] information attached to a particular
          reader or writer.  *)
      type info = string * [`Ver of string] * string option

      (** Data representation version. After any change in data
          representation the version should be increased.

          Serializers that are derived from a data representation must have
          the same version as a version of the data structure, from which
          it is derived. This kind of serializers can only read and write
          data of the same version.

          Other serializers can actually read and write data independent
          on its representation version. A serializer, that can't store
          data of current version simply shouldn't be added to a set of
          serializers.

          It is assumed, that if a reader and a writer has the same name
          and version, then whatever was written by the writer should be
          readable by the reader. The round-trip equality is not required,
          thus it is acceptable if some information is lost.

          It is also possible, that a reader and a writer that has the
          same name are compatible. In that case it is recommended to use
          semantic versioning.*)
      val version : string

      (** [size_in_bytes ?ver ?fmt datum] returns the amount of bytes
          that is needed to represent [datum] in the given format
          and version *)
      val size_in_bytes : ?ver:string -> ?fmt:string -> t -> int

      (** [of_bytes ?ver ?fmt bytes] deserializes a value from bytes.  *)
      val of_bytes : ?ver:string -> ?fmt:string -> bytes -> t

      (** [to_bytes ?ver ?fmt datum] serializes a [datum] to a sequence of
          bytes. *)
      val to_bytes : ?ver:string -> ?fmt:string -> t -> bytes

      (** [blit_to_bytes ?ver ?fmt buffer datum offset] copies a
          serialized representation of datum into a [buffer], starting from
          the [offset].  *)
      val blit_to_bytes : ?ver:string -> ?fmt:string -> bytes -> t -> int -> unit

      (** [of_bigstring ?ver ?fmt buf] deserializes a datum from bigstring  *)
      val of_bigstring : ?ver:string -> ?fmt:string -> bigstring -> t

      (** [of_bigstring ?ver ?fmt datum] serializes a datum to a sequence of
          bytes represented as bigstring *)
      val to_bigstring : ?ver:string -> ?fmt:string -> t -> bigstring

      (** [blit_to_bigstring ?ver ?fmt buffer datum offset] copies a
          serialized representation of datum into a [buffer], starting from
          [offset].  *)
      val blit_to_bigstring : ?ver:string -> ?fmt:string -> bigstring -> t -> int -> unit

      (** Input/Output functions for the given datum.*)
      module Io : sig

        (** [read ?ver ?fmt file] reads datum from a [file]  *)
        val read  : ?ver:string -> ?fmt:string -> string -> t

        (** [load ?ver ?fmt channel] loads datum from the input channel  *)
        val load  : ?ver:string -> ?fmt:string -> in_channel -> t

        (** [read_all ?ver ?fmt ?rev channel] reads a sequence of datums
            stored in the storage accessed via [channel] and returns it as a
            list. If [rev] is [true] (defaults to [false]) then a list will
            be reversed (slightly faster).  *)
        val load_all : ?ver:string -> ?fmt:string -> ?rev:bool -> in_channel -> t list

        (** [scan ?ver ?fmt channel] creates a stream of data, that
            are loaded consequently from the channel *)
        val scan  : ?ver:string -> ?fmt:string -> in_channel -> (unit -> t option)

        (** [write ?ver ?fmt file datum] writes the [datum] to the [file] *)
        val write  : ?ver:string -> ?fmt:string -> string -> t -> unit

        (** [save ?ver ?fmt channel datum] saves the [datum] to the [channel] *)
        val save  : ?ver:string -> ?fmt:string -> out_channel -> t -> unit


        (** [save_all ?ver ?fmt data channel] saves a list of data into
            a [channel] *)
        val save_all : ?ver:string -> ?fmt:string -> out_channel -> t list -> unit

        (** [dump ?ver ?fmt chan stream] dumps a [stream] (represented
            by a [next] function) into channel.  *)
        val dump  : ?ver:string -> ?fmt:string -> out_channel -> (unit -> t option) -> unit

        (** [show ?ver ?fmt datum] saves datum to standard output
            channel, using a [default_printer] if [fmt] is not
            specified. Does nothing the printer is not set up.  *)
        val show  : ?ver:string -> ?fmt:string -> t -> unit

        (** [print ?ver ?fmt ppf] prints datum to a given formatter, using
            a [default_printer] if [fmt] is left unspecified.  *)
        val print : ?ver:string -> ?fmt:string -> Format.formatter -> t -> unit

      end

      (** Data cache.

          Store and retrieve data from cache. The cache can seen as a
          persistant weak key-value storage. Data stored here can
          disappear at any time, but can survive for a long time
          (outliving the program). The library by itself doesn't provide
          a caching service for any type, only the interface. The
          caching service can be added externally (e.g., via a plugin).

          The caching infrastructure provides only facilities for
          storing and loading data. In fact this is just a weak
          key-value storage. A weak, because storage is allowed to
          to loose data.

          As a key, we use [digest] that is underneath the hood is and
          md5 sum of arguments used to build the cached data.

          Let's take for example a function that builds some control
          flow graphs. It has three parameters, one is an optional
          [debug], that doesn't affect the algorithm, and the rest two
          has type [string] and [int] correspondingly. The following
          code will try to load result from a cache, using a digest of
          the arguments, and if is not available, the the graph will be
          computed and stored in the cache. This function will work even
          if there is no caching service. Of course, there will be no
          benefits, since the [save] function will just immediately
          forget its argument.

          {[
            let compute_graph ?(debug=false) x y : Graphs.Cfg.t =
              let digest =
                Data.Cache.digest ~namespace:"example" "%s%d" x y in
              match Graphs.Cfg.Cache.load digest with
              | Some g -> g
              | None ->
                let g = build_graph ?debug x y in
                Graphs.Cfg.Cache.save digest g;
                g
          ]}

          Note: it is only reasonable to use caching for data types,
          that take significant amount of time to create.
      *)
      module Cache : sig

        (** [load id] load data previously stored under give [id]  *)
        val load : digest -> t option

        (** [save id data] store data under given [id]. If something is
            already stored, then it will be overwritten.*)
        val save : digest -> t -> unit
      end


      (** [add_reader ?desc ~ver name reader] registers a new [reader]
          with a provided [name], version [ver] and optional description
          [desc] *)
      val add_reader : ?desc:string -> ver:string -> string -> t reader -> unit


      (** [add_writer ?desc ~ver name writer] registers a new [writer]
          with a provided [name], version [ver] and optional description
          [desc] *)
      val add_writer : ?desc:string -> ver:string -> string -> t writer -> unit

      (** [available_reader ()] lists available readers for the data type  *)
      val available_readers : unit -> info list

      (** [default_reader] returns information about default reader  *)
      val default_reader : unit -> info

      (** [set_default_reader ?ver name] sets new default reader. If
          version is not specifed then the latest available version is
          used. Raises an exception if a reader with a given name doesn't
          exist.  *)
      val set_default_reader : ?ver:string -> string -> unit

      (** [with_reader ?ver name operation] temporary sets a default
          reader to a reader with a specified name and version. The default
          reader is restored after [operation] is finished.  *)
      val with_reader : ?ver:string -> string -> (unit -> 'a) -> 'a


      (** [available_writer ()] lists available writers for the data type  *)
      val available_writers : unit -> info list

      (** [default_writer] returns information about the default writer  *)
      val default_writer : unit -> info

      (** [set_default_writer ?ver name] sets new default writer. If
          version is not specifed then the latest available version is
          used. Raises an exception if a writer with a given name doesn't
          exist.  *)
      val set_default_writer : ?ver:string -> string -> unit

      (** [with_writer ?ver name operation] temporary sets a default
          writer to a writer with a specified name and version. The default
          writer is restored after [operation] is finished.  *)
      val with_writer : ?ver:string -> string -> (unit -> 'a) -> 'a


      (** [default_writer] optionally returns an information about
          default printer *)
      val default_printer : unit -> info option

      (** [set_default_printer ?ver name] sets new default printer. If
          version is not specifed then the latest available version is
          used. Raises an exception if a printer with a given name doesn't
          exist.  *)
      val set_default_printer : ?ver:string -> string -> unit

      (** [with_printer ?ver name operation] temporary sets a default
          printer to a printer with a specified name and version. The default
          printer is restored after [operation] is finished.  *)
      val with_printer : ?ver:string -> string -> (unit -> 'a) -> 'a


      (** {2 Low level access to serializers}  *)

      (** [find_reader ?ver name] lookups a reader with a given name. If
          version is not specified, then a reader with maximum version is
          returned.  *)
      val find_reader : ?ver:string -> string -> t reader option

      (** [find_writer ?ver name] lookups a writer with a given name. If
          version is not specified, then a writer with maximum version is
          returned.  *)
      val find_writer : ?ver:string -> string -> t writer option

    end

    (** Implements [Data.S] interface from the provided minimal implementation.  *)
    module Make (T : Versioned.S) : S with type t := T.t

    (** [Read] typeclass.

        A class of readable values. It is rarely needed to use this
        module directly, as it is used for implementing new readers,
        i.e., new protocols. If you are looking for the IO functions
        available for a type, then look at the {!Data.S} interface.
    *)
    module Read : sig

      (** readable  *)
      type 'a t = 'a reader

      (** A minimal complete definition is any method except
          [of_channel] (that is not sufficient).

          If a class is defined only with [from_bigstring] or [from_bytes]
          then [from_channel] function will consume all input and pass it
          to the correspondings function.  *)
      val create :
        ?of_channel     : (in_channel -> 'a) ->
        ?of_lexbuf      : (lexbuf -> 'a) ->
        ?of_scanbuf     : (scanbuf -> 'a) ->
        ?of_bigstring   : (bigstring -> 'a) ->
        ?of_bytes       : (bytes -> 'a) ->
        unit -> 'a t

      (** [of_bytes readable bytes] reads a value from [bytes] *)
      val of_bytes : 'a t -> bytes -> 'a

      (** [of_channel readable channel] inputs a value from a
          channel. If [of_channel] function wasn't provided when
          [readable] was created, then the whole content of the
          [in_channel] is read into an intermediate buffer, that is later
          read. Any leftover bytes are discarded. *)
      val of_channel : 'a t -> in_channel -> 'a

      (** [of_bigstring readable buf] deserializes a [readable] value
          from the bigstring.  *)
      val of_bigstring : 'a t -> bigstring -> 'a
    end


    (** Write typeclass.

        Defines a class of writeable (serializable) values. See module
        {!Read} for more information concerning when and why to use
        this module. *)
    module Write : sig

      (** a writer type class  *)
      type 'a t = 'a writer

      (** [create <minimal-implementation>] creates a writable
          instance from a provided minimal implementation.

          The minimal implementation is either of:
           - [to_bytes];
           - [to_bigstring];
           - [pp].
      *)
      val create :
        ?to_bytes  : ('a -> bytes) ->
        ?to_bigstring : ('a -> bigstring) ->
        ?dump  : (out_channel -> 'a -> unit) ->
        ?pp    : (Format.formatter -> 'a -> unit) ->
        ?size  : ('a -> int) ->
        ?blit_to_string:('a,string) copy ->
        ?blit_to_bigstring:('a,bigstring) copy ->
        unit -> 'a t


      (** [size writebale value] returns a size in bytes that
          [writeable] will use to write [value] *)
      val size : 'a t -> 'a -> int

      (** [to_channel writeable out value] writes the [writable]
          [value] into the channel [chan] *)
      val to_channel : 'a t -> out_channel -> 'a -> unit

      (** [to_formatter writeable ppf value] outputs the value
          into the formatter [ppf] *)
      val to_formatter : 'a t -> Format.formatter -> 'a -> unit

      (** [to_bytes writeable value] serializes the [value] into
          [bytes] data type.  *)
      val to_bytes : 'a t -> 'a -> bytes

      (** [to_bigstring writeable value] serializes the [value] into
          [bigstring] data type. *)
      val to_bigstring : 'a t -> 'a -> bigstring

      (** [blit_to_string writeable buf value pos] copies a serialized
          representation of the [value] into existing string [buf],
          starting from the given position. It is undefined behavior,
          if the [value] doesn't fit into the string [buf] *)
      val blit_to_string : 'a t -> string -> 'a -> int -> unit


      (** [blit_to_bigstring writeable buf value pos] copies a
          serialized representation of the [value] into existing
          bigstring [buf], starting from the given position. It is
          undefined behavior, if the [value] doesn't fit into the
          string [buf] *)
      val blit_to_bigstring : 'a t -> bigstring -> 'a -> int -> unit
    end

    (** Generic caching.

        Use [T.Cache] module if you want to cache values of type
        [T.t].

        Use {!Data.Cache.digest} to build digests of the data for
        caching.
    *)
    module Cache : sig

      (** cacher type class  *)
      type 'a t

      (** [create ~load ~save] creates a cache provider.  *)
      val create :
        load:(digest -> 'a option) ->
        save:(digest -> 'a -> unit) -> 'a t

      (** [digest ~namespace fmt x y z ...] a variadic function to
          create data digests. Use it like a printf, e.g.,
          {[
            type t = {name : string; parent : string; lang : string}

            let digest t = Data.Cache.digest ~namespace:"student" "%s%s"
                t.name t.parent
          ]}

          In the example, we created a digest that will ignore [lang]
          field of the data type (that is assumed to be transparent to
          the cached computation).

          Note: [digest] function will first eagerly build the whole
          string and then convert it to the digest. So it has O(N)
          complexity in space and time, where N is the total size of
          all constituting elements. If N is too big (hundreds of
          megabytes) then use [Digest] module for building digests
          incrementally.*)
      val digest : namespace:string ->
        ('a,Format.formatter,unit,digest) format4 -> 'a


      (** Data digesting for caching.*)
      module Digest : sig
        include Identifiable with type t = digest

        (** [create ~namespace] create a digest initialized with the
            given [namespace]. Since a caching service is using a flat
            namespace of keys, the [namespace] parameter is used to
            distinguish data build by different functions that have
            the same parameters digests. A module name is a good
            candidate for the namespace. *)
        val create : namespace:string -> t

        (** [add digest fmt x y z ...] is a variadic function that
            builds a string from arguments [x y z ...] using specified
            format [fmt] and adds this string to the digest. *)
        val add : t -> ('a,Format.formatter,unit,t) format4 -> 'a

        (** [add_sexp d sexp_of x] is [add d "%a" Sexp.pp (sexp_of x)] *)
        val add_sexp : t -> ('a -> Sexp.t) -> 'a -> t
      end

      (** [load cls digest] loads entry with a given [digest] from the
          cache. Note, this is a generic function, if you want to load
          a value of type [T.t] use [T.Cache.load] function.*)
      val load : 'a t -> digest -> 'a option

      (** [save cls digest x] stores entry [x] with a given [digest]
          to the cache. Note, this is a generic function, if you want
          to store a value of type [T.t] use [T.Cache.save] function.*)
      val save : 'a t -> digest -> 'a -> unit

      (** service signature  *)
      type service = {
        create : 'a . 'a reader -> 'a writer -> 'a t
      }

      (** Service injection point.

          By default the library doesn't provide any caching
          services. All data stored in the cache is forgotten.

          The caching service is provided by plugins, who register
          itself using [Service.provide] function. Only one caching
          service can be active at given moment.

          Note: this interface is for implementing caching for data
          type. If you're using data type whose module [T] implements
          interface {!Data}, then you can just use [T.Data.Cache]
          module. If you need to add caching to your custom data type,
          then use {!Regular}, {!Opaque} or {!Data.Make} functors (in
          the order of preference).*)
      module Service : sig

        (** [provide service] will substitute current caching service
            with a new [service]. *)
        val provide : service -> unit

        (** [request reader writer] returns a caching service for
            type ['a].    *)
        val request : 'a reader -> 'a writer -> 'a t
      end

    end
  end

  (** Regular types models a general concept of value, i.e., something
      that can be used in way similar to regular [int], [string],
      [char] and other built in types. So that it can be compared, used
      in maps, sets, hashtables, printer, etc.

      Note: this signature is pretty similar to core's [Identifiable],
      but doesn't require [of_string] function, that is usually much
      harder to implement in comparison with [to_string] function. Also,
      instead of [to_string] it requires [pp] function that can be
      implemented much more efficiently and elegantly. From the [pp]
      function the whole plethora of printing functions are derived:
      [str], [pps], [ppo].

      @see
      <https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/Std/Comparable.mod/S_binable.modt/>
      [Comparable.S_binable]

      @see
      <https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/Hashable/S_binable.modt/>
      [Hashable.S_binable]


*)
  module Regular : sig

    (** Regular interface.  *)
    module type S = sig
      type t [@@deriving bin_io, sexp, compare]
      include Printable.S            with type t := t
      include Comparable.S_binable   with type t := t
      include Hashable.S_binable     with type t := t
      include Data.S                 with type t := t
    end

    (** In order to implement [Regular] interface you need to provide a
        minimum implementation [M]  *)
    module Make( M : sig
        (** type t should be binable, sexpable and provide compare function  *)
        type t [@@deriving bin_io, sexp, compare]
        include Pretty_printer.S with type t := t
        include Data.Versioned.S with type t := t
        val hash : t -> int
        val module_name : string option
      end) : S with type t := M.t
  end


  (** Opaque type is like regular type, except that we can print or
      examine it in any way. So it can't be serialized or
      pretty-printed. An {!Opaque.Make} can create an instances of
      such type.  *)
  module Opaque : sig


  (** Opaque type is like regular type, except that we can print or
      examine it in any way. So it can't be serialized or
      pretty-printed.

      @see
      <https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/Std/Comparable.mod/S.modt/>
      [Comparable.S]

      @see
      <https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/Std/Hashable.mod/S.modt/>
      [Hashable.S]
  *)
    module type S = sig
      type t
      include Comparable.S with type t := t
      include Hashable.S   with type t := t
    end

    (** creates a module implementing [Opaque] interface.   *)
    module Make(M : sig
        type t [@@deriving compare]
        val hash : t -> int
      end) : S with type t := M.t
  end


  (** Extension of the standard bytes module.

      We enhanced OCaml standard [Bytes] module with the expected set
      of functions. [Bytes] are not [Regular], as [Regular] interface
      itself refers to bytes. However it includes a pretty big subset
      of regular (excluding {!Data} module, but bytes indeed do not need
      any specific support for the serialization).*)
  module Bytes : sig


    (** bytes  *)
    type t = Bytes.t [@@deriving bin_io, compare, sexp]

    include Container.S0   with type t := t with type elt := char
    include Blit.S         with type t := t
    include Identifiable.S with type t := t
    module To_string   : Blit.S_distinct with type src := t with type dst := string
    module From_string : Blit.S_distinct with type src := string with type dst := t



    (** [create n] returns a new byte sequence of length [n]. The
        sequence is uninitialized and contains arbitrary bytes.
        Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)
    external create : int -> t = "caml_create_string"

    (** [make n c] returns a new byte sequence of length [n], filled with
        the byte [c].
        Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)
    val make : int -> char -> t

    (** [init n ~f] returns a fresh byte sequence of length [n], with
        character [i] initialized to the result of [f i] (in increasing
        index order).
        Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)
    val init : int -> f:(int -> char) -> t

    (** [empty] a byte sequence of size 0. *)
    val empty : t

    (** [length t] returns the length (number of bytes) of [t]. *)
    external length: t -> int = "%string_length"

    (** [get s n] returns the byte at index [n] in [s].
        Raise [Invalid_argument] if [n] not a valid index in [s]. *)
    external get : t -> int -> char = "%string_safe_get"

    (** [set s n c] modifies [s] in place, replacing the byte at index [n]
        with [c].
        Raise [Invalid_argument] if [n] is not a valid index in [s]. *)
    external set : t -> int -> char -> unit = "%string_safe_set"

    (** [copy t] returns a new byte sequence that contains the same
        bytes as [t]. *)
    val copy : t -> t

    (** [of_string s] returns a new byte sequence that contains the
        same bytes as the given string. *)
    val of_string : string -> t

    (** [to_string t] returns a new string that contains the same
        bytes as the given byte sequence. *)
    val to_string : t -> string

    (** [extend s left right] returns a new byte sequence that contains
        the bytes of [s], with [left] uninitialized bytes prepended and
        [right] uninitialized bytes appended to it. If [left] or [right]
        is negative, then bytes are removed (instead of appended) from
        the corresponding side of [s].
        Raise [Invalid_argument] if the result length is negative or
        longer than {!Sys.max_string_length} bytes. *)
    val extend : t -> int -> int -> t

    (** [fill s start len c] modifies [s] in place, replacing [len]
        characters with [c], starting at [start].
        Raise [Invalid_argument] if [start] and [len] do not designate a
        valid range of [s]. *)
    val fill : t -> int -> int -> char -> unit

    (** [concat sep sl] concatenates the list of byte sequences [sl],
        inserting the separator byte sequence [sep] between each, and
        returns the result as a new byte sequence.
        Raise [Invalid_argument] if the result is longer than
        {!Sys.max_string_length} bytes. *)
    val concat : t -> t list -> t

    (** [cat s1 s2] concatenates [s1] and [s2] and returns the result
         as new byte sequence.
        Raise [Invalid_argument] if the result is longer than
        {!Sys.max_string_length} bytes. *)
    val cat : t -> t -> t

    (** [iteri t ~f] same as {!iter}, but the function is
        applied to the index of the byte as first argument and the
        byte itself as second argument. *)
    val iteri : t -> f:(int -> char -> unit) -> unit

    (** [map s ~f] applies function [f] in turn to all the bytes of [s]
        (in increasing index order) and stores the resulting bytes in
        a new sequence that is returned as the result. *)
    val map : t -> f:(char -> char) -> t

    (** [mapi s ~f] calls [f] with each character of [s] and its
        index (in increasing index order) and stores the resulting bytes
        in a new sequence that is returned as the result. *)
    val mapi : t -> f:(int -> char -> char) -> t

    (** [trim t] returns a copy of [t], without leading and trailing
        whitespace. The bytes regarded as whitespace are the ASCII
        characters [' '], ['\012'], ['\n'], ['\r'], and ['\t']. *)
    val trim : t -> t

    (** [escaped t] returns a copy of [t], with special characters
        represented by escape sequences, following the lexical
        conventions of OCaml.
        Raise [Invalid_argument] if the result is longer than
        {!Sys.max_string_length} bytes. *)
    val escaped : t -> t

    (** [index s c] returns the index of the first occurrence of byte [c]
        in [s].
        Raise [Not_found] if [c] does not occur in [s]. *)
    val index : t -> char -> int

    (** [rindex s c] returns the index of the last occurrence of byte [c]
        in [s].
        Raise [Not_found] if [c] does not occur in [s]. *)
    val rindex : t -> char -> int

    (** [index_from s i c] returns the index of the first occurrence of
        byte [c] in [s] after position [i].  [index s c] is
        equivalent to [index_from s 0 c].
        Raise [Invalid_argument] if [i] is not a valid position in [s].
        Raise [Not_found] if [c] does not occur in [s] after position [i]. *)
    val index_from : t -> int -> char -> int

    (** [rindex_from s i c] returns the index of the last occurrence of
        byte [c] in [s] before position [i+1].  [rindex s c] is equivalent
        to [rindex_from s (length s - 1) c].
        Raise [Invalid_argument] if [i+1] is not a valid position in [s].
        Raise [Not_found] if [c] does not occur in [s] before position [i+1]. *)
    val rindex_from : t -> int -> char -> int

    (** [contains s c] tests if byte [c] appears in [s]. *)
    val contains : t -> char -> bool

    (** [contains_from s start c] tests if byte [c] appears in [s] after
        position [start].  [contains s c] is equivalent to [contains_from
        s 0 c].
        Raise [Invalid_argument] if [start] is not a valid position in [s]. *)
    val contains_from : t -> int -> char -> bool

    (** [rcontains_from s stop c] tests if byte [c] appears in [s] before
        position [stop+1].
        Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
        position in [s]. *)
    val rcontains_from : t -> int -> char -> bool

    (** [uppercase t] returns a copy of [t], with all lowercase letters
        translated to uppercase, including accented letters of the ISO
        Latin-1 (8859-1) character set. *)
    val uppercase : t -> t

    (** [lowercase t] returns a copy of [t], with all uppercase letters
        translated to lowercase, including accented letters of the ISO
        Latin-1 (8859-1) character set. *)
    val lowercase : t -> t

    (** [capitalize t] returns a copy of [t], with the first byte set
        to uppercase. *)
    val capitalize : t -> t

    (** [uncapitalize t] returns a copy of [t], with the first byte set
        to lowercase. *)
    val uncapitalize : t -> t

    (** {4 Unsafe conversions (for advanced users)}

        This section describes unsafe, low-level conversion functions
        between [bytes] and [string]. They do not copy the internal data;
        used improperly, they can break the immutability invariant on
        strings provided by the [-safe-string] option. They are available for
        expert library authors, but for most purposes you should use the
        always-correct {!Bytes.to_string} and {!Bytes.of_string} instead. *)
    module Unsafe : sig

      (** [to_string b] - unsafely converts a byte sequence into a string.

          To reason about the use of [to_string], it is convenient to
          consider an "ownership" discipline. A piece of code that
          manipulates some data "owns" it; there are several disjoint ownership
          modes, including:
          - Unique ownership: the data may be accessed and mutated
          - Shared ownership: the data has several owners, that may only
            access it, not mutate it.

          Unique ownership is linear: passing the data to another piece of
          code means giving up ownership (we cannot write the
          data again). A unique owner may decide to make the data shared
          (giving up mutation rights on it), but shared data may not become
          uniquely-owned again.

          [to_string s] can only be used when the caller owns the byte
          sequence [s] -- either uniquely or as shared immutable data. The
          caller gives up ownership of [s], and gains ownership of the
          returned string.

          There are two valid use-cases that respect this ownership
          discipline:

          1. Creating a string by initializing and mutating a byte sequence
          that is never changed after initialization is performed.

          {[
            let string_init len f : string =
              let s = Bytes.create len in
              for i = 0 to len - 1 do Bytes.set s i (f i) done;
              Bytes.Unsafe.to_string s
          ]}

          This function is safe because the byte sequence [s] will never be
          accessed or mutated after [to_string] is called. The
          [string_init] code gives up ownership of [s], and returns the
          ownership of the resulting string to its caller.

          Note that it would be unsafe if [s] was passed as an additional
          parameter to the function [f] as it could escape this way and be
          mutated in the future -- [string_init] would give up ownership of
          [s] to pass it to [f], and could not call [to_string]
          safely.

          We have provided the {!String.init}, {!String.map} and
          {!String.mapi} functions to cover most cases of building
          new strings. You should prefer those over [to_string] or
          [to_string] whenever applicable.

          2. Temporarily giving ownership of a byte sequence to a function
          that expects a uniquely owned string and returns ownership back, so
          that we can mutate the sequence again after the call ended.

          {[
            let bytes_length (s : bytes) =
              String.length (Bytes.Unsafe.to_string s)
          ]}

          In this use-case, we do not promise that [s] will never be mutated
          after the call to [bytes_length s]. The {!String.length} function
          temporarily borrows unique ownership of the byte sequence
          (and sees it as a [string]), but returns this ownership back to
          the caller, which may assume that [s] is still a valid byte
          sequence after the call. Note that this is only correct because we
          know that {!String.length} does not capture its argument -- it could
          escape by a side-channel such as a memoization combinator.

          The caller may not mutate [s] while the string is borrowed (it has
          temporarily given up ownership). This affects concurrent programs,
          but also higher-order functions: if [String.length] returned
          a closure to be called later, [s] should not be mutated until this
          closure is fully applied and returns ownership. *)
      val to_string : t -> string

      (** [of_string s] - unsafely converts a shared string to a byte
          sequence that should not be mutated.

          The same ownership discipline that makes [to_string]
          correct applies to [of_string]: you may use it if you were
          the owner of the [string] value, and you will own the return
          [bytes] in the same mode.

          In practice, unique ownership of string values is extremely
          difficult to reason about correctly. You should always assume
          strings are shared, never uniquely owned.

          For example, string literals are implicitly shared by the
          compiler, so you never uniquely own them.

          {[
            let incorrect = Bytes.Unsafe.of_string "hello"
            let s = Bytes.of_string "hello"
          ]}

          The first declaration is incorrect, because the string literal
          ["hello"] could be shared by the compiler with other parts of the
          program, and mutating [incorrect] is a bug. You must always use
          the second version, which performs a copy and is thus correct.

          Assuming unique ownership of strings that are not string
          literals, but are (partly) built from string literals, is also
          incorrect. For example, mutating [of_string ("foo" ^ s)]
          could mutate the shared string ["foo"] -- assuming a rope-like
          representation of strings. More generally, functions operating on
          strings will assume shared ownership, they do not preserve unique
          ownership. It is thus incorrect to assume unique ownership of the
          result of [of_string].

          The only case we have reasonable confidence is safe is if the
          produced [bytes] is shared -- used as an immutable byte
          sequence. This is possibly useful for incremental migration of
          low-level programs that manipulate immutable sequences of bytes
          (for example {!Marshal.from_bytes}) and previously used the
          [string] type for this purpose. *)
      val of_string : string -> t

      (**/**)
      (** The following is for system use only. Do not call directly. *)
      external get  : t -> int -> char = "%string_unsafe_get"
      external set  : t -> int -> char -> unit = "%string_unsafe_set"

      [@@@ocaml.warning "-3"]

      external blit : t -> int -> t -> int -> int -> unit = "caml_blit_string" "noalloc"
      external fill : t -> int -> int -> char -> unit = "caml_fill_string" "noalloc"

      (**/**)
    end
  end
end
