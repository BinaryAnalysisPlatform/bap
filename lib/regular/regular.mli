open Core_kernel.Std

module Std : sig
  module Bytes : sig

    type t = Bytes.t with bin_io, compare, sexp

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

      (** The following is for system use only. Do not call directly. *)
      external get  : t -> int -> char = "%string_unsafe_get"
      external set  : t -> int -> char -> unit = "%string_unsafe_set"
      external blit : t -> int -> t -> int -> int -> unit = "caml_blit_string" "noalloc"
      external fill : t -> int -> int -> char -> unit = "caml_fill_string" "noalloc"

    end
  end

  type bytes = Bytes.t

  type 'a reader
  type 'a writer

  (** ['a printer] defines a type for pretty-printers for a value of
      type ['a]. This is the type, that is required by [%a] specifier,
      for [Format.printf]-family of functions. Also, this is the type,
      that can be installed into OCaml toplevel or debugger.

      Note: `bap.top` library automatically installs all printers. *)
  type 'a printer = Format.formatter -> 'a -> unit

  (** Data type interface.

      Types that implements this interface have a concrete
      representation, that can be serialized and deserialized.

      The only requirement is to provide a [version] value, that
      represents a version number of the representation. Every time
      the representation changes, the version should be updated.  *)
  module type Data = sig
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
        semantic versioning.
    *)
    val version : string

    (** [size_in_bytes ?ver ?fmt datum] returns the amount of bytes
        that is needed to represent [datum] in the a given format and
        version *)
    val size_in_bytes : ?ver:string -> ?fmt:string -> t -> int

    (** [of_bytes ?ver ?fmt bytes] deserializes a datum from bytes  *)
    val of_bytes : ?ver:string -> ?fmt:string -> bytes -> t

    (** [to_bytes ?ver ?fmt datum] serializes a datum to a sequence of
        bytes *)
    val to_bytes : ?ver:string -> ?fmt:string -> t -> bytes

    (** [blit_to_bytes ?ver ?fmt buffer datum offset] copies a
        serialized representation of datum into a [buffer], starting from
        [offset].  *)
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

    (** Input/Output functions for the a given datum.*)
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

      (** [scan ?ver ?fmt channel] creates a stream of datums, that
          are loaded consequently from the channel *)
      val scan  : ?ver:string -> ?fmt:string -> in_channel -> (unit -> t option)

      (** [write ?ver ?fmt file datum] writes [datum] to a [file] *)
      val write  : ?ver:string -> ?fmt:string -> string -> t -> unit

      (** [save ?ver ?fmt channel datum] saves [datum] to a [channel] *)
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
        disappear with time, but can survive for a long time. The
        library by itself doesn't provide a caching service for any
        type, only the interface. The caching service can be added
        externally (e.g., via plugin system).

        Note: it is only reasonable to use caching for data types,
        that take reasonable amount of time to create.
    *)
    module Cache : sig

      (** [load id] load data previously stored under give [id]  *)
      val load : string -> t option

      (** [save id data] store data under given [id]. If something is
          already stored, then it will be overwritten.*)
      val save : string -> t -> unit
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

    (** [default_writer] returns information about default writer  *)
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


  (** Printable interface is implemented by a significant amount of
      BAP types.  *)
  module type Printable = sig
    type t

    (** [to_string x] returns a human-readable representation of [x]  *)
    val to_string : t -> string

    (** [str () t] is formatted output function that matches "%a"
        conversion format specifier in functions, that prints to string,
        e.g., [sprintf], [failwithf], [errorf] and, suprisingly all
        [Lwt] printing function, including [Lwt_io.printf] and logging
        (or any other function with type ('a,unit,string,...)
        formatN`. Example:

        [Or_error.errorf "type %a is not valid for %a"
          Type.str ty Exp.str exp] *)
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

  module Printable(M : sig
      include Pretty_printer.S
      val module_name : string option
    end) : Printable with type t := M.t


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
      [str], [pps], [ppo]. *)
  module type Regular = sig
    type t with bin_io, sexp, compare
    include Printable            with type t := t
    include Comparable.S_binable with type t := t
    include Hashable.S_binable   with type t := t
    include Data with type t := t
  end

  (** Opaque type is like regular type, except that we can print or
      examine it in any way. So it can't be serialized or
      pretty-printed. An {!Opaque.Make} can create an instances of
      such type.  *)
  module type Opaque = sig
    type t
    include Comparable with type t := t
    include Hashable   with type t := t
  end

  (** Lazy sequence  *)
  module Seq : sig
    type 'a t = 'a Sequence.t with bin_io, compare, sexp
    include module type of Sequence with type 'a t := 'a t

    (** for compatibility with Core <= 111.28  *)
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int

    val of_array : 'a array -> 'a t

    val cons : 'a -> 'a t -> 'a t

    val is_empty : 'a t -> bool

    val pp : 'a printer -> 'a t printer
    val pp_bools : bool t printer
    val pp_chars : char t printer
    val pp_floats : float t printer
    val pp_ints : int t printer
    val pp_strings : string t printer

  end

  (** type abbreviation for ['a Sequence.t]  *)
  type 'a seq = 'a Seq.t with bin_io, compare, sexp

  (** [x ^:: xs] is a consing operator for sequences  *)
  val (^::) : 'a -> 'a seq -> 'a seq


  module Data : sig

    (** [copy buf obj pos] is a method to copy object [obj] into a buffer
        [buf], starting from a position [pos] and will return the number
        of bytes written.  XXX: we need the amount of bytes beforehand.
    *)
    type ('a,'b) copy = 'b -> 'a -> int -> unit
    (** *)
    type ('a,'b) dump = 'b -> 'a -> unit

    type lexbuf  = Lexing.lexbuf
    type scanbuf = Scanf.Scanning.scanbuf


    module type Versioned = sig
      (** type of data  *)
      type t

      (** version of data representation  *)
      val version : string
    end


    module type Sexpable = sig
      include Versioned
      include Sexpable with type t := t
    end

    module type Binable = sig
      include Versioned
      include Binable with type t := t
    end


    val marshal_reader : (module T with type t = 'a) -> 'a reader
    val marshal_writer : (module T with type t = 'a) -> 'a writer
    val sexp_reader : (module Sexpable with type t = 'a) -> 'a reader
    val sexp_writer : (module Sexpable with type t = 'a) -> 'a writer
    val bin_reader : (module Binable with type t = 'a) -> 'a reader
    val bin_writer : (module Binable with type t = 'a) -> 'a writer
    val pretty_writer : (module Pretty_printer.S with type t = 'a) -> 'a writer



    module type S = Data

    module Make (T : Versioned) : S with type t := T.t

    module Read : sig
      type 'a t = 'a reader

      (** A minimal complete definition is any method except
          [from_channel].

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

      val of_bytes : 'a t -> bytes -> 'a
      val of_channel : 'a t -> in_channel -> 'a
      val of_bigstring : 'a t -> bigstring -> 'a
    end

    module Write : sig
      type 'a t = 'a writer

      val create :
        ?to_bytes  : ('a -> bytes) ->
        ?to_bigstring : ('a -> bigstring) ->
        ?dump  : (out_channel -> 'a -> unit) ->
        ?pp    : (Format.formatter -> 'a -> unit) ->
        ?size  : ('a -> int) ->
        ?blit_to_string:('a,string) copy ->
        ?blit_to_bigstring:('a,bigstring) copy ->
        unit -> 'a t

      val size : 'a t -> 'a -> int
      val to_channel : 'a t -> out_channel -> 'a -> unit
      val to_formatter : 'a t -> Format.formatter -> 'a -> unit
      val to_bytes : 'a t -> 'a -> bytes
      val to_bigstring : 'a t -> 'a -> bigstring
      val blit_to_string : 'a t -> string -> 'a -> int -> unit
      val blit_to_bigstring : 'a t -> bigstring -> 'a -> int -> unit
    end


    module Cache : sig
      type 'a t

      val create :
        load:(string -> 'a option) ->
        save:(string -> 'a -> unit) -> 'a t

      val load : 'a t -> string -> 'a option

      val save : 'a t -> string -> 'a -> unit

      type service = {
        create : 'a . 'a reader -> 'a writer -> 'a t
      }

      module Service : sig
        val provide : service -> unit
        val request : 'a reader -> 'a writer -> 'a t
      end
    end
  end

  (** In order to implement [Regular] interface you need to provide a
      minimum implementation [M]  *)
  module Regular : sig
    module Make( M : sig
        (** type t should be binable, sexpable and provide compare function  *)
        type t with bin_io, sexp, compare
        include Pretty_printer.S with type t := t
        include Data.Versioned with type t := t
        val hash : t -> int
        val module_name : string option
      end) : Regular with type t := M.t
  end

  (** creates a module implementing [Opaque] interface.   *)
  module Opaque : sig
    module Make(S : sig
        type t with compare
        val hash : t -> int
      end) : Opaque with type t := S.t
  end
end
