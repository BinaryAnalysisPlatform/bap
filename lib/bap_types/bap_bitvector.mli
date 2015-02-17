(** Bitvector -- a type for representing binary values.

    {2 Overview }

    A numeric value with a 2-complement binary representation. It is
    good for representing addresses, offsets and other numeric values.

    Each value is attributed by a its bit-width. All arithmetic
    operations over values are done modulo their widths. It is an
    error to apply arithmetic operation to values with different
    widths. Default implementations will raise a [Width] exception,
    however there exists a family of modules that provide arithmetic
    operations lifted to an [Or_error.t] monad. It is suggested to use
    them, if you know what kind of operands you're expecting.


    {3 Clarifications endianness and bit-ordering }

    Bitvector should be considered as an number with an arbitrary
    width. That means, that as with all numbers it is subject to
    endianness. When we iterate over bitvector using some container
    interface we always start from the byte with the lower
    address. Depending on endianness it will be either least
    significant bytes (little-endian), or most significant
    (big-endian). Sometimes id does matter, sometimes it doesn't. In a
    latter case you can just use a default native-endian
    interface. But in a former case, please consider using explicit
    modules, either [Bytes_LE] or [Bytes_BE], even if you know that
    your system is [LE]. Things change.

    Bits are always numbered from right to left, with least
    significant bit having a zero index, and most significant having
    index equal to [width - 1]. That means, they're endianness
    agnostic.

    {3 Clarification on size-morphism }

    Size-monomorphic operations (as opposed to size-polymorphic
    comparison) doesn't allow to compare two operands with different
    sizes, and either raise exception or return [Error]. If we would
    have type safe interface, with type [t] defined as [type 'a t],
    where ['a] stands for size, then size-monomorphic operations will
    have type ['a t -> 'a t -> _], and size-polymorphic ['a t -> 'b t -> _].

    By default, size-polymorphic comparison is used (for rationale of
    this decision look at the implementation of a hash function). To
    understand the ordering relation one can think that a lexical
    ordering is specified on a tuple [(x,n)], where [x] is the number
    and [n] is the size. For example, the following sequence is in an
    ascending order:

    {[ 0x0:1, 0x0:32, 0x0:64, 0x1:1, 0x1:32, 0xD:4, 0xDEADBEEF:32]}.

    A size-monomorphic interfaced is exposed in a [Mono] submodule. So
    if you want a monomorphic map, then just use [Mono.Map] module.
    Note, [Mono] submodule doesn't provide [Table], since we cannot
    guarantee that all keys in a hash-table have equal size.

    {3 Clarification on signs}

    By default all numbers represented by a bitvector are considered
    unsigned. This includes comparisons, e.g., [of_int (-1) ~width:32]
    is greater than zero. If you need to perform signed operation, you
    can use [signed] operator to temporary cast your value to signed.
    We use temporary to emphasize that, the signedness property won't
    propagate to the result of the operation, e.g. result of the
    following expression: [Int_exn.(signed x / y)] will not be signed.

    If any operand of a binary operation is signed, then a signed
    version of an operation is used.

    Remember to use explicit casts, whenever you really need a signed
    representation. Examples:
    {[
      let x = of_int ~-6 ~width:8
      let y = to_int x          (* y = 250 *)
      let z = to_int (signed x) (* z = ~-6 *)
      let zero = of_int 0 ~width:8
      let p = x < zero          (* p = false *)
      let q = signed x < zero   (* p = true *)
    ]}

    {3 Clarification on string representation }

    As a part of [Identifiable] interface bitvector provides a pair of
    complement functions: [to_string] and [of_string], that provides
    facilities to store bitvector as a human readable string, and to
    restore it from string. The format of the representation is the
    following (in EBNF):
    {[
      repr  = [sign], base, digit, {digit}, ":", size | true | false;
      sign  = "+" | "-";
      base  = "0x" | "0b" | "0o";
      size  = dec, {dec};
      digit = dec | oct | hex;
      dec   = ?decimal digit?;
      oct   = ?octal digit?;
      hex   = ?hexadecimal digit?;
    ]}

    Examples: {[
      0x5D:32, 0b0101:16, 5:64, +5:8, +0x5D:16, true, false.
    ]}.

    Form [false] is a shortcut for [0:1], as well as [true] is
    [1:1].

    If [base] is omitted base-10 is assumed. The output format is
    always ["0x", hex, {hex}] in an unsigned form.

*)
open Core_kernel.Std

(** {2 Common Interfaces}

    Bitvector is a value, first of all, so it supports a common set of
    a value interface: it can be stored, compared, it can be a key in
    a dictionary, etc. Moreover, being a number it can be compared
    with zero and applied to a common set of integer operations.
*)
type t

(** [Width] exception is raised when size-monomorphic operation is
    applied to operands with different sizes. *)
exception Width with sexp

(** Specifies the order of bytes in a word. *)
type endian =
  | LittleEndian (** least significant byte comes first  *)
  | BigEndian    (** most  significant byte comes first  *)
with bin_io, compare, sexp

include Bap_regular.S with type t := t
include Comparable.With_zero with type t := t
include Bap_integer.S with type t := t
(** {2 Container interfaces}

    Bitvector is also a container for bytes and bits. You can access
    individual bytes using a [Container] interface.
*)

(** A comparable interface with size-monomorphic comparison. *)
module Mono : Comparable with type t := t

(** {2 Constructors}

    We provide several conversions from primitive data types. Also, do
    not forget about [of_string] function, exposed via [Identifiable]
    interface.
*)
val of_string : string -> t
val of_bool  : bool -> t
val of_int   : width:int -> int -> t
val of_int32 : ?width:int -> int32 -> t
val of_int64 : ?width:int -> int64 -> t

(** {3 Some predefined constant constructors }  *)


(** [b0 = of_bool false] - a zero bit  *)
val b0 : t
(** [b1 = of_bool true] - a one bit  *)
val b1 : t

(** {3 Helpful shortcuts }  *)

(** [one width] number one with a specified [width], is a shortcut for
    [of_int 1 ~width]*)
val one: int -> t
(** [zero width] zero with a specified [width], is a shortcut for
    [of_int 0 ~width]*)
val zero: int -> t

(** [ones width] is a number with a specified [width], and all bits
    set to 1. It is a shortcut for [of_int (lnot 0) ~width]*)
val ones : int -> t

(** [of_binary ?width endian num] creates a bitvector from a string
    interpreted as a sequence of bytes in a specified order.

    The result is always positive.

    [num] argument is copied

    [width] defaults to [String.length num]
*)
val of_binary : ?width:int -> endian -> string -> t

(** {2 Conversions to integers }  *)

(** {3 Signed conversions }  *)
val to_int   : t -> int   Or_error.t
val to_int32 : t -> int32 Or_error.t
val to_int64 : t -> int64 Or_error.t
val string_of_value : ?hex:bool -> t -> string

(** [signed t] casts t to a signed type, so that any operations
    applied on [t] will be signed *)
val signed : t -> t

(** [is_zero bv] is true iff all bits are set to zero. *)
val is_zero : t -> bool

(** [is_ones bv] is true if the least significant bit is equal to one  *)
val is_one : t -> bool

(** [bitwidth bv] return a bit-width, i.e., the amount of bits *)
val bitwidth : t -> int

(** [extract bv ~hi ~lo] extracts a subvector from [bv], starting
    from bit [hi] and ending with [lo]. Bits are enumerated from
    right to left (from least significant to most), starting from
    zero. [hi] maybe greater then [size].

    [hi] defaults to [width bv - 1]
    [lo] defaults to [0].

    Example:

    [extract (of_int 17 ~width:8) ~hi:4 ~lo:3]
    will result in a two bit vector consisting of the forth and
    third bits, i.e., equal to a number [2].

    [lo] and [hi] should be non-negative numbers. [lo] must be less
    then a [width bv] and [hi >= lo]. *)
val extract : ?hi:int -> ?lo:int -> t -> t Or_error.t

(** [extract_exn bv ~hi ~lo] is the same as [extract], but will raise
    an exception on error.  *)
val extract_exn : ?hi:int -> ?lo:int -> t -> t

(** [concat b1 b2] concatenates two bitvectors  *)
val concat : t -> t -> t

(** [b1 @. b2] is equal to [concat b1 b2] *)
val (@.): t -> t -> t

(** [succ n] returns next value after [n]. Of course it is not
    guaranteed that [succ n > n]*)
val succ : t -> t

(** [pred n] returns a value preceding [n]  *)
val pred : t -> t

(** [nsucc m n] is [Fn.apply_n_times ~n succ m], but more
    efficient.  *)
val nsucc : t -> int -> t

(** [npred m n] is [Fn.apply_n_times ~n pred addr], but more
    efficient.  *)
val npred : t -> int -> t

(** [a ++ n] is [nsucc a n]  *)
val (++) : t -> int -> t

(** [a -- n] is [npred a n]  *)
val (--) : t -> int -> t

(** {2 Iteration over bitvector components }  *)

(** [to_bytes x order] returns bytes of [x] in a specified [order].
    Each byte is represented as a [bitvector] itself. *)
val to_bytes : t -> endian ->    t Sequence.t
(** [to_bytes x order] returns bytes of [x] in a specified [order],
    with bytes represented by [char] type *)
val to_chars : t -> endian -> char Sequence.t

(** [to_bits x order] returns bits of [x] in a specified [order].
    [order] defines only the ordering of words in a bitvector, bits
    will always be in MSB first order. *)
val to_bits  : t -> endian -> bool Sequence.t

(** A type safe integer interface.

    All binary integer operations are only well defined on operands
    with equal sizes.

    Module [Int] provides a set of integer operations that do not
    raise exceptions, but return values raised to an Or_error
    monad.

    Example:

       [Z.(i16 v1 + i16 v2 / int 16 v3)],

    or just:

       [Z.(!$v1 + !$v2 / !$v3)].
*)

module Int_err : sig
  (** [!$v] lifts [v] to an Or_error monad. It is, essentially, the
      same as [Ok v] *)
  val (!$): t -> t Or_error.t

  (** The following lifter will check that their operand has a
      corresponding width. *)
  val i1 :  t -> t Or_error.t
  val i4 :  t -> t Or_error.t
  val i8 :  t -> t Or_error.t
  val i16 : t -> t Or_error.t
  val i32 : t -> t Or_error.t
  val i64 : t -> t Or_error.t

  (** [int w v] will be [Ok] if [v] has width [w] *)
  val int : int -> t -> t Or_error.t

  (** [of_word_size w] creates a lifter for a specified word size
      [w], i.e. either [i64] or [i32]  *)
  val of_word_size : Word_size.t -> t -> t Or_error.t

  include Bap_integer.S with type t = t Or_error.t
  include Monad.Infix with type 'a t := 'a Or_error.t
end

(** This module exposes a common integer interface with
    operations not lifted into [Or_error] monad, but raising
    [Width] exception if operands sizes mismatch.
*)
module Int_exn : Bap_integer.S with type t = t

(** Prefix trees for bitvectors.

    Bitvector comes with 4 predefined prefix trees:

    - [Trie.Big.Bits] - big endian prefix tree, where each
    token is a bit, and bitvector is tokenized from msb to lsb.

    - [Trie.Big.Byte] - big endian prefix tree, where each token
    is a byte, and bitvector is tokenized from most significant
    byte to less significant

    - [Trie.Little.Bits] - is a little endian bit tree.

    - [Trie.Little.Byte] - is a little endian byte tree.
*)
module Trie : sig
  module Big : sig
    module Bits : Bap_trie.S  with type key = t
    module Bytes : Bap_trie.S with type key = t
  end
  module Little : sig
    module Bits : Bap_trie.S  with type key = t
    module Bytes : Bap_trie.S with type key = t
  end
end
