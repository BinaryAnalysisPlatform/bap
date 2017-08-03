open Core_kernel.Std
open Regular.Std

type t

type endian =
  | LittleEndian
  | BigEndian
  [@@deriving bin_io, compare, sexp]

include Regular.S with type t := t
include Bap_integer.S with type t := t
module Mono : Comparable.S with type t := t
val of_string : string -> t
val of_bool  : bool -> t
val of_int   : width:int -> int -> t
val of_int32 : ?width:int -> int32 -> t
val of_int64 : ?width:int -> int64 -> t
val b0 : t
val b1 : t
val one: int -> t
val zero: int -> t
val ones : int -> t
val of_binary : ?width:int -> endian -> string -> t
val to_int   : t -> int   Or_error.t
val to_int32 : t -> int32 Or_error.t
val to_int64 : t -> int64 Or_error.t
val string_of_value : ?hex:bool -> t -> string
val signed : t -> t
val unsigned : t -> t
val is_zero : t -> bool
val is_one : t -> bool
val bitwidth : t -> int
val extract : ?hi:int -> ?lo:int -> t -> t Or_error.t
val extract_exn : ?hi:int -> ?lo:int -> t -> t
val concat : t -> t -> t
val (@.): t -> t -> t
val succ : t -> t
val pred : t -> t
val nsucc : t -> int -> t
val npred : t -> int -> t
val (++) : t -> int -> t
val (--) : t -> int -> t
val enum_bytes : t -> endian ->    t Sequence.t
val enum_chars : t -> endian -> char Sequence.t
val enum_bits  : t -> endian -> bool Sequence.t
val validate_positive     : t Validate.check
val validate_non_negative : t Validate.check
val validate_negative     : t Validate.check
val validate_non_positive : t Validate.check
val is_positive     : t -> bool
val is_non_negative : t -> bool
val is_negative     : t -> bool
val is_non_positive : t -> bool

module Int_err : sig
  val (!$): t -> t Or_error.t
  val i1 :  t -> t Or_error.t
  val i4 :  t -> t Or_error.t
  val i8 :  t -> t Or_error.t
  val i16 : t -> t Or_error.t
  val i32 : t -> t Or_error.t
  val i64 : t -> t Or_error.t
  val int : int -> t -> t Or_error.t
  val of_word_size : Word_size.t -> t -> t Or_error.t
  include Bap_integer.S with type t = t Or_error.t
  include Monad.Infix with type 'a t := 'a Or_error.t
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, compare, sexp]
  end
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, compare, sexp]
  end
end

module Unsafe  : Bap_integer.S with type t = t
module Int_exn : Bap_integer.S with type t = t
module Trie : sig
  module Big : sig
    module Bits : Bap_trie_intf.S  with type key = t
    module Bytes : Bap_trie_intf.S with type key = t
  end
  module Little : sig
    module Bits : Bap_trie_intf.S  with type key = t
    module Bytes : Bap_trie_intf.S with type key = t
  end
end
