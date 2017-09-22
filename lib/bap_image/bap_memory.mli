open Core_kernel.Std
open Regular.Std
open Bap_types.Std
open Image_common

type t [@@deriving sexp_of]

val create
  : ?pos:int                    (** defaults to [0]  *)
  -> ?len:int                    (** defaults to full length  *)
  -> endian
  -> addr
  -> Bigstring.t -> t Or_error.t

val of_file : endian -> addr -> string -> t Or_error.t
val view : ?word_size:size -> ?from:addr -> ?words:int -> t -> t Or_error.t
val range : t -> addr -> addr -> t Or_error.t
val merge : t -> t -> t Or_error.t
val first_byte : t -> t
val last_byte : t -> t
val endian : t -> endian
val get : ?disp:int -> ?index:int -> ?scale:size -> ?addr:addr -> t -> word Or_error.t
val (^) : t -> addr -> word Or_error.t
val (^!) : t -> addr -> word
val max_addr : t -> addr
val min_addr : t -> addr
val length : t -> int
val contains : t -> addr -> bool
val compare_with : t -> addr -> [
    | `addr_is_inside
    | `addr_is_below
    | `addr_is_above
  ]
module Input : sig
  type 'a reader = t -> pos_ref : addr ref -> 'a Or_error.t
  val word   : word_size:size -> word reader
  val int8   : word reader
  val uint8  : word reader
  val int16  : word reader
  val uint16 : word reader
  val int32  : word reader
  val int64  : word reader
  val int128 : word reader
  val int256 : word reader
end

include Printable.S with type t := t
val hexdump: t -> string
include Memory_iterators with type t := t
                          and type 'a m = 'a

module With_error : Memory_iterators with type t := t
                                      and type 'a m = 'a Or_error.t

module Make_iterators( M : Monad.S )
  : Memory_iterators with type t := t
                      and type 'a m = 'a M.t


val to_buffer : t -> Bigsubstring.t

module Trie : sig
  module Stable : sig
    module V1 : sig
      module R8  : Trie.S with type key = t
      module R16 : Trie.S with type key = t
      module R32 : Trie.S with type key = t
      module R64 : Trie.S with type key = t
    end
    module V2 : sig
      module R8  : Trie.S with type key = t
      module R16 : Trie.S with type key = t
      module R32 : Trie.S with type key = t
      module R64 : Trie.S with type key = t
    end

  end
  module R8  : Trie.S with type key = t
  module R16 : Trie.S with type key = t
  module R32 : Trie.S with type key = t
  module R64 : Trie.S with type key = t
end
