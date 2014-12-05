(** Memory representation. *)

open Core_kernel.Std
open Bap_types.Std
open Image_common

type t with sexp_of

val create
  : ?pos:int                    (** defaults to [0]  *)
  -> ?len:int                    (** defaults to full length  *)
  -> endian
  -> addr
  -> Bigstring.t -> t Or_error.t

(** [view word_size ~from ~words mem] returns a new memory
    that represents the specified region of memory [mem]. [copy]
    function performs deep copy.

    @param addr  defaults [min_addr mem]
    @param words defaults to the end of the memory region.
*)
val view : ?word_size:size -> ?from:addr -> ?words:int -> t -> t Or_error.t

(** [first_byte m] returns first byte of [m] as a memory  *)
val first_byte : t -> t
(** [last_byte m] returns last byte of [m] as a memory  *)
val last_byte : t -> t

(** [get word_size mem addr] reads memory value from the specified
    address. [word_size] default to [`r8] *)
val get : ?disp:int -> ?index:int -> ?scale:size -> ?addr:addr -> t -> word Or_error.t

(** [m^n] dereferences a byte at address [n]  *)
val (^) : t -> addr -> word Or_error.t

(** [m^.n] dereferences a byte at address [n]  *)
val (^!) : t -> addr -> word

(** [{max,min}_addr] function specify upper and lower bounds of the memory *)
val max_addr : t -> addr
val min_addr : t -> addr

(** [contains mem addr] returns true if [mem] contains address [addr]  *)
val contains : t -> addr -> bool

(** [compare_with mem addr] compares memory with [addr]  *)
val compare_with : t -> addr -> [
    | `addr_is_inside
    | `addr_is_below
    | `addr_is_above
  ]

(** A set of low level input operations.
    Note: it is more effective to use above head iterators, instead
    of this low level interface, since iterators do not need to check
    every memory access.  *)
module Input : sig
  (** [reader mem ~pos_ref] defines a set of functions with a
      common interface. Each function accepts a memory [mem] and a
      [pos_ref] - a reference to a address that should be read. This
      reference will be updated for the amount of bytes that was
      actually read.

      @return a word lifted into a monad.
  *)
  type 'a reader = t -> pos_ref : addr ref -> 'a Or_error.t
  val word   : word_size:size -> word reader
  val int8   : word reader
  val uint8  : word reader
  val int16  : word reader
  val uint16 : word reader
  val int32  : word reader
  val int64  : word reader
end

(** Pretty prints memory. *)
val pp : Format.formatter -> t -> unit

(** [hexdump t out] outputs hexdump (as per [hexdump -C]) of the
    memory to formatter [out]  *)
val hexdump: t -> string

(** a set of iterators, with identity monad.  *)
include Memory_iterators with type t := t
                          and type 'a m = 'a

(** iterators lifter to the Or_error monad  *)
module With_error : Memory_iterators with type t := t
                                      and type 'a m = 'a Or_error.t

(** lifts iterators to monad [M]  *)
module Make_iterators( M : Monad.S )
  : Memory_iterators with type t := t
                      and type 'a m = 'a M.t


(** { 3 Interfacing with C}

    The following interfaces is supposed to be used only for the
    purposes of exposing memory to c programs. *)

(** [to_buffers mem] creates a buffer representing the memory [mem].
    It is not specified whether the returned buffer has some sharing
    with underlying implementation. In other words the returned buffer
    shouldn't be modified.

    Since it is not guaranteed that memory is contiguous, a sequence of
    buffers is returned, with each buffer representing a contiguous
    part of memory.

*)
val to_buffer : t -> Bigsubstring.t
