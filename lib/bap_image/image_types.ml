open Core_kernel.Std
open Bap_types.Std

(** Memory representation as a container of words.

    Word sizes can vary, by default [word_size] is equal to address
    size of the architecture under analysis (not the host architecture
    of course).

    All container operations a lifted to monad, but for convenience
    see [Memory.Unsafe] module. *)
module type Container = sig
  type t

  (** monad *)
  type 'a m

  include Monad.Infix with type 'a t := 'a m

  (** [copy/view word_size ~from ~words mem] returns a new memory
      that represents the specified region of memory [mem]. [copy]
      function performs deep copy.

      @param addr  defaults [min_addr mem]
      @param words defaults to the end of the memory region.
  *)
  val copy : ?word_size:int -> ?from:addr -> ?words:int -> t -> t m
  val view : ?word_size:int -> ?from:addr -> ?words:int -> t -> t m

  (** [get mem addr] reads memory value from the specified address *)
  val get : ?word_size:int -> t -> addr -> word m

  (** A common set of [Container.S0] operations raise to monad [m] *)
  val mem : ?word_size:int -> t -> word -> bool m
  val exists   : ?word_size:int -> t -> f:(word -> bool m) -> bool m
  val for_all  : ?word_size:int -> t -> f:(word -> bool m) -> bool m
  val count    : ?word_size:int -> t -> f:(word -> bool m) -> int m
  val find     : ?word_size:int -> t -> f:(word -> bool m) -> word option m
  val find_map : ?word_size:int -> t -> f:(word -> 'a option m) -> 'a option m

  val to_list  : ?word_size:int -> t -> word list m
  val to_array : ?word_size:int -> t -> word array m

  val fold  : ?word_size:int -> t -> init:'b -> f:('b -> word -> 'b m) -> 'b m
  val iter  : ?word_size:int -> t -> f:(word -> unit m) -> unit m

  (** [foldi,iteri] iterators provides an addresses for each word  *)
  val foldi : ?word_size:int -> t -> init:'b -> f:(addr -> 'b -> word -> 'b m) -> 'b m
  val iteri : ?word_size:int -> t -> f:(addr -> word -> unit m) -> unit m

  (** [{max,min}_addr] function specify upper and lower bounds of the memory *)
  val max_addr : t -> addr
  val min_addr : t -> addr

  (** [contains mem addr] returns true if [mem] contains address [addr]  *)
  val contains : t -> addr -> bool

  (** A set of low level input operations.
      Note: it is more effective to use above head iterators, instead
      of this low level interface  *)
  module Input : sig
    (** [reader mem ~pos_ref] defines a set of functions with a
        common interface. Each function accepts a memory [mem] and a
        [pos_ref] - a reference to a address that should be read. This
        reference will be updated for the amount of bytes that was
        actually read.

        @return a word lifted into a monad.
    *)
    type 'a reader = t -> pos_ref : addr ref -> 'a m
    val word   : ?word_size:int -> word reader
    val int8   : word reader
    val uint8  : word reader
    val int16  : word reader
    val uint16 : word reader
    val int32  : word reader
    val int64  : word reader
  end
  (** [hexdump t out] outputs hexdump (as per [hexdump -C]) of the
      memory to formatter [out]  *)
  val hexdump: t -> Format.formatter -> unit m
end

(** Memory exposes two views on the same data.
    A default view is safe, as it is raise to an [Or_error] monad.
    The second view, is indeed safe too, it just raises an exception
    instead of returning [Error].
*)

module type Memory = Container with type 'a m = 'a Or_error.t
module type Memory_exn = Container with type 'a m = 'a


module Backend = Image_backend
