(** A loadable memory image of an executable.  *)

open Core_kernel.Std
open Regular.Std
open Bap_types.Std

open Image_internal_std

type t [@@deriving sexp_of]            (** image   *)
type segment [@@deriving bin_io, compare, sexp]
type symbol [@@deriving bin_io, compare, sexp]

type path = string

type result = (t * Error.t list) Or_error.t

val create : ?backend:string -> path -> result
val of_string : ?backend:string -> string -> result
val of_bigstring : ?backend:string -> Bigstring.t -> result

val entry_point : t -> addr
val filename : t -> string option
val arch: t -> arch
val addr_size : t -> addr_size
val endian : t -> endian

val data : t -> Bigstring.t
val backend_image : t -> Backend.Img.t

val words : t -> size -> word table
val segments : t -> segment table
val virtuals : t -> Backend.Segment.t list
val symbols : t -> symbol table


val segment : segment tag
val symbol  : string tag
val section : string tag

val memory : t -> value memmap

val memory_of_segment  : t -> segment -> mem
val memory_of_symbol   : t -> symbol -> mem * mem seq
val symbols_of_segment : t -> segment -> symbol seq
val segment_of_symbol  : t -> symbol -> segment

module Segment : sig
  type t = segment
  include Regular.S with type t := t
  val name : t -> string
  val is_writable   : t -> bool
  val is_readable   : t -> bool
  val is_executable : t -> bool
end

module Symbol : sig
  type t = symbol
  include Regular.S with type t := t
  val name : t -> string
  val is_function : t -> bool
  val is_debug : t -> bool
end

val register_backend : name:string -> Image_backend.t -> [ `Ok | `Duplicate ]

val available_backends : unit -> string list
