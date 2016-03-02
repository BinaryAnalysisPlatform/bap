(** A loadable memory image of an executable.  *)

open Core_kernel.Std
open Regular.Std
open Bap_types.Std

open Image_internal_std


(** {2 Type definitions}  *)

type t [@@deriving sexp_of]            (** image   *)

(** segment *)
type segment [@@deriving bin_io, compare, sexp]
(** symbol  *)
type symbol [@@deriving bin_io, compare, sexp]

type path = string

(** {2 Constructing}  *)

(** constructing an image can result in actual image and a set
    (hopefully empty) of errors occured in a process of decoding an
    image, that do not prevent us from actually creating an image. So,
    this information messages can be considered as warnings. *)
type result = (t * Error.t list) Or_error.t

(** [create ?backend filename] creates an image of the file specified
    specified by the [filename]. If [backend] is equal to "auto", then
    all backends are tried in order. If only one backend can read this
    file (i.e., there is no ambiguity), then image is returned. If
    [backend] is not specifed, then the LLVM backend is used. *)
val create : ?backend:string -> path -> result

(** [of_string ?backend ~data] creates an image from the specified
    [data]. See {!create} for [backend] parameter. *)
val of_string : ?backend:string -> string -> result

(** [of_bigstring ?backend ~data] creates an image from the specified
    [data]. See {!create} for [backend] parameter. *)
val of_bigstring : ?backend:string -> Bigstring.t -> result


(** {2 Attributes}  *)

val entry_point : t -> addr
val filename : t -> string option
val arch: t -> arch
val addr_size : t -> addr_size
val endian : t -> endian

val data : t -> Bigstring.t

(** {2 Tables }  *)
val words : t -> size -> word table
val segments : t -> segment table

(** @deprecated: this will be removed in a next release  *)
val symbols : t -> symbol table


val segment : segment tag
val symbol  : string tag
val section : string tag

(** returns memory  *)
val memory : t -> value memmap

(** {2 Mappings }  *)
val memory_of_segment  : t -> segment -> mem
(** [memory_of_symbol symbol]: returns the memory of symbol in acending order. *)
val memory_of_symbol   : t -> symbol -> mem * mem seq
val symbols_of_segment : t -> segment -> symbol seq
val segment_of_symbol  : t -> symbol -> segment



module Segment : sig
  type t = segment
  include Regular with type t := t
  val name : t -> string
  val is_writable   : t -> bool
  val is_readable   : t -> bool
  val is_executable : t -> bool
end

module Symbol : sig
  type t = symbol
  include Regular with type t := t
  val name : t -> string
  val is_function : t -> bool
  val is_debug : t -> bool
end

(** {2 Backend Interface}  *)

(** [register_backend ~name backend] tries to register backend under
    the specified [name]. *)
val register_backend : name:string -> Image_backend.t -> [ `Ok | `Duplicate ]

val available_backends : unit -> string list
