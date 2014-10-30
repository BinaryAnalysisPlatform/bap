(** A loadable memory image of an executable.  *)

open Core_kernel.Std
open Bap_types.Std
open Image_types

(** {2 Type definitions}  *)

type t                          (** image   *)
type sec                        (** section *)
type sym                        (** symbol  *)
type mem                        (** memory  *)
type mem_exn                    (** memory with unsafe access *)

(** {2 Constructing}  *)

(** [create ?backend ~path:filename] creates an image of the file
    specified specified by the [filename]. If [backend] is not specfied,
    then all backends are tried in order. If only one backend can read
    this file (i.e., there is no ambiguity), then image is
    returned. *)
val create : ?backend:string -> path:string -> t Or_error.t

(** [of_string ?backend ~data] creates an image from the specified
    [data]. See {!create} for [backend] parameter. *)
val of_string : ?backend:string -> data:string -> t Or_error.t

(** [of_string ?backend ~data] creates an image from the specified
    [data]. See {!create} for [backend] parameter. *)
val of_bigstring : ?backend:string -> data:Bigstring.t -> t Or_error.t

(** {2 Attributes}  *)

val entry_point : t -> addr
val filename : t -> string
val arch: t -> arch
val addr_size : t -> Word_size.t
val endian : t -> endian

val sections : t -> sec Sequence.t
val symbols : t -> sym Sequence.t

(** [find_symbol img addr] lookups for a symbol at [addr]  *)
val find_symbol :  t -> addr -> sym option

(** [find_section img addr] looks for a section that
    contains [addr]  *)
val find_section : t -> addr -> sec option

(** returns entire image as a memory. See {!Image_types} module for the
    memory interface *)
val memory : t -> mem

val memory_exn : t -> mem_exn

val memory_with_exn : mem -> mem_exn

(** returns a hexdump (as per [hd] program) of the file memory  *)
val hexdump : t -> string

(** Image as a memory  *)
module Mem : Memory with type t := mem

module Mem_exn : Memory_exn with type t := mem_exn

(** Image section.

    A note about terminology. This section corresponds mostly to Elf's
    and Mach-O segments. The name section was chosen arbitrary, it is
    just part of the file that has something in common, like access
    bits, for example.
*)
module Sec : sig
  type t = sec
  val name : t -> string
  val addr : t -> addr
  val size : t -> int

  val is_executable : t -> bool
  val is_readable : t -> bool
  val is_writable : t -> bool
  val memory : t -> mem
  val memory_exn : t -> mem_exn
  val hexdump : t -> string
end

(** Image symbols.  *)
module Sym : sig
  type t = sym
  val name : t -> string option
  val addr : t -> addr
  val size : t -> int option
  val is_function : t -> bool option
  val memory : t -> mem option
  val hexdump : t -> string
end


(** {2 Backend Interface}  *)

(** [register_backend ~name backend] tries to register backend under
    the specified [name]. *)
val register_backend : name:string -> Backend.t -> [ `Ok | `Duplicate ]
