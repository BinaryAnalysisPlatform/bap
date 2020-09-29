(** A loadable memory image of an executable.  *)

open Bap_core_theory
open Core_kernel
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
val from_spec : Bigstring.t -> Ogre.doc -> result

val entry_point : t -> addr
val filename : t -> string option
val arch: t -> arch
val addr_size : t -> addr_size
val endian : t -> endian
val spec : t -> Ogre.doc
val data : t -> Bigstring.t

val words : t -> size -> word table
val segments : t -> segment table
val symbols : t -> symbol table

val segment : segment tag
val symbol  : string tag
val section : string tag
val code_region   : unit tag
val specification : Ogre.doc tag

val memory : t -> value memmap

val memory_of_segment  : t -> segment -> mem
val memory_of_symbol   : t -> symbol -> mem * mem seq
val symbols_of_segment : t -> segment -> symbol seq
val segment_of_symbol  : t -> symbol -> segment

module Spec : sig
  val from_arch : arch -> Ogre.doc
  val slot : (Theory.Unit.cls, Ogre.doc) KB.slot
end

module Segment : sig
  type t = segment
  include Regular.S with type t := t
  val name : t -> string
  val addr : t -> addr
  val size : t -> int
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


module type Loader = sig
  val from_file : string -> Ogre.doc option Or_error.t
  val from_data : Bigstring.t -> Ogre.doc option Or_error.t
end

val register_loader : name:string -> (module Loader) -> unit
val find_loader : string -> (module Loader) option
val available_backends : unit -> string list


module Scheme : sig
  open Ogre.Type
  type addr = int64
  type size = int64
  type off = int64
  type value = int64

  type 'a region = {addr : addr; size : int64; info : 'a}

  val off : int64 Ogre.field
  val size : int64 Ogre.field
  val addr : int64 Ogre.field
  val name : string Ogre.field
  val root : int64 Ogre.field
  val fixup : addr Ogre.field
  val readable : bool Ogre.field
  val writable : bool Ogre.field
  val executable : bool Ogre.field

  val arch : (string, (string -> 'a) -> 'a) Ogre.attribute
  val subarch : (string, (string -> 'a) -> 'a) Ogre.attribute
  val format : (string, (string -> 'a) -> 'a)  Ogre.attribute
  val vendor : (string, (string -> 'a) -> 'a) Ogre.attribute
  val system : (string, (string -> 'a) -> 'a) Ogre.attribute
  val abi : (string, (string -> 'a) -> 'a) Ogre.attribute
  val bits : (size, (size -> 'a) -> 'a) Ogre.attribute
  val is_little_endian : (bool, (bool -> 'a) -> 'a) Ogre.attribute
  val is_executable : (bool, (bool -> 'a) -> 'a) Ogre.attribute

  val bias : (off, (off -> 'a) -> 'a) Ogre.attribute
  val segment : ((bool * bool * bool) region,
                 (addr -> addr -> bool -> bool -> bool -> 'a) -> 'a) Ogre.attribute
  val section : (unit region, (addr -> addr -> 'a) -> 'a) Ogre.attribute
  val code_start : (addr, (addr -> 'a) -> 'a) Ogre.attribute
  val entry_point : (addr, (addr -> 'a) -> 'a) Ogre.attribute
  val symbol_chunk :
    (addr region, (addr -> addr -> addr -> 'a) -> 'a) Ogre.attribute

  val named_region :
    (string region, (addr -> addr -> string -> 'a) -> 'a) Ogre.attribute

  val named_symbol :
    (addr * string, (addr -> string -> 'a) -> 'a) Ogre.attribute

  val mapped : (int64 region, (addr -> addr -> addr -> 'a) -> 'a) Ogre.attribute

  val relocation :
    (addr * addr, (addr -> addr -> 'a) -> 'a) Ogre.attribute

  val external_reference :
    (addr * string, (addr -> string -> 'a) -> 'a) Ogre.attribute

  val base_address : (addr, (addr -> 'a) -> 'a) Ogre.attribute

  val code_region :
    (addr * size * off, (addr -> size -> off -> 'a) -> 'a) Ogre.attribute

  val symbol_value :
    (addr * value, (addr -> value -> 'a) -> 'a) Ogre.attribute
end
