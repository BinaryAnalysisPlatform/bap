open Core_kernel
open Regular.Std
open Bap.Std



(** DWARF parser  *)
module Std : sig

  (** Dwarf library
      This library gives an access to debugging information stored
      in a binary program.  *)
  module Dwarf : sig

    (** File sections  *)
    module Section : sig
      type t =
        | Info
        | Abbrev
        | Str
      [@@deriving sexp, bin_io, compare, variants]
    end

    (** Debug Entry Tag  *)
    module Tag : sig
      type t =
        | Compile_unit
        | Partial_unit
        | Subprogram
        | Entry_point
        | Inlined_subroutine
        | Unknown of int
      [@@deriving sexp, bin_io, compare, variants]
    end


    (** Attribute  *)
    module Attr : sig
      type t =
        | Name
        | Low_pc
        | High_pc
        | Entry_pc
        | Unknown of int
      [@@deriving sexp, bin_io, compare, variants]
    end

    type lenspec =
      | Leb128
      | One
      | Two
      | Four
      | Eight
    [@@deriving sexp, bin_io, compare]

    (** Attribute form  *)
    module Form : sig
      type t =
        | Addr
        | String
        | Block of lenspec
        | Const of lenspec
        | Flag_present
        | Strp
        | Ref of lenspec
        | Indirect
        | Offset
        | Expr
        | Sig
      [@@deriving sexp, bin_io, compare, variants]
    end

    type tag  = Tag.t  [@@deriving sexp, bin_io, compare]
    type attr = Attr.t [@@deriving sexp, bin_io, compare]
    type form = Form.t [@@deriving sexp, bin_io, compare]
    type section = Section.t [@@deriving sexp, bin_io, compare]
    type fn [@@deriving bin_io, compare, sexp]

    (** Function representation.  *)
    module Fn : sig
      type t = fn [@@deriving bin_io, compare, sexp]

      (** [pc_lo fn] the lowest address of a function (the entry point)  *)
      val pc_lo : t -> addr


      (** [pc_hi fn] the highest address (if known)  *)
      val pc_hi : t -> addr option
      include Identifiable.S with type t := t
    end


    (** Buffer is a light abstraction over [string] and [bigstring],
        that can allow one to share the same string for different sections
        without explicit copying.
    *)
    module Buffer : sig
      type 'a t
      (** [create ~pos:0 ] creates a buffer from a data  *)
      val create: ?pos:int -> 'a -> 'a t

      (** [with_pos buf pos] creates a new buffer that shares data with
          [buf], but has different starting position  *)
      val with_pos: 'a t -> int -> 'a t

      (** [with_off buf off] creates a new buffer that shares data with
          [buf], but has different starting position equal to [pos buf + off] *)
      val with_off: 'a t -> int -> 'a t

      (** [pos buf] starting position  *)
      val pos: 'a t -> int

      (** [data pos] actual data.

          Note: it doesn't start from [pos], it start from [0] *)
      val data: 'a t -> 'a
    end

    module Data : sig
      type 'a t
      type 'a buffer = 'a Buffer.t

      (** [create endian sections] creates data representation from a assoc list
          of sections. Will complain if there're repeating sections.  *)
      val create: endian -> (section * 'a buffer) list -> 'a t Or_error.t

      (** [section data] lookups for a [section] in [data]  *)
      val section: 'a t -> section -> 'a buffer Or_error.t

      (** [endian data] the endianness of [data]  *)
      val endian: 'a t -> endian
    end

    (** Function boundary identification.  *)
    module Fbi : sig
      type t

      (** [create data] tries to create a DWARF reader, from
          supplied [data]. May yield an error, if there wasn't sufficient
          sections, or if format is not understandable.

          To provide information about functions parser needs at least this
          three sections:

          - .debug_abbrev [Section.Abbr]
          - .debug_info   [Section.Info]
          - .debug_str    [Section.Str]
      *)
      val create : string Data.t -> t Or_error.t

      (** [functions searcher] enumerates functions  *)
      val functions : t -> (string * fn) seq
    end
  end

  module Leb128 : sig
    (** an encoded value  *)
    type t [@@deriving bin_io, compare, sexp]

    (** [encode ~signed v] encodes value [v] in a LEB128 format. If
        signed is true, then uses signed encoding. *)
    type 'a encoder = ?signed:bool -> 'a -> t
    (** [decode leb] decodes a number from LEB128 representation.  *)
    type 'a decoder = t -> 'a Or_error.t

    (** [size leb] return size in bytes of the number stored in LEB128
        encoding.  *)
    val size: t -> int
    val read: ?signed:bool -> string -> pos_ref:int ref -> t Or_error.t
    val write: t -> string -> pos:int -> unit

    val to_int:   int   decoder
    val to_int32: int32 decoder
    val to_int64: int64 decoder

    val of_int:   int   encoder
    val of_int32: int32 encoder
    val of_int64: int64 encoder
  end
end
