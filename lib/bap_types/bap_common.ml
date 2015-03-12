(** Common BIL type definitions.

    In this module basic types are defined, and it can be considered
    as an internal [Std] module, that should be included most modules,
    internal to the library. *)
open Core_kernel.Std

(** {2 Basic modules}
    The following modules defines the most basic types, on which the
    `bap_core` library is built.
*)
module Bitvector = Bap_bitvector
module Integer   = Bap_integer
module Regular   = Bap_regular
module Printable = Regular.Printable
module Trie      = Bap_trie

(** {2 Basic Interfaces}  *)
module type Integer   = Integer.S
module type Regular   = Regular.S
module type Printable = Regular.Printable
module type Trie      = Trie.S

type endian = Bitvector.endian =
    LittleEndian | BigEndian
with sexp,bin_io,compare


module Size = struct
  (** Defines possible sizes for operations operands  *)
  type all = [
    | `r8
    | `r16
    | `r32
    | `r64
    | `r128
    | `r256
  ] with bin_io, compare, sexp, variants

  type 'a p = 'a constraint 'a = [< all]
  with bin_io, compare, sexp

  type t = all p
  with bin_io, compare, sexp
end

(** size of operand  *)
type size = Size.t
with bin_io, compare, sexp

(** size of address  *)
type addr_size = [ `r32 | `r64 ] Size.p
with bin_io, compare, sexp

type nat1 = int
with bin_io, compare, sexp

(** The IR type of a BIL expression *)
module Type = struct
  type t =
    (** [Imm n] - n-bit immidiate   *)
    | Imm of nat1
    (** [Mem (a,t)]memory with a specifed addr_size *)
    | Mem of addr_size * size
  with bin_io, compare, sexp, variants
end

type typ = Type.t
with bin_io, compare, sexp


(** Supported architectures  *)
module Arch = struct
  type x86 = [
    | `x86
    | `x86_64
  ] with bin_io, compare, enumerate, sexp

  type arm = [
    | `arm
    | `armeb
    | `armv4
    | `armv4t
    | `armv5
    | `armv6
    | `armv7
    | `thumb
    | `thumbeb
  ] with bin_io, compare, enumerate, sexp

  type aarch64 = [
    | `aarch64
    | `aarch64_be
  ]
  with bin_io, compare, enumerate, sexp

  type ppc = [
    | `ppc
    | `ppc64
    | `ppc64le
  ]
  with bin_io, compare, enumerate, sexp

  type mips = [
    | `mips
    | `mipsel
    | `mips64
    | `mips64el
  ]
  with bin_io, compare, enumerate, sexp

  type sparc = [
    | `sparc
    | `sparcv9
  ]
  with bin_io, compare, enumerate, sexp

  type nvptx = [
    | `nvptx
    | `nvptx64
  ]
  with bin_io, compare, enumerate, sexp

  type hexagon = [`hexagon]
  with bin_io, compare, enumerate, sexp

  type r600 = [`r600]
  with bin_io, compare, enumerate, sexp

  type systemz = [`systemz]
  with bin_io, compare, enumerate, sexp

  type xcore = [`xcore]
  with bin_io, compare, enumerate, sexp

  type t = [
    | aarch64
    | arm
    | hexagon
    | mips
    | nvptx
    | ppc
    | r600
    | sparc
    | systemz
    | x86
    | xcore
  ] with bin_io, compare, enumerate, sexp, variants
end

(** {2 Common type abbreviations}
    You will see them later.
*)

type arch = Arch.t
with bin_io, compare, sexp

type word = Bap_bitvector.t
with bin_io, compare, sexp

type addr = Bap_bitvector.t
with bin_io, compare, sexp
