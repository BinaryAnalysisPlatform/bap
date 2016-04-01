(** Common BIL type definitions.

    In this module basic types are defined, and it can be considered
    as an internal [Std] module, that should be included most modules,
    internal to the library. *)
open Core_kernel.Std
open Regular.Std

(** {2 Basic modules}
    The following modules defines the most basic types, on which the
    `bap_core` library is built.
*)
module Bitvector = Bap_bitvector
module Integer   = Bap_integer
module Trie      = Bap_trie

(** {2 Basic Interfaces}  *)
module type Integer   = Integer.S
module type Trie      = Bap_trie_intf.S


type endian = Bitvector.endian =
    LittleEndian | BigEndian
  [@@deriving sexp, bin_io, compare]


module Size = struct
  (** Defines possible sizes for operations operands  *)
  type all = [
    | `r8
    | `r16
    | `r32
    | `r64
    | `r128
    | `r256
  ] [@@deriving bin_io, compare, sexp, variants]

  type 'a p =
    'a constraint 'a = [< all] [@@deriving bin_io, compare, sexp]

  type t = all p
    [@@deriving bin_io, compare, sexp]
end

(** size of operand  *)
type size = Size.t
  [@@deriving bin_io, compare, sexp]

(** size of address  *)
type addr_size = [ `r32 | `r64 ] Size.p
  [@@deriving bin_io, compare, sexp]

type nat1 = int
  [@@deriving bin_io, compare, sexp]

(** The IR type of a BIL expression *)
module Type = struct
  type t =
    (** [Imm n] - n-bit immediate   *)
    | Imm of nat1
    (** [Mem (a,t)]memory with a specifed addr_size *)
    | Mem of addr_size * size
    [@@deriving bin_io, compare, sexp, variants]
end

type typ = Type.t
  [@@deriving bin_io, compare, sexp]


(** Supported architectures  *)
module Arch = struct
  type x86 = [
    | `x86
    | `x86_64
  ] [@@deriving bin_io, compare, enumerate, sexp]

  type arm = [
    | `armv4
    | `armv5
    | `armv6
    | `armv7
  ] [@@deriving bin_io, compare, enumerate, sexp]

  type armeb = [
    | `armv4eb
    | `armv5eb
    | `armv6eb
    | `armv7eb
  ] [@@deriving bin_io, compare, enumerate, sexp]

  type thumb = [
    | `thumbv4
    | `thumbv5
    | `thumbv6
    | `thumbv7
  ] [@@deriving bin_io, compare, enumerate, sexp]

  type thumbeb = [
    | `thumbv4eb
    | `thumbv5eb
    | `thumbv6eb
    | `thumbv7eb
  ] [@@deriving bin_io, compare, enumerate, sexp]


  type aarch64 = [
    | `aarch64
    | `aarch64_be
  ]
    [@@deriving bin_io, compare, enumerate, sexp]

  type ppc = [
    | `ppc
    | `ppc64
    | `ppc64le
  ]
    [@@deriving bin_io, compare, enumerate, sexp]

  type mips = [
    | `mips
    | `mipsel
    | `mips64
    | `mips64el
  ]
    [@@deriving bin_io, compare, enumerate, sexp]

  type sparc = [
    | `sparc
    | `sparcv9
  ]
    [@@deriving bin_io, compare, enumerate, sexp]

  type nvptx = [
    | `nvptx
    | `nvptx64
  ]
    [@@deriving bin_io, compare, enumerate, sexp]

  type hexagon = [`hexagon]
    [@@deriving bin_io, compare, enumerate, sexp]

  type r600 = [`r600]
    [@@deriving bin_io, compare, enumerate, sexp]

  type systemz = [`systemz]
    [@@deriving bin_io, compare, enumerate, sexp]

  type xcore = [`xcore]
    [@@deriving bin_io, compare, enumerate, sexp]

  type t = [
    | aarch64
    | arm
    | armeb
    | thumb
    | thumbeb
    | hexagon
    | mips
    | nvptx
    | ppc
    | r600
    | sparc
    | systemz
    | x86
    | xcore
  ] [@@deriving bin_io, compare, enumerate, sexp, variants]
end

(** {2 Common type abbreviations}
    You will see them later.
*)

type arch = Arch.t
  [@@deriving bin_io, compare, sexp]

type word = Bap_bitvector.t
  [@@deriving bin_io, compare, sexp]

type addr = Bap_bitvector.t
  [@@deriving bin_io, compare, sexp]
