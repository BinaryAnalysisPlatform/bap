open Core_kernel.Std
open Regular.Std
open Bap_types.Std
open Bap_image_std
open Graphlib.Std

type insn = Bap_disasm_insn.t
type jump = [
  | `Jump
  | `Cond
] [@@deriving compare, sexp]
type edge = [jump | `Fall] [@@deriving compare, sexp]
type t [@@deriving compare, sexp_of]

val create : mem -> (mem * insn) list -> t
val addr : t -> addr
val memory : t -> mem
val leader : t -> insn
val terminator : t -> insn
val insns : t -> (mem * insn) list
include Opaque.S     with type t := t
include Printable.S  with type t := t
