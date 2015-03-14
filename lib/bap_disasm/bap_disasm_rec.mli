(** Recursive Descent Disassembler  *)

open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm_basic

type t
type block with compare, sexp_of
type insn = full_insn
type lifter = mem -> insn -> bil Or_error.t
type decoded = mem * insn option * bil option with sexp_of
type jump = [
  | `Jump     (** unconditional jump                  *)
  | `Cond     (** conditional jump                    *)
] with compare, sexp

type edge = [jump | `Fall] with compare,sexp

type error = [
  | `Failed_to_disasm of mem
  | `Failed_to_lift of mem * insn * Error.t
] with sexp_of

val run :
  ?backend:string ->
  ?lifter:lifter -> ?roots:addr list -> arch -> mem -> t Or_error.t

val blocks : t -> block Table.t

val errors : t -> error list

module Block : sig
  type t = block with compare, sexp_of

  type dest = [
    | `Block of block * edge
    | `Unresolved of    jump
  ] with compare, sexp_of
  val addr : t -> addr
  val memory : t -> mem
  val leader : t -> decoded
  val terminator : t -> decoded
  val insns : t -> decoded list
  val succs : t -> t seq
  val preds : t -> t seq
  val dests : t -> dest seq
  include Printable with type t := t
end
