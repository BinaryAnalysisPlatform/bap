(** Recursive Descent Disassembler  *)

open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm_basic
open Bap_disasm_block_intf

type t
type block with compare, sexp_of
type insn = full_insn
type lifter = mem -> insn -> bil Or_error.t
type maybe_insn = insn option * bil option with sexp_of
type decoded = mem * maybe_insn with sexp_of

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
  include Block_accessors
    with type t = block
     and type insn := maybe_insn
  include Block_traverse  with type t := t
end
