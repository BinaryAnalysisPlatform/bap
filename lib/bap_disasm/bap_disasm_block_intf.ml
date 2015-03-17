open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

(** a jump kind  *)
type jump = [
  | `Jump     (** unconditional jump                  *)
  | `Cond     (** conditional jump                    *)
] with compare, sexp

type edge = [jump | `Fall] with compare,sexp

module type Block_accessors = sig
  type t with compare, sexp_of
  type insn
  (** [addr block] address of the first instruction  *)
  val addr : t -> addr

  (** [memory blk] memory region, occupied by a block*)
  val memory : t -> mem

  (** [leader blk] the first instruction *)
  val leader : t -> insn

  (** [terminator blk] last instruction of the block  *)
  val terminator : t -> insn

  (** [insns blk] returns a list of block instructions  *)
  val insns : t -> (mem * insn) list

  include Comparable with type t := t
  include Hashable   with type t := t
  (** all the printing stuff, including [to_string] function *)
  include Printable  with type t := t
end

module type Block_traverse = sig
  type t
  type dest = [
    | `Block of t * edge
    | `Unresolved of    jump
  ] with compare, sexp_of

  (** [dests blk] block immediate destinations including unresolved
      one *)
  val dests : t -> dest seq

  (** [succs blk] block immediate successors  *)
  val succs : t -> t seq
  (** [preds blk] block immediate predecessors  *)
  val preds : t -> t seq
end
