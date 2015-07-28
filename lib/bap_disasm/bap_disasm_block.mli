(** A basic block  *)

(** {4 Navigating blocks structure}

    The following functions allows you to navigate through blocks
    without explicitly using graphs. Each neighborhood function
    returns closest neighbors as a lazy sequence. Please, be cautious,
    since this can contain loops, i.e. block can contain itself as a
    predecessor.

    You can use [Block.compare] or [compare_block] functions, to safely
    compare blocks with each other, without a risk of non-termination.
*)

(** returns a sequence of instructions that was decoded.
    Note, the sequence is not guaranteed to be contigious, instructions
    that wasn't decoded will be skipped.
*)
open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_basic
open Bap_disasm_block_intf
open Bap_graph_intf

type t with compare, sexp_of
type insn = Bap_disasm_insn.t with compare,bin_io,sexp

include Block_accessors with type t := t and type insn := insn
include Block_traverse  with type t := t

module Graph : Graph
  with type node = t
   and type Node.label = t
   and type Edge.label = edge


(** [to_graph ?bound entry] builds a graph starting with [entry] and
    spanning all reachable blocks that are bounded by a memory region
    [bound].
    @param bound defaults to infinite memory region.
*)
val to_graph : ?bound:(addr -> bool) -> t -> Graph.t


(** lifting from a lower level  *)
val of_rec_block : Bap_disasm_rec.block -> t
