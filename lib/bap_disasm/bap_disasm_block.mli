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

type t with compare, sexp_of
type insn = Bap_disasm_insn.t with compare,bin_io,sexp

include Block_accessors with type t := t and type insn := insn
include Block_traverse  with type t := t

val dfs : ?next:(t -> t seq) -> ?bound:mem -> t -> t seq


(** A classic control flow graph using OCamlgraph library.
    Graph vertices are made abstract, but the implement
    [Block_accessors] interface, including hash tables, maps, hash
    sets etc. *)
module Cfg : sig
  module Block : sig
    type t with sexp_of
    include Block_accessors with type t := t and type insn := insn
  end

  (** Imperative graph *)
  module Imperative : Graph.Sig.I
    with type V.t = Block.t
     and type V.label = Block.t
     and type E.t = Block.t * edge * Block.t
     and type E.label = edge

  (** The default graph is persistant  *)
  include Graph.Sig.P
    with type V.t = Block.t
     and type V.label = Block.t
     and type E.t = Block.t * edge * Block.t
     and type E.label = edge

end

(** [to_graph ?bound entry] builds a graph starting with [entry] and
    spanning all reachable blocks that are bounded by a memory region
    [bound].
    @param bound defaults to infinite memory region.
*)
val to_graph : ?bound:mem -> t -> Cfg.Block.t * Cfg.t
val to_imperative_graph :
  ?bound:mem -> t -> Cfg.Block.t * Cfg.Imperative.t


(** lifting from a lower level  *)
val of_rec_block : Bap_disasm_rec.block -> t
