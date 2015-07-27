open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_ir
open Bap_graph_intf
open Bap_graph

(* this module implements a graph structure on top of
   sub term. This is not a control flow graph, just a
   regular graph (no entry or exit node requirements
   are requested) *)

type t
type edge

module Edge : sig
  include Edge with type graph = t
                and type node = blk term
                and type t = edge

  val jmps  : [`after | `before] -> t -> graph -> jmp term Sequence.t
  val edges : [`after | `before] -> t -> graph -> t Sequence.t
  val jmp : t -> jmp term
  val tid : t -> tid
  val cond : t -> graph -> exp

  include Printable with type t := t
end

module Node : sig
  include Node with type graph = t
                and type t = blk term
                and type edge = edge
                and type label = blk term
  include Printable with type t := t
end

include Graph with type t := t
               and type node = blk term
               and type edge := edge
               and type Node.label = blk term
               and module Node := Node
               and module Edge := Edge

val create : ?tid:tid -> ?name:string -> unit -> t
val of_sub : sub term -> t
val to_sub : t -> sub term


module Tree : Printable with type t = node tree
module Frontier : Printable with type t = node frontier
module Path : Printable with type t = node path
module Partition : Printable with type t = node partition
module Group : Printable with type t = node group
