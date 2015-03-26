open Bap_types.Std
open Image_internal_std
open Bap_disasm

type t = string table

(** [create roots base cfg]
    creates a symbol table from [cfg]. If no roots are
    provided, then only calls
*)
val create : addr list -> mem -> block table -> t

module Graph : Graph.Sig.P
  with type V.label = mem * string (** function body *)
   and type E.label = addr         (** callsite  *)


(** [to_graph blocks syms] creates a callgraph. Edges of the graph
    are labeled with a callsite *)
val to_graph : block table -> t -> Graph.t
