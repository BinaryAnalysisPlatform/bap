open Core_kernel.Std
open Regular.Std

module type Node = sig
  type t
  type graph
  type label
  type edge

  val create : label -> t
  val label  : t -> label
  val mem    : t -> graph -> bool
  val succs  : t -> graph -> t Sequence.t
  val preds  : t -> graph -> t Sequence.t
  val inputs : t -> graph -> edge Sequence.t
  val outputs: t -> graph -> edge Sequence.t
  val degree : ?dir:[`In | `Out] -> t -> graph -> int
  val insert : t -> graph -> graph
  val update : t -> label -> graph -> graph
  val remove : t -> graph -> graph
  val has_edge : t -> t -> graph -> bool
  val edge : t -> t -> graph -> edge option
  include Opaque with type t := t
end

module type Edge = sig
  type t
  type node
  type graph
  type label
  val create : node -> node -> label -> t
  val label : t -> label
  val src : t -> node
  val dst : t -> node
  val mem : t -> graph -> bool
  val insert : t -> graph -> graph
  val update : t -> label -> graph -> graph
  val remove : t -> graph -> graph
  include Opaque with type t := t
end

module type Graph = sig
  type node
  type edge

  type t

  module Node : Node with type graph = t
                      and type t = node
                      and type edge = edge

  module Edge : Edge with type graph = t
                      and type t = edge
                      and type node = node
  val empty : t

  val nodes : t -> node Sequence.t
  val edges : t -> edge Sequence.t

  val is_directed : bool

  val number_of_edges : t -> int
  val number_of_nodes : t -> int

  include Opaque with type t := t
  include Printable with type t := t
end

type ('c,'n,'e) graph =
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e)


module type Predicate = sig
  type edge
  type node
  val edge : edge -> bool
  val node : node -> bool
end


module type Isomorphism = sig
  type s
  type t
  val forward  : s -> t
  val backward : t -> s
end

type edge_kind = [
  | `Tree
  | `Back
  | `Cross
  | `Forward
]


class type ['n,'e,'s] dfs_visitor = object
  method start_tree : 'n -> 's -> 's
  method enter_node : int -> 'n -> 's -> 's
  method leave_node : int -> 'n -> 's -> 's
  method enter_edge : edge_kind -> 'e -> 's -> 's
  method leave_edge : edge_kind -> 'e -> 's -> 's
end

type ('n,'a) labeled = {
  node : 'n;
  node_label : 'a;
} with bin_io, sexp


type node_attr = Graph.Graphviz.DotAttributes.vertex
type edge_attr = Graph.Graphviz.DotAttributes.edge
type graph_attr = Graph.Graphviz.DotAttributes.graph
