open Core_kernel.Std
open Regular.Std
open Graphlib_intf
open Format

type 'a tree
type 'a frontier
type 'a partition
type 'a group
type 'a path
type equiv

module type Graph = Graph

val create : (module Graph with type t = 'c
                            and type Node.label = 'a
                            and type Edge.label = 'b) ->
  ?nodes:'a list ->
  ?edges:('a * 'a * 'b) list -> unit -> 'c



val union :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) -> 'c -> 'c -> 'c

val inter :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) -> 'c -> 'c -> 'c

val to_dot :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) ->
  ?graph_attrs:('c -> graph_attr list) ->
  ?node_attrs:('n -> node_attr list) ->
  ?edge_attrs:('e -> edge_attr list) ->
  ?string_of_node:('n -> string) ->
  ?string_of_edge:('e -> string) ->
  ?channel:Out_channel.t ->
  ?formatter:formatter ->
  ?filename:string -> 'c -> unit

val depth_first_search : (module Graph with type t = 'c
                                        and type node = 'n
                                        and type edge = 'e) ->
  ?rev:bool -> ?start:'n ->
  ?start_tree:('n -> 's -> 's) ->
  ?enter_node:(int -> 'n -> 's -> 's) ->
  ?leave_node:(int -> 'n -> 's -> 's) ->
  ?enter_edge:(edge_kind -> 'e -> 's -> 's) ->
  ?leave_edge:(edge_kind -> 'e -> 's -> 's) ->
  'c -> init:'s -> 's

val depth_first_visit : (module Graph with type t = 'c
                                       and type node = 'n
                                       and type edge = 'e) ->
  ?rev:bool -> ?start:'n -> 'c -> init:'s -> ('n,'e,'s) dfs_visitor -> 's

class ['n,'e,'s] dfs_identity_visitor : ['n,'e,'s] dfs_visitor

val reverse_postorder_traverse :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) -> ?rev:bool -> ?start:'n -> 'c -> 'n Sequence.t

val postorder_traverse :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) -> ?rev:bool -> ?start:'n -> 'c -> 'n Sequence.t

val dominators : (module Graph with type t = 'c
                                and type node = 'n
                                and type edge = 'e) -> ?rev:bool -> 'c -> 'n -> 'n tree

val dom_frontier : (module Graph with type t = 'c
                                  and type node = 'n
                                  and type edge = 'e) -> ?rev:bool -> 'c -> 'n tree -> 'n frontier

val strong_components :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) -> 'c -> 'n partition

val shortest_path :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) ->
  ?weight:('e -> int) -> ?rev:bool -> 'c -> 'n -> 'n -> 'e path option

val is_reachable :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) ->
  ?rev:bool -> 'c -> 'n -> 'n -> bool


val fold_reachable :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) ->
  ?rev:bool -> init:'a -> f:('a -> 'n -> 'a) -> 'c -> 'n -> 'a


val compare :
  (module Graph with type t = 'a
                 and type node = 'n) ->
  (module Graph with type t = 'b
                 and type node = 'n) ->
  'a -> 'b -> int

val filtered :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e) ->
  ?skip_node:('n -> bool) ->
  ?skip_edge:('e -> bool) -> unit ->
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e)

val view :
  (module Graph with type t = 'c
                 and type node = 'n
                 and type edge = 'e
                 and type Node.label = 'a
                 and type Edge.label = 'b) ->
  node:(('n -> 'f) * ('f -> 'n)) ->
  edge:(('e -> 'd) * ('d -> 'e)) ->
  node_label:(('a -> 'p) * ('p -> 'a)) ->
  edge_label:(('b -> 'r) * ('r -> 'b)) ->
  (module Graph with type t = 'c
                 and type node = 'f
                 and type edge = 'd
                 and type Node.label = 'p
                 and type Edge.label = 'r)




module To_ocamlgraph(G : Graph) :
  Graph.Sig.P with type t = G.t
               and type V.t = G.node
               and type E.t = G.edge
               and type V.label = G.Node.label
               and type E.label = G.Edge.label

module Of_ocamlgraph(G : Graph.Sig.P) :
  Graph with type t = G.t
         and type node = G.V.t
         and type edge = G.E.t
         and type Node.label = G.V.label
         and type Edge.label = G.E.label


module Filtered
    (G : Graph)
    (P : Predicate with type node = G.node
                    and type edge = G.edge) :
  Graph with type t = G.t
         and type node = G.node
         and type edge = G.edge
         and module Node = G.Node
         and module Edge = G.Edge

module Mapper
    (G  : Graph)
    (N  : Isomorphism with type s = G.node)
    (E  : Isomorphism with type s = G.edge)
    (NL : Isomorphism with type s = G.Node.label)
    (EL : Isomorphism with type s = G.Edge.label) :
  Graph with type t = G.t
         and type node = N.t
         and type edge = E.t
         and type Node.label = NL.t
         and type Edge.label = EL.t



module Tree : sig
  type 'a t = 'a tree

  val children : 'a t -> 'a -> 'a Sequence.t
  val parent : 'a t -> 'a -> 'a option
  val ancestors : 'a t -> 'a -> 'a Sequence.t
  val descendants : 'a t -> 'a -> 'a Sequence.t

  val is_child_of : 'a t -> parent:'a -> 'a -> bool
  val is_ancestor_of : 'a t -> child:'a -> 'a -> bool
  val is_descendant_of : 'a t -> parent:'a -> 'a -> bool

  val to_sequence : 'a t -> 'a Sequence.t
  val pp : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
end

module Frontier : sig
  type 'a t = 'a frontier
  val enum : 'a t -> 'a -> 'a Sequence.t
  val mem : 'a t -> 'a -> 'a -> bool

  val to_sequence : 'a t -> 'a Sequence.t
  val pp : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
end

module Partition : sig
  type 'a t = 'a partition
  val groups : 'a t -> 'a group Sequence.t
  val group : 'a t -> 'a -> 'a group option
  val equiv : 'a t -> 'a -> 'a -> bool
  val number_of_groups : 'a t -> int
  val of_equiv : 'a t -> equiv -> 'a group option
  val pp : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
end

module Group : sig
  type 'a t = 'a group
  val enum : 'a group -> 'a Sequence.t
  val mem  : 'a group -> 'a -> bool
  val top  : 'a group -> 'a
  val to_equiv : 'a group -> equiv
  val pp : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
end


module Equiv : sig
  type t = equiv
  val to_int : t -> int
  include Regular.S with type t := t
end


module Path : sig
  type 'e t = 'e path
  val edges : 'e t -> 'e Sequence.t
  val edges_rev : 'e t -> 'e Sequence.t
  val start : 'e t -> 'e
  val finish : 'e t -> 'e
  val weight : 'e t -> int
  val length : 'e t -> int
  val pp : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
end
