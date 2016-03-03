open Core_kernel.Std
open Regular.Std

(**
      {3 Graph library}

      {!Graphlib} is a generic library that extends a well known
      OCamlGraph library. {!Graphlib} uses its own, more reach,
      {!Graph} interface that is isomorphic to OCamlGraph's [Sigs.P]
      signature for persistant graphs. Two functors witness the
      isomorphism of the interfaces:
      {!Graphlib.To_ocamlgraph} and {!Graphlib.Of_ocamlgraph}. Thanks
      to these functors, any algorithm written for OCamlGraph can be
      used on [Graphlibs] graph and vice verse.

      The {!Graph} interface provides a richer interface in a Core
      style. Nodes and Edges implements {!Opaque} data structure,
      i.e., they come with Maps, Sets, Hashtbls, etc, preloaded (e.g.,
      [G.Node.Set] is a set of node for graph implementation, provided
      by a module named [G]). Graphs also implement {!Printable}
      interface, that makes them much easier to debug.

      Along with graphs, auxiliary data structures are provided, like
      {{!Path}path} to represent paths in graph, {{!Tree}tree} for
      representing different graph spannings, {{!Partition}partition}
      for graph partitioning, and more.

      {!Graphlib} is a library that provides a set of generic
      algorithms, as well as implementations of a {!Graph} interface,
      and a suite of preinstantiated graphs.

      Contrary to OCamlGraph, each {!Graphlib} interface is provided
      as a function, not a functor. Thus making there use syntactically
      easier. Also, {!Graphlib} heavily uses optional and keyword
      parameters. For die-hards, many algorithms still have functor
      interface.

      All {!Graphlib} algorithms accept a first-class module with
      graph implementation as a first argument. You can think of this
      parameter as an explicit type class. Later, when modular
      implicits will be accepted in OCaml, this parameter can be
      omitted. But for now, we need to pass it.

      A recommended way to work with {!Graphlib} is to bind the
      chosen implementation with some short name, usually [G] would be
      a good choice:

   {[module G = Graphlib.String.Bool]}

      This will bind name [G] with a graph implementation that has
      [string] nodes, with edges marked by values of type [bool].

      To create a graph of type [G.t] one can use a generic
      {!Graphlib.create} function:

   {[let g = Graphlib.create (module G) ~edges:[
       "entry", "loop", true;
       "loop", "exit", true;
       "loop", "loop", false;
     ] ()]}

      This will create an instance of type [G.t]. Of course, it is
      still possible to use non-generic [G.empty], [G.Node.insert],
      [G.Edge.insert].
*)
module Std : sig
  (** {!Graph} nodes.  *)
  module type Node = sig
    (** Semantics of operations is denoted using mathematical model,
        described in {!Graph} interface.  *)

    type t                      (** node type is opaque  *)
    type graph
    type label
    type edge

    (** [create label] creates a new node, and associates it with a
        a given [label].  *)
    val create : label -> t

    (** [label n] returns a value associated with a node [n].  *)
    val label : t -> label

    (** [mem n g] is [true] if [n] is a member of nodes [N] of graph
        [g].  *)
    val mem : t -> graph -> bool

    (** [succs node graph] returns a sequence of successors of a
        [node] in a a given [graph] *)
    val succs : t -> graph -> t seq

    (** [preds node graph] returns a sequence of predecessors of a
        [node] in a a given [graph] *)
    val preds : t -> graph -> t seq

    (** [inputs node graph] is incomming edges of a [node] in [graph]  *)
    val inputs : t -> graph -> edge seq

    (** [outputs node graph] is outcomming edges of a [node] in [graph]  *)
    val outputs : t -> graph -> edge seq

    (** [degree ?dir n] when [in_or_out] is [`In] then returns
        the amount of incomming edges, otherwise returns the amount of
        outcomming edges. If parameter [dir] is left absent, then
        return the amount of adjacent nodes (i.e., a sum of incomming
        and outcomming edges).  *)
    val degree : ?dir:[`In | `Out] -> t -> graph -> int

    (** [insert n g] returns new graph [g'] that has a set of nodes
        [N] extended with node [n]. If [N] was contained [n], then
        the [g == g']. Use {!update} to change existing nodes.

        Postconditions: {v
          - N(g') = N(g) ∪ {n}.
          v}
    *)
    val insert : t -> graph -> graph

    (** [update n l g] if node [n] is not in [N(g)] then return [g],
        else return graph [g] where node [n] is labeled with [l].

        Postconditions: {v
          - n ∉ N(g) -> n ∉ N(g').
          - n ∈ N(g) → ν(g')n = l.
          v}
    *)
    val update : t -> label -> graph -> graph

    (** [remove n g] returns graph [g'], with a node [n] removed from
        a set of nodes [N].

        Postconditions: {v
          - E(g) ⊆ E(g')
          - N(g) ⊆ N(g')
          - N(g') = N(g) \ {n}.
          v}
    *)
    val remove : t -> graph -> graph

    (** [has_edge x y g] is true iff (x,y) ∈ E. *)
    val has_edge : t -> t -> graph -> bool

    (** [edge x y g] if graph [g] has an edge between nodes [x] and
        [y] then it is returned.  *)
    val edge : t -> t -> graph -> edge option

    (** node provides common data structures, like Table, Map, Set,
        Hash_set, etc.  *)
    include Opaque with type t := t
  end

  (** Interface that every Graph edge should provide  *)
  module type Edge = sig
    (** Semantics of operations is denoted using mathematical model,
        described in {!Graph} interface.  *)

    type t
    type node
    type graph
    type label

    (** [create x y l] creates an edge connecting nodes [x] and [y]
        labeled with a a given label [l] *)
    val create : node -> node -> label -> t

    (** [label e] returns a label of an edge [e]  *)
    val label : t -> label

    (** [src e] returns a source of an edge [e]  *)
    val src : t -> node

    (** [dst e] returns a destination of an edge [e] *)
    val dst : t -> node

    (** [mem e g] is true if [e] ∈ E.  *)
    val mem : t -> graph -> bool

    (** [insert e g] returns a graph [g'] with a set of edges extended
        with edge [e]. If [src e] or [dst e] wasn't in the set of nodes
        [N], then it is extended as well, so that axioms of graph are
        preserved.

        Postconditions: {v
          - E(g') = E(g) ∪ {e}.
          v}
    *)
    val insert : t -> graph -> graph

    (** [update e l g] if edge [e] exists in graph [g] then return a
        new graph [g'] in which edge [e] is associated with label [l].
        Otherwise return [g] unchanged.

        Postcondition: {v
          - E(g) ⊆ E(g')
          - N(g) ⊆ N(g')
          - e ∉ E(g) → e ∉ E(g').
          - e ∈ E(g) → ε(g')e = l.
          v}
    *)
    val update : t -> label -> graph -> graph

    (** [remove e g] returns a graph [g'] that doesn't contain edge
        [e].

        Postconditions: {v
          - E(g') = E(g) \ {e}.
          v}
    *)
    val remove : t -> graph -> graph
    include Opaque with type t := t
  end



  (** Graph signature.  *)
  module type Graph = sig
    (** Graph is mathematical data structure that is used to represent
        relations between elements of a set. Usually, graph is defined
        as an ordered pair of two sets - a set of vertices and a set
        of edges that is a 2-subset of the set of nodes,

        {v G = (V,E). v}

        In Graphlib vertices (called nodes in our parlance) and edges
        are labeled. That means that we can associate data with edges
        and nodes. Thus graph should be considered as an associative
        data structure. And from mathematics perspective graph is
        represented as an ordered 6-tuple, consisting of a set of nodes,
        edges, node labels, edge labels, and two functions that maps
        nodes and edges to their corresponding labels:

        {v G = (N, E, N', E', ν : N -> N', ε : E -> E'), v}

        where set [E] is a subset of [ N × N ].

        With this general framework an unlabeled graph can be
        represented as:

        {v G = (N, E, N, E, ν = λx.x, ε = λx.x) v}

        Another possible representation of an unlabeled graph would be:

        {v G = (N, E, {u}, {v}, ν = λx.u, ε = λx.v). v}

        Implementations are free to choose any suitable representation
        of graph data structure, if it conforms to the graph signature
        and all operations follows the described semantics and
        properties of a graph structure are preserved.

        The axiomatic semantics of operations on a graph is described by
        a set of postconditions. To describe the semantics of an
        operation in terms of input and output arguments, we project
        graphs to its fields with the following notation
        [<field>(<graph>)], e.g., [N(g)] is a set of nodes of graph [g].

        Only the strongest postcondition is specified, e.g., if it is
        specified that [νn = l], then it also means that

        [n ∈ N ∧ ∃u((u,v) ∈ E ∨ (v,u) ∈ E) ∧ l ∈ N' ∧ ...]

        In other words the structure [G] of the graph G is an invariant
        that is always preserved.
    *)

    (** type of graph  *)
    type t

    (** type of nodes *)
    type node

    (** type of edges  *)
    type edge


    (** Graph nodes.  *)
    module Node : Node with type graph = t
                        and type t = node
                        and type edge = edge

    (** Graph edges  *)
    module Edge : Edge with type graph = t
                        and type t = edge
                        and type node = node

    (** [empty] is an empty graph  *)
    val empty : t

    (** [nodes g] returns all nodes of graph [g] in an unspecified order  *)
    val nodes : t -> node seq

    (** [edges g] returns all edges of graph [g] in an unspecified order  *)
    val edges : t -> edge seq

    (** [is_directed] is true if graph is a directed graph.  *)
    val is_directed : bool

    (** [number_of_edges g] returns the size of a graph [g].  *)
    val number_of_edges : t -> int

    (** [number_of_nodes g] returns the order of a graph [g]  *)
    val number_of_nodes : t -> int

    (** All graphs provides a common interface for any opaque data structure  *)
    include Opaque with type t := t

    (** All graphs are printable.   *)
    include Printable with type t := t
  end

  (** a type abbreviation for a packed module, implementing graph
      interface.
      Note: this type prenexes only 3 out of 8 type variables, so,
      sometimes it is not enough. *)
  type ('c,'n,'e) graph =
    (module Graph with type t = 'c
                   and type node = 'n
                   and type edge = 'e)

  (** Graph edges classification.
      For explanations see {{!Graphlib.depth_first_search}DFS}.*)
  type edge_kind = [
    | `Tree                     (** edge is a part of a tree  *)
    | `Back                     (** back edge   *)
    | `Cross                    (** cross edge  *)
    | `Forward                  (** forward edge  *)
  ]

  (** a {!Tree} representation.  *)
  type 'a tree

  (** a type representing {!Frontier}s  *)
  type 'a frontier

  (** a {{!Partition}result} of partitioning algorithms  *)
  type 'a partition

  (** a partition {{!Group}Cell} *)
  type 'a group

  (** walk without a repetition of edges and inner nodes *)
  type 'a path

  (** runtime witness of the {{!Equiv}equivalence class} *)
  type equiv

  (** Tree is a particular subtype of a graph for which
      each node has only one predecessor, and there is only
      one path between tree root and any other node.
      Here is an example of a tree:
      {v
                                 (A)
                                  |
                          +-------+-------+
                          |       |       |
                         (B)     (C)     (D)
                                  |
                          +-------+-------+
                          |       |       |
                         (E)     (F)     (G)
                                  |
                                 (H)
        v}
  *)
  module Tree : sig
    type 'a t = 'a tree

    (** [children tree x] returns all immediate successors of node
        [x]. For example, children of node [A] is a sequence of
        [B,C,D]. But node [B] doesn't have any children at all.*)
    val children : 'a t -> 'a -> 'a seq

    (** [parent tree n] returns an immediate parent of a given node.
        Returns [None] only if [n] is a tree root. For example, parent
        of [F] is [C]. And [A] doesn't have a parent.*)
    val parent : 'a t -> 'a -> 'a option

    (** [ancestors tree n] returns a sequence of all ancestors of node
        [n]. An ancestor of a node is either a parent or an ancestor of
        the parent. For example, ancestors of [G] are [C] and [D]. The
        root node is the only one, that has an empty set of
        ancestors. *)
    val ancestors : 'a t -> 'a -> 'a seq

    (** [descendants tree n] returns a set of all descendants of a given
        node [n]. Descendant is either a child or a descendant of a
        child. For example, all nodes in the example [tree] (except
        the roots itself) are descendants of the root [A].  The
        descendants of [C] are [E,F,G,H].  *)
    val descendants : 'a t -> 'a -> 'a seq

    (** [is_child_of tree parent child] returns [true] if child is one
        of [children tree root] *)
    val is_child_of : 'a t -> parent:'a -> 'a -> bool

    (** [is_ancestor_of tree child x] returns true, if [x] is one of
        the ancestors of a [child] node.  *)
    val is_ancestor_of : 'a t -> child:'a -> 'a -> bool

    (** [is_descendant_of ~parent tree x] is [true] for all [x] that
        are descendants of a [parent] node.  *)
    val is_descendant_of : 'a t -> parent:'a -> 'a -> bool

    (** [to_sequence tree] enumerates nodes of a [tree] in an
        unspecified order.  *)
    val to_sequence : 'a t -> 'a seq

    (** [pp pp_elt] creates a pretty-printer for a node, based on
        element's pretty-printer [pp_elt]. The tree is printed in a dot
        format, for the ease of visualization. Example:
        {[let pp_int_tree = Tree.pp Int.pp]}
        Note: For all instatiations of [Graph] interface in the
        [Graphlib] library printable versions of [tree], [partition],
        [group] etc are provided. For example, for [Graphlib.Int.Bool]
        graph the printable version of a [tree] is available under
        [Graphlib.Int.Tree]. All instantiated pretty-printers are
        automatically installed once the library is loaded into the
        toplevel. *)
    val pp : 'a printer -> 'a t printer
  end

  (** Frontier maps each node into a possibly empty set of nodes.
      This is used for representing dominance and post-dominance
      frontiers.  *)
  module Frontier : sig

    type 'a t = 'a frontier

    (** [enum f x] enumerates frontier of [x]  *)
    val enum : 'a t -> 'a -> 'a seq

    (** [mem f x y] is true if [y] is in a frontier of [x]  *)
    val mem : 'a t -> 'a -> 'a -> bool

    (** [to_sequence frontier] enumerates all elements of a [frontier] *)
    val to_sequence : 'a t -> 'a seq

    (** [pp pp_elt] instantiates a pretty-printer for a given
        element. See {!Tree.pp} for more information.  *)
    val pp : 'a printer -> 'a t printer
  end

  (** Path between two nodes.  *)
  module Path : sig
    (** path is a walk without repetitions  *)

    (** representation type  *)
    type 'e t = 'e path

    (** [start p] the starting edge of a path [p] *)
    val start : 'e t -> 'e

    (** [finish p] the last edge of a path [p] *)
    val finish : 'e t -> 'e

    (** [edges p] a sequence of edges from start to finish  *)
    val edges : 'e t -> 'e seq

    (** [edges_rev p] a reversed sequence from finish to start  *)
    val edges_rev : 'e t -> 'e seq

    (** [weight p] total weight of a path *)
    val weight : 'e t -> int

    (** amount of edges in a path *)
    val length : 'e t -> int

    (** [pp pp_elt] constructs a pretty based on element printer
        [pp_elt] *)
    val pp : 'a printer -> 'a t printer
  end

  (** Result of a set partitioning.

      A partition of a set [S] is a set of non-empty subset of set [S],
      such that each element in a set of [S] is included in one and only
      one of the subsets and a union of the subsets forms a set [S].

      All nodes belonging to the same subset (called [group] in our
      parlance) represents the equivalence class. The equivalence side
      can be represented by a particular ordinal number or
      representative, that can be thought about as an ordinary
      number. See {!Equiv} for the representation of this ordinal
      numbers. A particular element of an equivalence class plays a
      role of «representative element». Depending on the nature of
      partitioning, this role can have different semantics.

      This data structure is used to represent results of partioning of
      a graph into groups of nodes, for example, to strongly connected
      components.*)
  module Partition : sig

    type 'a t = 'a partition

    (** [groups p] returns all partition cells of a partitioning [p] *)
    val groups : 'a t -> 'a group seq

    (** [group p x] returns a [group] of an element [x]. Note, this
        function is not total since the set of all values of type ['a] is
        usually larger than the underlying set that was partitioned.  *)
    val group : 'a t -> 'a -> 'a group option

    (** [equiv p x y] is true of [x] and [y] belongs to the same
        equivalence class (i.e., to the same group).  *)
    val equiv : 'a t -> 'a -> 'a -> bool

    (** [number_of_groups p] returns the amount of groups in a given
        partitioning [p]. *)
    val number_of_groups : 'a t -> int

    (** [of_equiv p n] rebuilds a group from an equivalence class
        ordinal number. *)
    val of_equiv : 'a t -> equiv -> 'a group option

    (** [pp pp_elem p] prints partition [p] using element printer [pp_elem]  *)
    val pp : 'a printer -> 'a t printer
  end

  (** Group is a non-empty set that is a result of partitioning of an
      underlying set [S] into a set of non-intersecting and non-empty
      subsets that cover set [S]. See {!Partition} for more
      information.  *)
  module Group : sig

    type 'a t = 'a group

    (** [enum group] enumerates all elements of a group, including the
        designated one.  *)
    val enum : 'a group -> 'a seq

    (** [mem group x] checks membership of [x] in a given [group].  *)
    val mem  : 'a group -> 'a -> bool

    (** [top group] returns the top element of a group also known as a
        representative element. The function is total since groups is
        guaranteed to be non-empty.    *)
    val top  : 'a group -> 'a

    (** [to_equiv g] returns the ordinal number representing the
        particular group [g] *)
    val to_equiv : 'a group -> equiv

    (** [pp pp_elem g] prints group [g] using element printer [pp_elem]  *)
    val pp : 'a printer -> 'a t printer

  end

  (** Ordinal for representing equivalence. Useful, for indexing
      elements based on their equivalence. *)
  module Equiv : sig
    type t
    val to_int : t -> int
    include Regular with type t := t
  end


  (** {5 Auxiliary graph data structures}  *)

  (** A type of modules for filtering graphs.
      See {!Graphlib.filtered} or {!Graphlib.Filtered}  *)
  module type Predicate = sig
    type edge
    type node
    val edge : edge -> bool
    val node : node -> bool
  end

  (** [Isomorphism] is a bijection between type [s] and [t].
      Usefull for creating graph views and mapping graphs.
      See {!Graphlib.view} and {!Graphlib.Mapper}.
  *)
  module type Isomorphism = sig
    type s
    type t
    val forward  : s -> t
    val backward : t -> s
  end

  class type ['n,'e,'s] dfs_visitor = object
    method start_tree :       'n -> 's -> 's
    method enter_node : int -> 'n -> 's -> 's
    method leave_node : int -> 'n -> 's -> 's
    method enter_edge : edge_kind -> 'e -> 's -> 's
    method leave_edge : edge_kind -> 'e -> 's -> 's
  end


  (** {4 Visual attributes for graphvizualization.}
      Consult OCamlGraph library for more information.
  *)

  type node_attr  = Graph.Graphviz.DotAttributes.vertex
  type edge_attr  = Graph.Graphviz.DotAttributes.edge
  type graph_attr = Graph.Graphviz.DotAttributes.graph

  type ('n,'a) labeled = {
    node : 'n;
    node_label : 'a;
  }


  (** Generic Graph Library  *)
  module Graphlib : sig

    (* we need this restatement, since first class modules in
       4.01 were nominally typed.  *)
    module type Graph = Graph

    (** [create (module G) ~nodes ~edges ()] creates a graph using
        implementation provided by [module G].
        Example:
        {[
          module G = Graphlib.String.Bool;;
          let g = Graphlib.create (module G) ~edges:[
              "entry", "loop", true;
              "loop", "exit", false;
              "loop", "loop", true] ()
        ]} *)
    val create :
      (module Graph with type t = 'c
                     and type Node.label = 'a
                     and type Edge.label = 'b) ->
      ?nodes:'a list ->
      ?edges:('a * 'a * 'b) list -> unit -> 'c

    (** [to_dot (module G) ~filename:"graph.dot" g] dumps graph [g]
        using [dot] format. This is a customizable version of printing
        function. For most cases it will be enough to use [G.pp] or
        [G.to_string] function. Use this function, if you really need
        to customize your output.

        @param graph_attrs a list of global graph attributes;
        @param node_attrs  a list of node specific attributes;
        @param edge_attrs  a list of edge specific attributes;
        @param string_of_node used to print nodes;
        @param string_of_edge used to print edges;
        @param channel where to output the graph;
        @param formatter where to output the graph;
        @param filename where to output the graph;

        Note: if no output parameter is provided, the graph will not be
        outputted. More than one output targets is OK. For example,
        [to_dot (module G) ~filename:"graph.dot" ~channel:stdout g] will
        output graph [g] into both file named ["graph.dot"] and
        standard output.

        Note: if [string_of_node] function is not provided, then graph
        nodes will be labeled with the reverse post order number.  *)
    val to_dot :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?graph_attrs:('c -> graph_attr list) ->
      ?node_attrs:('n -> node_attr list) ->
      ?edge_attrs:('e -> edge_attr list) ->
      ?string_of_node:('n -> string) ->
      ?string_of_edge:('e -> string) ->
      ?channel:out_channel ->
      ?formatter:Format.formatter ->
      ?filename:string -> 'c -> unit

    (** [depth_first_search (module G) ~init g].  It is the most
        important algorithm of the Graphlib. It builds a forest of
        spanning trees of a graph, classifies graph edges and numbers
        nodes. It is a Swiss-army knife, that is very useful in
        implementing many other algorithms. You can think of this
        function as [fold] on steroids. But unlike [fold], that
        accepts only one function, the [depth_first_search] accepts 5
        different functions, that will be called on different
        situations, allowing you to «fill in the blanks» of your
        algorithm.

        Although [depth_first_search] doesn't allow you to drive the
        walk itself, there're still ways to do this, using {!filtered}
        function. That allows you to hide nodes or edges from the
        walker, thus effectively erasing them from a graph, without
        even touching it.

        @param rev if true, then the graph [g] is traversed in a
        reverse direction. This is essentially the same, as reversing
        the graph, but make sure, that you've adjusted the start
        node.

        @param start if specified, then the traverse will be started
        from the node that is equal to node [start]. Otherwise the
        traverse is started from the first node of a graph as returned
        by [G.nodes], i.e., usually it is an arbitrary node.

        @param start_tree [node] [state] is called on each new spanning
        tree started by the algorithm. If all nodes are reachable from
        the start node, then this function will be called only
        once. If all nodes of a graph are connected, then this
        function, will be called only once.

        @param enter_node [pre] [node] [state] is called when a node
        is first discovered by the traversal. The number is a preorder
        number, also known as depth-first number or [dfnum]. All nodes
        are entered in a pre-order.

        @param leave_node [rpost] [node] [state] is called when all
        successors of a [node] are left (finished). The provided
        number is a reverse post order number, that also defines a
        topological sorting order of a graph. All nodes, are left in
        a post order.

        @param enter_edge [kind] [edge] [state] is called when and
        [edge] is first discovered. Edge kinds are described below.
        The destination of the edge may not be discovered (i.e.,
        entered) yet. But the source is already entered (but not
        finished).

        @param leave_edge [kind] [edge] [state] is called when the
        edge destination is at least started.

        {2 Edges classification}

        An edge in a spanning tree, produced by a depth first walk,
        can belong to one of the following category (kind):
        - Tree edges constitutes a spanning tree [T] of a graph;
        - Forward edges go from an ancestor to a descendants in
          a tree [T];
        - Back edges go from descendants to ancestors in [T],
          including node itself (they are also known as cycle
          edges).
        - Cross edges - all other edges, i.e., such edges for
          which doesn't go from ancestors to descendants or vice
          verse. They are possible since, tree defines only partial
          ordering.

        With respect to a pre-order and reverse post-ordering
        numbering the source [x] and a destination [y] of an edge with
        a given [kind] satisfy to the following inequalities:

        {v
            +---------+-----------------+---------------------+
            | Tree    | pre[x] < pre[y] | rpost[x] < rpost[y] |
            | Forward | pre[x] < pre[y] | rpost[x] < rpost[y] |
            | Back    | pre[x] ≥ pre[y] | rpost[x] ≥ rpost[y] |
            | Cross   | pre[x] > pre[y] | rpost[x] < rpost[y] |
            +---------+-----------------+---------------------+
          v}

        Note: since there can be more than one valid order of
        traversal of the same graph, (and thus more than one valid
        spanning tree), depending on a traversal the same edges can be
        classified differently. With the only exception, that a back
        edge will be always a back edge, disregarding the particular
        order.

        {3 Complexity}
        The algorithm is linear in time and space (including the stack
        space). In fact, for small graphs it uses stack, but for large
        graphs dynamically switches to a heap storage. *)
    val depth_first_search :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool ->
      ?start:'n ->
      ?start_tree:('n -> 's -> 's) ->
      ?enter_node:(int -> 'n -> 's -> 's) ->
      ?leave_node:(int -> 'n -> 's -> 's) ->
      ?enter_edge:(edge_kind -> 'e -> 's -> 's) ->
      ?leave_edge:(edge_kind -> 'e -> 's -> 's) ->
      'c -> init:'s -> 's

    (** [depth_first_visit (module G) ~init visitor g] allows to
        specify visiting functions using object. That opens space for
        re-usability and using open recursion.  *)
    val depth_first_visit :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> ?start:'n -> 'c -> init:'s -> ('n,'e,'s) dfs_visitor -> 's

    (** base class with all methods defaults to nothing.  *)
    class ['n,'e,'s] dfs_identity_visitor : ['n,'e,'s] dfs_visitor

    (** returns a sequence of nodes in reverse post order.  *)
    val reverse_postorder_traverse :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> ?start:'n -> 'c -> 'n seq

    (** returns a sequence of nodes in post order  *)
    val postorder_traverse :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> ?start:'n -> 'c -> 'n seq

    (** [dominators (module G) g entry] builds a dominators tree for
        a given graph.

        Definition: a {b walk} is a sequence of alternating nodes and
        edges, where each edge's endpoints are the preceding and
        following nodes in the sequence.

        Definition: a node [v] is {b reachable} if there exists a walk
        starting from [entry] and ending with [v].

        Definition: node [u] {b dominates} [v] if [u = v] or if all walks
        from [entry] to [v] contains [u].

        Definition: node [u] {b strictly dominates} [v] if it dominates
        [v] and [u <> v].

        Definition: node [u] {b immediately dominates} [v] if it
        strictly dominates [v] and there is no other node that
        strictly dominates [v] and is dominated by [u].

        Algorithm computes a dominator tree [t] that has the following
        properties:
        + Sets of graph nodes and tree nodes are equal;
        + if node [u] is a parent of node [v], then node [u]
           immediately dominates node [v];
        + if node [u] is an ancestors of node [v], then node [u]
           strictly dominates node [v];
        + if node [v] is a child of node [u], then node [u]
           immediately dominates node [v];
        + if node [v] is a descendant of node [u], then node [u]
           strictly dominates node [v].

        If every node of graph [g] is reachable from a provided
        [entry] node, then properties (2) - (5) are reversible, i.e.,
        an [if] statement can be read as [iff], and the tree is
        unique.


        {b Lemma}: Everything dominates unreachable block.

        {b Proof}: (by contradiction) suppose there exists a node [u] that
        doesn't dominate unreachable block [v]. That means, that there
        exists a path from [entry] to [v] that doesn't contain
        [u]. But that means, at least, that [v] is reachable. This  is
        a contradiction with the original statement that [v] is
        unreachable. {b Qed.}

        If some nodes of graph [g] are unreachable from the provided
        [entry] node, then they are dominated by all other nodes of a
        graph. It means that the provided system is under constrained
        and has more then one solution (i.e., there exists more than
        one tree, that satisfies properties (1) - (5). In a current
        implementation each unreachable node is immediately dominated
        by the [entry], if the [entry] is in graph.

        To get a post-dominator tree, reverse the graph by passing
        [true] to [rev] and pass exit node as a starting node.

        Note: although it is not imposed by the algotihm, but it is a
        good idea to have an entry node, that doesn't have any
        predecessors. Usually, this is what is silently assumed in
        many program analysis textbooks, but is not true in general
        for control-flow graphs that are reconstructed from binaries *)
    val dominators :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> 'c -> 'n -> 'n tree

    (** [dom_frontier (module G) g dom_tree] calculates dominance
        frontiers for all nodes in a graph [g].     *)
    val dom_frontier :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> 'c -> 'n tree -> 'n frontier

    (** [strong_components (module G) g] partition graph into strongly
        connected components. The top of each component is a root
        node, i.e., a node that has the least pre-order number.*)
    val strong_components :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      'c -> 'n partition

    (** [shortest_path (module G) ?weight ?rev g u v]
        Find a shortest path from node [u] to node [v].

        @param weight defines a weight of each edge. It defaults to 1.
        @param rev allows to reverse graph.    *)
    val shortest_path :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?weight:('e -> int) -> ?rev:bool -> 'c -> 'n -> 'n -> 'e path option

    (** [is_reachable (module G) ?rev g u v] is true if node [v] is
        reachable from node [u] in graph [g]. If rev is true, then it
        will solve the same problem but on a reversed graph.  *)
    val is_reachable :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> 'c -> 'n -> 'n -> bool

    (** [fold_reachable (module G) ?rev ~init ~f g n] applies function
        [f] to all nodes reachable from node [g] in graph [g]. If
        [rev] is true, then the graph is reversed.

        For example, the following will build a set of reachable nodes:
        [fold_reachable (module G) ~init:G.Node.Set.empty ~f:Set.add]
    *)
    val fold_reachable :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> init:'a -> f:('a -> 'n -> 'a) -> 'c -> 'n -> 'a

    (** [compare (module G1) (module G2) g1 g2] compares two graphs,
        with different implementation but the same node type.  *)
    val compare :
      (module Graph with type t = 'a
                     and type node = 'n) ->
      (module Graph with type t = 'b
                     and type node = 'n) ->
      'a -> 'b -> int

    (** [let module G' = filtered (module G) ?skip_node ?skip_edge ()]
        creates a new module [G'] that can be used at any place
        instead of [G], but that will hide nodes and edges, for which
        functions [skip_node] and [skip_edge] return true.

        Example:
        {[
          let killed_edges = G.Edge.Hash_set.create () in
          let module G = Graphlib.filtered (module G)
              ~skip_edge:(Hash_set.mem killed_edges) () in
          let rec loop g () =
            (* use (module G) as normal *)
            Hash_set.add killed_edges some_edge;
            (* all edges added to [killed_edges] will no be visible *)
        ]} *)
    val filtered :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?skip_node:('n -> bool) ->
      ?skip_edge:('e -> bool) -> unit ->
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e)

    (** [view (module G) ~node ~edge ~node_label ~edge_label]
        creates a proxy module, that will transform back and
        forward elements of graph, using corresponding functions.  *)
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


    (** [To_ocamlgraph(G)] returns a module that implements
        OCamlGraph interface for a persistent graph.  *)
    module To_ocamlgraph(G : Graph) :
      Graph.Sig.P with type t = G.t
                   and type V.t = G.node
                   and type E.t = G.edge
                   and type V.label = G.Node.label
                   and type E.label = G.Edge.label

    (** [Of_ocamlgraph(O)] creates an adapter module, that implements
        [Graphlib] interface on top of the module implementing
        [OCamlGraph] interface.*)
    module Of_ocamlgraph(G : Graph.Sig.P) :
      Graph with type t = G.t
             and type node = G.V.t
             and type edge = G.E.t
             and type Node.label = G.V.label
             and type Edge.label = G.E.label

    (** functorized version of a {!filter} function.  *)
    module Filtered
        (G : Graph)
        (P : Predicate with type node = G.node
                        and type edge = G.edge) :
      Graph with type t = G.t
             and type node = G.node
             and type edge = G.edge
             and module Node = G.Node
             and module Edge = G.Edge

    (** functorized version of {!Graphlib.view} function.  *)
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

    (** [Make(Node)(Edge)] creates a module that implements [Graph]
        interface and has unlabeled nodes of type [Node.t] and edges
        labeled with [Edge.t] *)
    module Make(Node : Opaque)(Edge : T) : Graph
      with type node = Node.t
       and type Node.label = Node.t
       and type Edge.label = Edge.t


    module Labeled(Node : Opaque)(NL : T)(EL : T) : Graph
      with type node = (Node.t, NL.t) labeled
       and type Node.label = (Node.t, NL.t) labeled
       and type Edge.label = EL.t

    type scheme
    type 'a symbolizer = ('a -> string)

    (** [create_scheme ~next init] create a name generator, that will
        start with [init] and apply [next] on it infinitly. *)
    val create_scheme : next:(string -> string) -> string -> scheme

    (** lower case symbols, starting from 'a' and moving up to 'z'.
        As 'z' is reached, all foregoing symbols will have a form
        of 'node_N' where 'N' is an increasing natural number. *)
    val symbols : scheme

    (** numbers from zero to inifinity ([Sys.max_int] in fact) *)
    val numbers : scheme
    (** empty string  *)
    val nothing : scheme

    val by_given_order : scheme -> ('a -> 'a -> int) -> 'a Sequence.t -> 'a symbolizer
    val by_natural_order : scheme -> ('a -> 'a -> int) -> 'a Sequence.t -> 'a symbolizer

    module Dot : sig
      val pp_graph :
        ?name:string ->
        ?attrs:string list ->
        ?string_of_node: 'n symbolizer ->
        ?node_label: 'n symbolizer ->
        ?edge_label: 'e symbolizer ->
        nodes_of_edge : ('e -> 'n * 'n) ->
        nodes: 'n Sequence.t ->
        edges: 'e Sequence.t -> Format.formatter -> unit
    end
  end

end
