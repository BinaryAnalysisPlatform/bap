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
      a interface.

      All {!Graphlib} algorithms accept a first-class module with
      graph implementation as a first argument. You can think of this
      parameter as an explicit type class. Later, when modular
      implicits will be accepted in OCaml, this parameter can be
      omitted. But for now, we need to pass them.

      A recommended way to work with {!Graphlib} is to bind the
      chosen implementation with some short name, usually [G] would be
      a good choice:

   {[module G = Graphlib.Make(String)(Bool)]}

      This will bind name [G] with a graph implementation that has
      [string] nodes, with edges labeled by values of type [bool].

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
    include Opaque.S with type t := t
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
    include Opaque.S with type t := t
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
    include Opaque.S with type t := t

    (** All graphs are printable.   *)
    include Printable.S with type t := t
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
    type t = equiv
    val to_int : t -> int
    include Regular.S with type t := t
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


  (** {4 Visual attributes for graph vizualization.}
      Consult OCamlGraph library for more information.
  *)

  type node_attr  = Graph.Graphviz.DotAttributes.vertex
  type edge_attr  = Graph.Graphviz.DotAttributes.edge
  type graph_attr = Graph.Graphviz.DotAttributes.graph

  type ('n,'a) labeled = {
    node : 'n;
    node_label : 'a;
  }


  (** A solution to a system of fixed-point equations.

      We represent the solution to a system of fixed-point equations
      as a pair of a finite mapping [M] that associates a variable (a
      node) with its value, and a default value, that is a value of
      all variables that are not in [M].
  *)
  module Solution : sig


    (** an abstract representation of a solution  *)
    type ('n,'d) t


    (** [create constraints default] creates an initial approximation of a
        solution. The [default] parameter defines the default value of
        all variables unspecified in the initial constraints.

        @param constraints a finite mapping from variables to their
        value approximations.

        For the returned value [s] the following is true:

        - [not(is_fixpoint s)]
        - [iterations s = 0]
        - [get s x = constraints[x] if x in constraints else default]
    *)
    val create : ('n,'d,_) Map.t -> 'd -> ('n,'d) t


    (** [iterations s] returns the total number of iterations that was
        made to obtain the current solution.  *)
    val iterations : ('n,'d) t -> int

    (** [default s] return the default value assigned to all variables
        not in the internal finite mapping. This is usually a bottom
        or top value, depending on whether iteration increases or
        decreases.
    *)
    val default : ('n,'d) t -> 'd

    (** [is_fixpoint s] is [true] if the solution is a fixed point
        solution, i.e., is a solution that stabilizes the system of
        equations.  *)
    val is_fixpoint : ('n,'d) t -> bool

    (** [get s x] returns a value of [x].  *)
    val get : ('n,'d) t -> 'n -> 'd

    (** [derive s ~f default] creates a new solution from an old one
        with a new [default] and where for each node [n] in [s]'s finite
        map, if [f n (get s n) = Some v] then [n] maps to [v].
    *)
    val derive : ('n,'d) t -> f:('n -> 'd -> 'a option) -> 'a -> ('n,'a) t

  end


  (** Generic Graph Library  *)
  module Graphlib : sig

    (** [Make(Node)(Edge)] creates a module that implements [Graph]
        interface and has unlabeled nodes of type [Node.t] and edges
        labeled with [Edge.t]

        In [Core_kernel] basically any type that is reasonable to be
        used as a graph node, satisfies the {!Opaque.S} interface. So,
        a new graph structure can be implemented directly, e.g.,
        {[module G = Graphlib.Make(Int64)(Unit)]}

        If a type doesn't satisfy the [Opaque] interface, then it can
        be easily derived with [Opaque.Make] if [compare] and [hash]
        functions are provided.

    *)
    module Make(Node : Opaque.S)(Edge : T) : Graph
      with type node = Node.t
       and type Node.label = Node.t
       and type Edge.label = Edge.t



    (** [Labeled(Node)(Node_label)(Edge_label)] creates a graph
        structure with both nodes and edges labeled with abitrary
        types.

        Contrary to [Make] functor, where a node and a node label are
        unified, the [Labeled] functor creates a graph data structure,
        where they are different. Moreover, the node label is pure
        abstract and can be any type, including functional.*)
    module Labeled(Node : Opaque.S)(NL : T)(EL : T) : Graph
      with type node = (Node.t, NL.t) labeled
       and type Node.label = (Node.t, NL.t) labeled
       and type Edge.label = EL.t


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

    (** [union (module G) g1 g2] returns a graph [g] that is a union
        of graphs [g1] and [g2], i.e., contains all nodes and edges
        from this graphs.

        Postcondition: {v
          - N(g) = N(g1) ∪ N(g2).
          - E(g) = E(g1) ∪ E(g2).
        v}
    *)
    val union :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) -> 'c -> 'c -> 'c

    (** [inter (module G) g1 g2] returns a graph [g] that is an
        intersection of graphs [g1] and [g2], i.e., it contain
        and edges from this graphs.

        Postcondition: {v
          - N(g) = N(g1) ∩ N(g2).
          - E(g) = E(g1) ∩ E(g2).
        v}
    *)
    val inter :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) -> 'c -> 'c -> 'c


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
      ?channel:Out_channel.t ->
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

        The algorithm is linear in time. It uses constant stack
        space. In fact, for small graphs it uses stack, but for large
        graphs dynamically switches to a heap storage. The space
        complexity is bounded by linear function of the graph depth.  *)
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
        frontiers for all nodes in a graph [g].

        The dominance frontier of a node [d] is the set of all nodes [n]
        such that [d] dominates an immediate predecessor of [n], but [d] does
        not strictly dominate [n]. It is the set of nodes where [d]'s
        dominance stops. *)
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

    (** [fixpoint ~equal ~init ~merge ~f g] computes a solution for a
        system of equations denoted by graph [g], using the inital
        approximation [init] (obtained either with [Solution.create] or
        from the previous calls to [fixpoint]).

        The general representation of the fixpoint equations is

        {v
          x(i) = f(i) (a(i,1) x(1) %  ... % a(i,j) x(j)),
        v}

        where
        - [x(i)] is the value of the [i]'th variable (node);
        - [a(i,j)] is [1] if there is an edge from the node
          [i] to the node [j] and [0] otherwise;
        - [%] the merge operator;
        - [f(i)] is the transfer function for the node [i];
        - [=] is the equivalence operation.

        A solution is obtained through a series of iterations until
        the fixpoint is reached, i.e., until the system
        stabilizes. The total number of iterations could be bound by
        an arbitrary number. If the maximum number of iterations is
        reached before the system stabilizes then the solution is not
        complete. An incomplete solution could be resumed later, or
        used as it is (for example, in case of ascending chain the
        solution is always a lower approximation of a real solution,
        so it is always safe to use it).

        @param the upper bound to the number of iterations the solver
        can make.

        @param start the entry node of the graph

        @param rev if [true] then graph is visited in the reverse order,
        defaults to [false].

        @param step a function that is called every time a new value
        of a variable is obtained (an extension point for
        narrowing/widening).

        @param equal compares two approximations for equivalence

        @param init initial approximation

        @param merge the operator for path merging
        (usually meet/join depending on a starting point).

        @param f the transfer function


        {3 Data Flow Analysis and Abstract Interpretation}

        The [fixpoint] function implements a general fixed pointed
        iterative solver, that is suitable for implementing data-flow
        analysis or abstract interpreters. This section will provide
        some insight on how a particular choice of parameters affects
        the result of computation. We will start with a small
        introduction to the theory of Data Flow Analysis and Abstract
        Interpretation, with respect to the solution of a system of
        fixed point equations.

        {4 Introduction}

        The data domain is a set of values equiped with a partial
        ordering operation [(L,<=)], also know as a lattice or a
        poset. We assume, that the lattice is complete, i.e., there
        are two special elements of a set that are called [top] and
        [bot] (the bottom value). The top element is the greatest
        element of the set L, i.e., for all [x] in [L], [x <=
        top]. Correspondingly, the bottom element is the least element
        of the set [L], i.e., for all [x] in [L], [bot <= x]. It is not
        required by the framework that the lattice has both or any of
        them, however their presence makes the explanation easier, and
        since any lattice could be artificially extended with these
        two elements, their introduction will not introduce a loss of
        generality. Since values of the lattice [L] represent
        information, the partial ordering between two pieces of
        information [a] and [b], e.g., [a <= b], tells us that [a]
        contains no more information than [b]. Therefore the [top]
        value contans all the information, representable in the
        lattice, correspondingly the [bot] value represents an absence
        of information. Thus, we will consider the bottom value as an
        over approximation (or a lower approximation in terms of the
        lattice ordering relation), as an absence of information
        cannot contain false statements (vacuous truth). Assuming,
        that there is some value [t], [bot <= t <= top] that represents
        a ground truth (that is usually unobtainable), we say that all
        values that are less than or equal [t] are over-approximations
        of the truth, and the rest of the values, are
        under-approximations. Under-approximations under estimate a
        behavior of a system, and usually represent information about
        a system that is not true, hence an under approximate solution,
        is usually unsafe to use. In general, our task is to find an
        over approximation that is as close as possible to the ground
        truth [t].

        A solution to a fixed point equation (i.e., an equation of the
        form [x = f(x)]) could be obtainted by starting from some
        initial approximation [x0] and repeatedly applying [f] to it,
        until the solution is found, i.e., [x = f(... f(f(x0)) ...)].
        In general, a function may have multiple (or no at all) fixed
        points. If a function has several fixed points, then we can
        distinguish two extremums - the least fixed point [lfp] and
        the greatest fixed point [gfp] w.r.t the ordering of lattice
        [L]. Assuming that function [f] is positive monotonic function
        (i.e., [x <= y] implies that [f(x) <= f(y)]), thechoice of the
        initial value [x0] denotes which of the two fixed points is
        computed. When we start with the bot value, we are ascending
        from it until the least fixed point is obtained. Dually, if we
        will start with the top value, we will descend until the
        maximal fixpoint is reached. Assuming that both fixpoints are
        lower approximations of the ground truth, we can easily see,
        that the maximal fixpoint solution is more attractive, as it
        bears more information than the minimal (unless both are the
        same). However, the ascending search, bears a nice property,
        that any intermediate solution is an over-approximation of the
        ground truth (i.e., we are monotonically aggregating facts).

        In general, a function may not have a solution at all, or the
        solution may not be computable in practice, i.e., when the chain
        of function applications [x = f(... f(x0) ...)] is either
        infinite or effectively infinite (e.g., 2^64 applications). The
        Tarksi theorem states that if [L] is complete and [f] is
        monotone, then it has a fixed point. If a lattice has a
        limited height (maximum length of chain of elements, such that
        x0 < x1 < .. < xM) then we will obtain a solution in no more
        than [M] steps. However, if [M] is very big, or infinite, then
        the solution won't be find in general, and the computation may
        not terminate.

        This brings us to the main distinction between Abstract
        Interpretation and Data Flow Analysis. The latter usually
        deals with lattice that have finite heights, while the former
        deals with very big and infinite lattices. To accelerate the
        convergence of a chain of approximations, a special technique
        called [widening] is used. Widening can be seen as an
        operation that jumps over several applications of the function
        [f], hence it actually accelerates the convergence. Given,
        that widening jumps forward in the chain, it is possible to
        overshoot the fixed point solution. This can potential involve
        a lossage of precision (in case of the descending chain) or to
        an incorrect solution (in case of the ascending chain).

        So far, we were considering only one equation. However, we
        need to solve a system of equations, denoted by a graph (as
        graph denotes a boolean adjacency matrix [A = {a(i,j)}], such that
        [a(i,j)] is [1] if there is an edge from [i] to [j], and [0]
        otherwise). To solve the system of equations, we need to find
        such vector [x1,..,xM] that solves all equations. In general,
        a system may not have a solution (an over-constrainted system),
        may have one solution, and may have many solutions (under
        constrained system). In our case, the system is not linear, as
        each equation is function of type [L -> L] not [L^M -> N], since
        all input variables are merged with some operator, usually
        [meet] or [join]. A correct choice of the merge opeator ensures
        correctness and convergence of the solution.

        The [meet] operator is a commutative, associative, and
        idempotent operator, such that if [z = meet x y], then [z <= x
        && z <= y] and for all [w] in [L], [w <= x && w <= y] implies [w <=
        z]. The [z] value is called the greatest lower bound ([glb], or
        [inf]) of [x] and [y]. Intuitively, the [meet] operator takes
        two pieces of information and removes all contradictions. Thus
        [z] is the maximal consensus between [x] and [y]. The [top]
        element is the neutral element with respect to the [meet]
        operation, e.g., [meet x top = x]. A consequent application of
        the [meet] operation builds a {i descending chain} of
        approximations, i.e., the amount of information is reduced on
        each step.

        The [join] operator is dual to [meet] and is defined
        correspondingly (with the flipped order). A join of [x] and
        [y] is called the least upper bound ([lub], [sup]) of [x] and
        [y]. Intuitively, the [join] operator takes two
        non-contradictory pieces information and returns their
        concatenation. The [bot] element is the neutral element with
        respect to the [join] operation, e.g., [join x bot = x]. A
        consequent application of the [join] operation builds an
        ascending chain of approximations, i.e., the amount of
        information is increased on each step.

        {4 Using the [fixpoint] function}

        The [fixpoint] interface is trying to be as general as
        possible, but at the same time easy to use. The interface,
        allows a user to choose the direction of approximation
        (ascending vs. descending) and how to accelerate the
        convergence in case of tall lattices by applying narrowing and
        widening. The implentation fixes the iteration strategy (by
        always using the topological ordering). In case if you need a
        fixed point solver, that allows you to use different iteration
        strategies, the [fixpoint][1] library provides a descent
        alternative.

        [1]: http://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/fixpoint/

        {5 Using [fixpoint] for Classical Data Flow analysis}

        The classical Data Flow analysis uses the descending chain of
        approximations in a complete lattice with finite (usually very
        small) height. Thus, the [fixpoint] solution is the greatest
        (maximal) fixed point (a maximal set of facts on which all
        equations agree). If the transfer function [f] is monotone and
        distributive, then it is the meet-over-paths (mop) solution,
        in a more general case [gfp <= mop] and [mop <= t] (where [t] is
        the ground truth). (Note, [gfp] is of course the best solution
        to the system of equations, as it is the maximal of all
        solutions. Both [mop] and [t] are not solutions to the given
        system, but define the true properties of a system under test,
        that we are trying to model with the system of fixed point
        equations. The fact that [gfp] is smaller, indicates that our
        model looses the precision (i.e., it is overconstrained). For
        example, the meet-over-path solution is a meet of all possible
        paths, even those that are infeasible, thus the system is
        overconstrained as we are taking into account system
        behaviors that will never happen, this is however safe, to
        overconstraint a system, as it will give us an
        over-approximation of a real system behavior).

        The [fixpoint] function could be easily applied to both
        forward and backward data flow problem, as backward problem
        could be seen as a forward problem on a reversed graph. To
        effectively reverse a graph, set the [rev] flag to [true] and
        set the [enter] parameter to the [exit] node.



        {5 Using [fixpoint] for Abstract Interpretation}

        Abstract Interpretation usually deals with complex and large
        lattices, and applies different heuristics to ensure
        termination with the minimum loss of precision. This usually
        ends up in applying widening and narrowing operations. Since
        widening accelerates by jumping forward in the chain of
        approximations, it can easily overshoot a fixed point, and in
        case of the ascending chain this may lead to a solution that
        is an under approximation of the ground truth. Thus, abstract
        interpretation is usually applied in the descending order. In
        this case the [widen] operation may still overshot the maximal
        fixpoint, that will lead to an over-approximation of the
        ground truth, that is safe, but still looses
        precision. Therefore, it is necessary to apply widening as
        rarely as possible. Unfortunatelly, a question where and when
        to apply widening is undecidable by itself, that's why
        heuristics are used. The [fixpoint] function, provides a
        limited capabilities to control widening via the [step i n x
        x'] function that is called every time a new [i]'th
        approximation [x'] for variable [n] is computed. The [step]
        function must return an upper bound (not necessary the least
        one) of the previous approximation [x] and the new
        approximation [x']. The default, implementation just returns
        [x']. An alternative implementation may widen [x'] if the
        number of steps [i] in the chain is higher than some
        threshold, and/or if [x] is a widening point (e.g., the loop
        header).

        Note: terms widening and narrowing comes from the interval
        analysis where they were first introduced, and correspond to
        the widening of an interval (usually up to infinitiy) and
        narrowing a widened interval based on some heurisitic.

        {6 Using [fixpoint] for general iterative approximation}

        In a general case, the [fixpoint] function could be used to
        compute successive approximations of a solution to a system of
        (in)equations, even if [f] is not monotone, and the lattice is not
        finite. The termination could be guaranteed by limiting the
        maximum number of iterations. And the correctness could be
        ensured by starting from the bottom element, and using the
        ascending chain of approximations. In that case, even a
        partially complete solution would be an over-approximation of
        a real solution. The obtained partial solution could be later
        resumed (and possibly extended with newly obtained facts).


        {4 Implementation}

        The [fixpoint] uses the Kildall iterative algorithm. And
        applies equations in reverse postorder (topological
        order). The solution is represented as an abstract finite
        mapping, that is also used to specify the initial set of
        constraints and the initial value of unconstrained
        variables. It is possible to specify more than one constraint
        to the system of equation (as opposed to the classical
        approach where the constraint denotes only the input for the
        entry node).  *)
    val fixpoint : (module Graph with type t = 'c
                                  and type node = 'n) ->
      ?steps:int -> ?start:'n -> ?rev:bool ->
      ?step:(int -> 'n -> 'd -> 'd -> 'd) ->
      init:('n,'d) Solution.t ->
      equal:('d -> 'd -> bool) ->
      merge:('d -> 'd -> 'd) -> f:('n -> 'd -> 'd) -> 'c -> ('n,'d) Solution.t


    (** name generation scheme  *)
    type scheme


    (** a function that gives a name for a value of type ['a]  *)
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


    (** Generic dot printer.  *)
    module Dot : sig


      (** [pp_graph ~nodes_of_edge ~nodes ~edges ppf] - generic dot
          printer.

          This function is useful for implementing custom dot
          printers. Use it if the default dot printer {!Graph.pp}
          doesn't suit your needs.

          For the purpose of this function a graph can be represented
          with three values:
            - [nodes_of_edge] returns the source and destination nodes
              of an edge;
            - [nodes] is a sequence of nodes;
            - [edges] is a sequence of edges;

          @param name the name of the graph.
          @param attrs graphviz attributes of the graph.
          @param string_of_node name of a node.
          @param node_label text representation of the node label.
          @param edge_label text representation of the edge label.

          All optional parameters default to a null value, so that
          if a parameter is not specified, then a corresponding entry
          will not be printed. *)
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
