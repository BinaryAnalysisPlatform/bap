open Core_kernel.Std
open Regular.Std
open Graphlib_intf
open Format

module type Graph = Graph

type 'a seq = 'a Seq.t


class type ['a] set = object
  method enum : 'a Seq.t
  method mem : 'a -> bool
end

class type ['a,'b] map = object
  method find : 'a -> 'b option
end

type 'a tree = {
  all : 'a list;
  par : ('a, 'a) map;
  cld : ('a,'a set) map;
  ans : ('a,'a set) map;
  des : ('a,'a set) map;
}

let create_set set = object
  method enum = Set.to_sequence set
  method mem x = Set.mem set x
end

let create_map find = object
  method find = find
end

let string_of_set ~sep pp_elt set =
  Seq.map set#enum ~f:(asprintf "%a" pp_elt) |>
  Seq.to_list |> String.concat ~sep

let empty_set map =
  Set.empty ~comparator:(Map.comparator map)

let create_ancestors empty find : ('a,'a set) map =
  let rec walk set x = match find x with
    | None -> set
    | Some p -> walk (Set.add set p) p in
  object
    method find x = Some (create_set (walk empty x))
  end

let create_descendants children : ('a,'a set) map =
  let rec walk set x = match Map.find children x with
    | None -> set
    | Some cs -> Set.union cs @@ Set.fold cs ~init:set ~f:walk in
  object
    method find x =
      Some (create_set (walk (empty_set children) x))
  end

let create_tree parents children = {
  all = Map.keys children;
  par = create_map parents;
  ans = create_ancestors (empty_set children) parents;
  des = create_descendants children;
  cld = object
    method find x =
      Option.(Map.find children x >>| create_set)
  end
}

let enum = function
  | Some x -> x#enum
  | None -> Seq.empty

module Tree = struct
  type 'a t = 'a tree

  let children t x = t.cld#find x |> enum
  let parent t x = t.par#find x
  let ancestors t x = t.ans#find x |> enum
  let descendants t x = t.des#find x |> enum
  let mem table t p = match (table t)#find p with
    | None -> fun _ -> false
    | Some xs -> xs#mem

  let is_ancestor_of t ~child = mem (fun t -> t.ans) t child
  let is_descendant_of t ~parent = mem (fun t -> t.des) t parent
  let is_child_of t ~parent = mem (fun t -> t.cld) t parent

  let to_sequence t = Seq.of_list t.all

  let pp pp_elt ppf tree =
    fprintf ppf "@.@[<v2>digraph {";
    begin match tree.all with
      | [p] -> fprintf ppf "@;\"%a\"" pp_elt p
      | _ -> List.iter tree.all ~f:(fun p ->
          Seq.iter (children tree p) ~f:(fun c ->
              fprintf ppf "@;\"%a\" -> \"%a\"" pp_elt p pp_elt c));
    end;
    pp_close_box ppf ();
    fprintf ppf "@]@.}"

  let () = Pretty_printer.register "Graphlib.Std.Tree.pp"
end

module Frontier = struct
  type 'a t = {
    all : 'a list;
    map : ('a,'a set) map
  }
  let create all map = {all; map}

  let enum t x = t.map#find x |> enum
  let mem t x = match t.map#find x with
    | None -> fun _ -> false
    | Some xs -> xs#mem

  let to_sequence t = Seq.of_list t.all

  let pp pp_elt ppf t =
    List.iter t.all ~f:(fun src ->
        match t.map#find src with
        | None -> fprintf ppf "()"
        | Some set ->
          fprintf ppf "[%a => (%s)]@." pp_elt src
            (string_of_set ~sep:" " pp_elt set))

  let () = Pretty_printer.register "Graphlib.Std.Frontier.pp"
end

module Equiv = struct
  type t = int [@@deriving bin_io, compare, sexp]
  let to_int = ident
  include Regular.Make(struct
      include Int
      let module_name = Some "Graphlib.Std.Equiv"
      let version = "1.0.0"
    end)
end

type equiv = Equiv.t [@@deriving bin_io, compare, sexp]

module Group = struct
  (* top of set should be included into the set *)
  type 'a t = {
    top : 'a;
    set : 'a set;
    ord : equiv;
  }

  let create top set ord = {top; set; ord}
  let enum t = t.set#enum
  let mem t = t.set#mem
  let top t = t.top
  let to_equiv t = t.ord
  let pp pp_elt ppf t =
    fprintf ppf "{%d: %a => (%s)}"
      t.ord pp_elt t.top (string_of_set ~sep:" " pp_elt t.set)

  let () = Pretty_printer.register "Graphlib.Std.Group.pp"
end

type 'a group = 'a Group.t

module Partition = struct
  type 'a t = {
    roots : 'a array;
    groups: 'a set array;
    find  : 'a -> int option;
  }

  (* takes a mapping from node to its root *)
  let create comparator comps =
    let roots,groups =
      Hashtbl.fold comps ~init:(Map.empty ~comparator)
        ~f:(fun ~key:node ~data:root map ->
            Map.add_multi map ~key:root ~data:node) |>
      Map.to_alist |> List.unzip in
    let roots = Array.of_list roots in
    let groups = Array.of_list_map groups ~f:(fun xs ->
        create_set (Set.of_list ~comparator xs)) in
    let {Comparator.compare} = comparator in
    let find_root x =
      Array.binary_search roots ~compare `First_equal_to x in
    let find x = Option.Monad_infix.(Hashtbl.find comps x >>= find_root) in
    {roots; groups; find}

  let nth_group t n = Group.create t.roots.(n) t.groups.(n) n
  let groups t = Seq.(range 0 (Array.length t.roots) >>| nth_group t)
  let group t x = Option.(t.find x >>| nth_group t)
  let equiv t x y = Option.equal Equiv.equal (t.find x) (t.find y)
  let number_of_groups t = Array.length t.roots
  let of_equiv t i =
    if i >= 0 && i < Array.length t.roots
    then Some (nth_group t i) else None
  let pp pp_elt ppf t =
    fprintf ppf "@;@[<v2>partition = {";
    Seq.iter (groups t) ~f:(fprintf ppf "@;%a" (Group.pp pp_elt));
    fprintf ppf "@]@;}"

  let () = Pretty_printer.register "Graphlib.Std.Partition.pp"
end

type 'a partition = 'a Partition.t
type 'a frontier = 'a Frontier.t

module To_ocamlgraph(G : Graph) = struct
  type t = G.t
  type edge = G.edge
  type vertex = G.node

  module V = G.Node

  module E = struct
    include G.Edge
    type vertex = G.node
    let create x y l = create x l y
  end

  let is_directed = G.is_directed

  let is_empty g = G.number_of_nodes g = 0

  let nb_vertex = G.number_of_nodes
  let nb_edges = G.number_of_edges

  let checked name f n g =
    if G.Node.mem n g then f n g
    else invalid_argf "%s: node is not in G" name ()

  let out_degree g n =
    checked "out_degree" (G.Node.degree ~dir:`Out) n g

  let in_degree g n =
    checked "in_degree" (G.Node.degree ~dir:`In) n g

  let mem_vertex g n = G.Node.mem n g
  let mem_edge g x y = G.Node.has_edge x y g

  let succ g n =
    checked "succ" G.Node.succs n g |> Seq.to_list_rev

  let pred g n =
    checked "pred" G.Node.preds n g |> Seq.to_list_rev

  let succ_e g n =
    checked "succ_e" G.Node.outputs n g |> Seq.to_list_rev

  let pred_e g n =
    checked "pred_e" G.Node.inputs n g |> Seq.to_list_rev

  let iter_vertex f g = G.nodes g |> Seq.iter ~f

  let iter_edges_e f g = G.edges g |> Seq.iter ~f
  let iter_succ f g n = G.Node.succs n g |> Seq.iter ~f
  let iter_pred f g n = G.Node.preds n g |> Seq.iter ~f

  let fold_vertex f g init =
    G.nodes g |> Seq.fold ~f:(fun init x -> f x init) ~init

  let iter_edges f g =
    G.edges g |> Seq.iter ~f:(fun e -> f (G.Edge.src e) (G.Edge.dst e))

  let fold_edges f g init =
    G.edges g |> Seq.fold ~init ~f:(fun init e ->
        f (G.Edge.src e) (G.Edge.dst e) init)

  let fold_edges_e f g init =
    G.edges g |> Seq.fold ~init ~f:(fun init e -> f e init)

  let map_vertex f g =
    G.nodes g |> Seq.fold ~init:G.empty ~f:(fun g v ->
        G.Node.insert (f v) g)

  let find_edge g x y = match G.Node.edge x y g with
    | Some e -> e
    | None -> raise Not_found

  let find_all_edges g x y = G.Node.edge x y g |> function
    | None -> []
    | Some x -> [x]

  let fold_succ f g n init =
    G.Node.succs n g |> Seq.fold ~init ~f:(fun a x -> f x a)

  let fold_pred f g n init =
    G.Node.preds n g |> Seq.fold ~init ~f:(fun a x -> f x a)

  let iter_succ_e f g n = G.Node.outputs n g |> Seq.iter ~f

  let iter_pred_e f g n = G.Node.inputs n g |> Seq.iter ~f

  let fold_succ_e f g n init =
    G.Node.outputs n g |> Seq.fold ~init ~f:(fun a x -> f x a)

  let fold_pred_e f g n init =
    G.Node.inputs n g |> Seq.fold ~init ~f:(fun a x -> f x a)

  let empty = G.empty
  let add_vertex g n = G.Node.insert n g
  let remove_vertex g n = G.Node.remove n g
  let add_edge g n =
    invalid_arg "add_edge operation is not supported"

  let add_edge_e g e = G.Edge.insert e g

  let remove_edge g x y = match G.Node.edge x y g with
    | None -> g
    | Some e -> G.Edge.remove e g

  let remove_edge_e g e = G.Edge.remove e g

  let mem_edge_e g e = G.Node.has_edge (G.Edge.src e) (G.Edge.dst e) g
end

let seq_of_fold fold g =
  try
    let open Seq.Generator in
    fold (fun v m -> m >>= fun () -> yield v) g (return ()) |> run
  with exn -> Seq.empty

module Of_ocamlgraph(G : Graph.Sig.P) = struct
  type t = G.t
  type node = G.V.t
  type edge = G.E.t

  let is_directed = G.is_directed

  let empty = G.empty

  let nodes = seq_of_fold G.fold_vertex
  let edges = seq_of_fold G.fold_edges_e

  let number_of_edges = G.nb_edges
  let number_of_nodes = G.nb_vertex

  module Node = struct
    type t = node
    type edge = G.E.t
    type graph = G.t
    type label = G.V.label

    let create = G.V.create
    let label = G.V.label
    let mem n g = G.mem_vertex g n
    let succs n g = seq_of_fold (fun f -> G.fold_succ f g) n
    let preds n g = seq_of_fold (fun f -> G.fold_pred f g) n
    let outputs n g = seq_of_fold (fun f -> G.fold_succ_e f g) n
    let inputs n g = seq_of_fold (fun f -> G.fold_pred_e f g) n
    let insert n g = G.add_vertex g n
    let update n l g =
      if G.mem_vertex g n
      then G.add_vertex (G.remove_vertex g n) (create l) else g
    let remove n g = G.remove_vertex g n
    let has_edge x y g = G.mem_edge g x y
    let edge x y g =
      try Some (G.find_edge g x y) with Not_found -> None

    let degree ?dir n g =
      try match dir with
        | None -> G.in_degree g n + G.out_degree g n
        | Some `In -> G.in_degree g n
        | Some `Out -> G.out_degree g n
      with exn -> 0

    include Opaque.Make(struct
        type t = node
        let hash = G.V.hash
        let compare = G.V.compare
        let version = "1.0.0"
      end)
  end

  module Edge = struct
    type t = edge
    type node = Node.t
    type graph = G.t
    type label = G.E.label

    let create x y l = G.E.create x l y
    let label = G.E.label
    let src = G.E.src
    let dst = G.E.dst
    let mem e g = G.mem_edge_e g e
    let insert e g = G.add_edge_e g e
    let update e l g =
      if G.mem_edge_e g e
      then
        let e' = (create (src e) (dst e) l) in
        G.add_edge_e (G.remove_edge_e g e) e'
      else g

    let remove e g = G.remove_edge_e g e

    include Opaque.Make(struct
        type t = edge
        let hash e =
          Hashtbl.hash (src e) lxor Hashtbl.hash (dst e)
        let compare x y = match Node.compare (src x) (src y) with
          | 0 -> Node.compare (dst x) (dst y)
          | n -> n
      end)
  end

  include Printable.Make(struct
      type nonrec t = t
      let module_name = None
      let version = "1.0.0"
      let pp ppf graph =
        let open Graphlib_pp in
        let string_of_node =
          by_natural_order symbols Node.compare
            (nodes graph) in
        Dot.pp_graph
          ~string_of_node
          ~nodes_of_edge:(fun e -> Edge.(src e, dst e))
          ~nodes:(nodes graph)
          ~edges:(edges graph)  ppf

    end)

  include Opaque.Make(struct
      type t = G.t
      let hash g =
        Seq.fold (edges g) ~init:0 ~f:(fun hash x ->
            hash lxor Edge.hash x)

      (* Note:
         The comparison function is rather inefficient here, since
         we can't rely that the order of iteration on edges and nodes
         will the same for otherwise equal graphs.

         It is usually the same for persistant graphs, built from
         maps. But in general we may not rely on this fact. So, we
         can't just compare using fold over set of edges and nodes,
         as in that case the following will not hold:

                  assert (remove n (insert n g) = g)
      *)

      let cmp (type t)
          (module C : Comparable with type t = t) enum x y =
        let set x = Seq.fold (enum x) ~f:Set.add ~init:C.Set.empty in
        C.Set.compare (set x) (set  y)

      (* complexity: O(e*log(e) + n*log(n)), where [e] is a number of
         edges and [n] is a number of nodes. See a note above for the
         clarification *)
      let compare x y =
        match cmp (module Edge) edges x y with
        | 0 -> cmp (module Node) nodes x y
        | n -> n
    end)
end

let create
    (type t) (type a) (type b)
    (module G : Graph with type t = t
                       and type Node.label = a
                       and type Edge.label = b)
    ?(nodes=[]) ?(edges=[]) () =
  let g =
    List.fold edges ~init:G.empty ~f:(fun g (src,dst,data) ->
        G.Edge.insert (G.Edge.create
                         (G.Node.create src)
                         (G.Node.create dst) data) g) in
  List.fold nodes ~init:g ~f:(fun g n -> G.Node.insert (G.Node.create n) g)



let compare (type e) (type g) (type n)
    (module E : Graph with type t = e and type node = n)
    (module G : Graph with type t = g and type node = n) e g =
  let module Edges = Set.Make(struct
      type t = E.Node.t * E.Node.t [@@deriving compare]
      let sexp_of_t = sexp_of_opaque
      let t_of_sexp = opaque_of_sexp
    end) in
  let module Nodes = E.Node.Set in
  let set empty x = Seq.fold x ~f:Set.add ~init:empty in
  let edges edges src dst empty g =
    edges g |> Seq.map ~f:(fun e -> src e, dst e) |> set empty in
  let e_edges = edges E.edges E.Edge.src E.Edge.dst Edges.empty e in
  let g_edges = edges G.edges G.Edge.src G.Edge.dst Edges.empty g in
  match Edges.compare e_edges g_edges with
  | 0 -> 0
  | n ->
    let ens = set Nodes.empty (E.nodes e) in
    let gns = set Nodes.empty (G.nodes g) in
    Nodes.compare ens gns

type number = {pre : int; rpost : int}

module Ordering(G : Graph) = struct
  type t = {
    numbers : number G.Node.Map.t;
    nodes_left : int;
    nodes_entered : int;
  }

  let init g = {
    numbers = G.Node.Map.empty;
    nodes_left = G.number_of_nodes g;
    nodes_entered = 0;
  }

  let enter t u = {
    t with
    nodes_entered = t.nodes_entered + 1;
    numbers = Map.change t.numbers u (fun _ -> Some {
        pre = t.nodes_entered;
        rpost = 0;
      })
  }, t.nodes_entered

  let leave t u = {
    t with
    nodes_left = t.nodes_left - 1;
    numbers = Map.change t.numbers u (function
        | None -> assert false
        | Some n -> Some {n with rpost = t.nodes_left - 1})
  }, t.nodes_left - 1

  let number t u = Map.find t.numbers u
end

let pass _ _ s = s

let depth_first_search
    (type g) (type n) (type e)
    (module G : Graph with type t = g
                       and type node = n
                       and type edge = e)
    ?(rev=false) ?start
    ?(start_tree=(fun _ s -> s))
    ?(enter_node=pass)
    ?(leave_node=pass)
    ?(enter_edge=pass)
    ?(leave_edge=pass) g ~init  =
  let module Order = Ordering(G) in
  let adj = if rev then G.Node.inputs else G.Node.outputs in
  let dst = if rev then G.Edge.src else G.Edge.dst in
  let tailrec = G.number_of_nodes g > 10000 in
  let rec visit ord u state k =
    let ord, pre = Order.enter ord u in
    let state = enter_node pre u state in
    let ord,state =
      adj u g |> Seq.fold ~init:(ord,state) ~f:(fun (ord,state) e ->
          let v = dst e in
          let kind = match Order.number ord v with
            | None -> `Tree
            | Some {rpost = 0} -> `Back
            | Some t -> if pre < t.pre then `Forward else `Cross in
          let finish (ord,state) = ord, leave_edge kind e state in
          let state = enter_edge kind e state in
          if tailrec
          then if kind = `Tree
            then visit ord v state (fun s -> k (finish s))
            else k (finish (ord,state))
          else if kind = `Tree
          then finish (visit ord v state k)
          else finish (ord,state)) in
    let ord,rpost = Order.leave ord u in
    ord, leave_node rpost u state in
  let ord  = Order.init g in
  let init = match start with
    | None -> ord,init
    | Some s -> if G.Node.mem s g
      then visit ord s (start_tree s init) ident else ord,init in
  G.nodes g |> Seq.fold ~init ~f:(fun (ord,state) u ->
      match Order.number ord u with
      | None -> visit ord u (start_tree u state) ident
      | _ -> ord,state) |> snd

let depth_first_visit graph ?rev ?start g ~init vis  =
  depth_first_search graph ?rev ?start g ~init
    ~start_tree:vis#start_tree
    ~enter_node:vis#enter_node
    ~leave_node:vis#leave_node
    ~enter_edge:vis#enter_edge
    ~leave_edge:vis#leave_edge

class ['n,'e,'s] dfs_identity_visitor : ['n,'e,'s] dfs_visitor =
  object
    method start_tree   _ s = s
    method enter_node _ _ s = s
    method leave_node _ _ s = s
    method enter_edge _ _ s = s
    method leave_edge _ _ s = s
  end

let union (type t) (type n) (type e)
    (module G : Graph with type t = t
                       and type node = n
                       and type edge = e) g1 g2 =

  depth_first_search (module G) ~init:g1 g2
    ~enter_node:(fun _ -> G.Node.insert)
    ~enter_edge:(fun _ -> G.Edge.insert)

let inter (type t) (type n) (type e)
    (module G : Graph with type t = t
                       and type node = n
                       and type edge = e) g1 g2 =
  let insert_edge other e g =
    if G.Edge.mem e other then G.Edge.insert e g else g in
  let insert_node other n g =
    if G.Node.mem n other then G.Node.insert n g else g in
  let insert g g1 g2 = depth_first_search (module G) g1 ~init:g
      ~enter_node:(fun _ -> insert_node g2)
      ~enter_edge:(fun _ -> insert_edge g2) in
  insert (insert G.empty g1 g2) g2 g1

let reverse_postorder_traverse graph ?rev ?start g =
  depth_first_search graph ?rev ?start g ~init:[]
    ~leave_node:(fun _ n ns -> n :: ns) |> Seq.of_list

let postorder_traverse graph ?rev ?start g =
  let open Seq.Generator in
  depth_first_search ?rev ?start graph g ~init:(return ())
    ~leave_node:(fun _ n ns -> ns >>= fun () -> yield n) |> run

let create_namer (type t) (type n)
    (module G : Graph with type t = t and type node = n) g =
  let namer =
    depth_first_search (module G) g ~init:G.Node.Map.empty
      ~leave_node:(fun rpost n names ->
          Map.add names ~key:n ~data:(Int.to_string rpost)) in
  Map.find_exn namer

let nil _ = []

let to_dot
    (type t) (type n) (type e)
    (module G : Graph with type t = t and type node = n
                                      and type edge = e)
    ?(graph_attrs=nil)
    ?(node_attrs=nil)
    ?(edge_attrs=nil)
    ?(string_of_node)
    ?(string_of_edge)
    ?channel ?formatter ?filename g  =
  let string_of_node = match string_of_node with
    | Some namer -> namer
    | None -> create_namer (module G) g in
  let module Dottable = struct
    module G = To_ocamlgraph(G)
    include G
    let graph_attributes = graph_attrs
    let default_vertex_attributes _ = []
    let vertex_name = string_of_node
    let vertex_attributes = node_attrs
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes e =
      let attrs = edge_attrs e in
      List.find attrs ~f:(function `Label _ -> true | _ -> false)
      |> function
      | Some _ -> attrs
      | None -> match string_of_edge with
        | None -> attrs
        | Some f -> `Label (f e) :: attrs

  end in
  let module Dot = Graph.Graphviz.Dot(Dottable) in
  Option.iter channel (fun chan -> Dot.output_graph chan g);
  Option.iter formatter (fun ppf -> Dot.fprint_graph ppf g);
  Option.iter filename (Out_channel.with_file ~f:(fun chan ->
      Dot.output_graph chan g))


(** Immediate dominators.
    This algorithm implements «A simple, fast dominance algorithm»
    [1], with some modifications, that allows it to run on arbitrary
    graphs.

    If provided with a graph and an entry node it will span only
    connected part of the graph, ignoring unreachable nodes. It will
    return a parent node as an immediate dominator of any unreachable
    node

    [1]: Cooper, Keith D., Timothy J. Harvey, and Ken Kennedy. "A simple,
    fast dominance algorithm." Software Practice & Experience 4 (2001):
    1-10.

*)
let idom (type t) (type n) (type e)
    (module G : Graph with type t = t
                       and type node = n
                       and type edge = e)
    ?(rev=false) g entry =
  let adj = if rev then G.Node.succs else G.Node.preds in
  let len = with_return (fun {return} ->
      depth_first_search ~rev (module G) g ~init:0 ~start:entry
        ~start_tree:(fun _ len -> if len <> 0 then return len else len)
        ~enter_node:(fun _ _ len -> len + 1)) in
  let node = Array.create ~len entry in
  let pnums = G.Node.Table.create ~size:len () in
  let doms = Array.create ~len ~-1 in
  let pnum = Hashtbl.find_exn pnums in
  if len > 0 then doms.(len - 1) <- len - 1;
  with_return (fun {return} ->
      depth_first_search ~rev (module G) g ~init:0 ~start:entry
        ~leave_node:(fun _ n i ->
            if i >= len then return ();
            node.(i) <- n;
            Hashtbl.set pnums ~key:n ~data:i;
            i + 1) |> (ignore : int -> _));
  let rec lift x y : int = match x,y with
    | -1,_ | _, -1 -> len - 1
    | _ -> if x < y then lift doms.(x) y else x in
  let rec intersect x y : int =
    if x = y then x else
      let x = lift x y in
      let y = lift y x in
      intersect x y in
  let rec loop () =
    Seq.range 1 len |> Seq.fold ~init:false ~f:(fun changed i ->
        let i = len - i - 1 in
        let new_idom =
          adj node.(i) g |>
          Seq.fold ~init:(-1) ~f:(fun new_idom p ->
              try (* unreachable predeccessors are invisible *)
                let pn = pnum p in
                if doms.(pn) < 0 then new_idom
                else if new_idom < 0 then pn
                else intersect new_idom pn
              with Not_found -> new_idom ) in
        let changed' = doms.(i) <> new_idom in
        if changed' then doms.(i) <- new_idom;
        changed' || changed) && loop () in
  loop () |> (ignore : bool -> _);
  `idom (fun n ->
      try
        let i = pnum n in
        if i <> len - 1 then Some node.(doms.(i)) else None
      with Not_found ->
        if G.Node.mem n g
        then Some node.(len - 1) else None)


let dominators (type t) (type n) (type e)
    (module G : Graph
      with type t = t
       and type edge = e
       and type node = n) ?rev g entry =
  let `idom parent = idom ?rev (module G) g entry in
  let init = G.nodes g |> Seq.fold ~init:G.Node.Map.empty ~f:(fun t n ->
      Map.add t ~key:n ~data:[]) in
  let children = G.nodes g |> Seq.fold ~init ~f:(fun tree n ->
      match parent n with
      | Some p -> Map.add_multi tree ~key:p ~data:n
      | None -> tree) |> Map.map ~f:G.Node.Set.of_list in
  create_tree parent children

module type Dom_frontier_algo = functor (G : Graph) -> sig
  val compute : ?rev:bool -> G.t -> G.node tree -> G.Node.Set.t G.Node.Map.t
end

module Dom_frontier_cooper(G:Graph) = struct
  let compute ?(rev=false) g tree =
    let adj = if rev then G.Node.succs else G.Node.preds in
    let idom = Tree.parent tree in
    let rec walk top dfs r =
      if Option.equal G.Node.equal (Some r) top then dfs
      else match idom r with
        | None   -> Set.add dfs r
        | Some p -> walk top (Set.add dfs r) p in
    G.nodes g |> Seq.fold ~init:G.Node.Map.empty ~f:(fun dfs n ->
        let adj = adj n g in
        let dom = idom n in
        Seq.fold adj ~init:G.Node.Set.empty ~f:(walk dom) |>
        Set.fold ~init:dfs ~f:(fun dfs visited ->
            Map.change dfs visited (function
                | None -> Some (G.Node.Set.singleton n)
                | Some set -> Some (Set.add set n))))
end

let dom_frontier_generic (type t) (type n) (type e)
    (module Algo : Dom_frontier_algo)
    (module G : Graph
      with type t = t
       and type node = n
       and type edge = e) ?rev g idom : n Frontier.t =
  let module Algo = Algo(G) in
  let frontier = Algo.compute ?rev g idom in
  let frontier = Map.map frontier ~f:create_set in
  Frontier.create (Map.keys frontier) (object
    method find = Map.find frontier
  end)

let dom_frontier g =
  dom_frontier_generic (module Dom_frontier_cooper) g

let strong_components
    (type t) (type n) (type e)
    (module G : Graph with type t = t
                       and type node = n
                       and type edge = e) g =
  let roots = G.Node.Table.create () in
  let comps = G.Node.Table.create ~size:(G.number_of_nodes g) () in
  let root = Hashtbl.find_exn roots in
  let spill_comp root stack =
    List.drop_while stack ~f:(fun c ->
        Hashtbl.add_exn comps ~key:c ~data:root;
        G.Node.(c <> root)) |> List.tl_exn in
  depth_first_search (module G) g ~init:[]
    ~enter_node:(fun t v stack ->
        Hashtbl.add_exn roots ~key:v ~data:(t,v); v :: stack)
    ~leave_node:(fun _ v stack ->
        G.Node.outputs v g |> Seq.iter ~f:(fun e ->
            let w = G.Edge.dst e in
            if not (Hashtbl.mem comps w) then
              let min x y = if fst x < fst y then x else y in
              let data = min (root v) (root w) in
              Hashtbl.change roots v (fun _ -> Some data));
        if G.Node.(snd (root v) = v)
        then spill_comp v stack else stack) |> function
  | [] -> Partition.create G.Node.comparator comps
  | _ -> assert false

module Path = struct
  type 'a t = {
    edges  : 'a array;
    weight : int;
  } [@@deriving bin_io, compare, sexp, fields]
  let create edges weight = {
    edges = Array.of_list_rev edges;
    weight;
  }
  let length t = Array.length t.edges
  let edges t = Seq.of_array t.edges
  let edges_rev t =
    let len = length t in
    Seq.range 0 len |> Seq.map ~f:(fun i -> t.edges.(len - i - 1))

  let start t = t.edges.(0)
  let finish t = t.edges.(length t - 1)

  let pp pp_elt ppf t =
    fprintf ppf "@[<2>{ %a" pp_elt t.edges.(0);
    Seq.range 1 (length t) |> Seq.iter  ~f:(fun i ->
        fprintf ppf ", %a" pp_elt t.edges.(i));
    fprintf ppf "}@]"
  let () = Pretty_printer.register "Graphlib.Std.Path.pp"
end

type 'a path = 'a Path.t

let shortest_path
    (type t) (type n) (type e)
    (module G : Graph with type t = t
                       and type node = n
                       and type edge = e)
    ?(weight=(fun _ -> 1)) ?(rev=false) g v1 v2 =
  let cmp (n,_,_) (m,_,_) = Int.compare n m in
  let adj = if rev then G.Node.inputs else G.Node.outputs in
  let dst = if rev then G.Edge.src else G.Edge.dst in
  let visited = G.Node.Hash_set.create () in
  let dist = G.Node.Table.create  () in
  let q = Heap.create ~cmp () in
  let rec loop () = match Heap.pop q with
    | None -> None
    | Some (w,v,p) when G.Node.equal v v2 -> Some (Path.create p w)
    | Some (w,v,p) ->
      if not (Hash_set.mem visited v) then update v w p;
      loop ()
  and update v w p =
    Hash_set.add visited v;
    adj v g |> Seq.iter ~f:(fun e ->
        let ev = dst e in
        let dev = w + weight e in
        match Hashtbl.find dist ev with
        | Some w when w < dev -> ()
        | _ ->
          Hashtbl.set dist ev dev;
          Heap.add q (dev, ev, e :: p)) in
  Heap.add q (0, v1, []);
  Hashtbl.set dist v1 0;
  loop ()

let is_reachable graph ?rev g u v =
  shortest_path graph ?rev g u v <> None

let fold_reachable (type t) (type n) (type e)
    (module G : Graph with type t = t
                       and type node = n
                       and type edge = e) ?rev ~init ~f g start =
  with_return (fun {return} ->
      depth_first_search (module G) g ?rev ~start ~init
        ~start_tree:(fun n s  ->
            if G.Node.(n = start) then s else return s)
        ~enter_node:(fun _ n u -> f u n))

module Filtered
    (G : Graph)
    (Has : Predicate with type edge = G.edge and type node = G.node) =
struct
  type t = G.t
  type node = G.node
  type edge = G.edge

  module Has = struct
    let node = Has.node
    let edge e =
      Has.edge e && node (G.Edge.dst e) && node (G.Edge.src e)
  end

  let (//) xs f = Seq.filter xs ~f
  let (/@) xs f = Seq.map xs ~f

  let empty = G.empty
  let is_directed = G.is_directed
  let nodes g = G.nodes g // Has.node
  let edges g = G.edges g // Has.edge
  let number_of_nodes g = Seq.length (nodes g)
  let number_of_edges g = Seq.length (edges g)


  module Node = struct
    include G.Node
    let mem n g = Has.node n && mem n g
    let enum enum n g = if Has.node n then enum n g else Seq.empty
    let inputs n g  = enum inputs n g  // Has.edge
    let outputs n g = enum outputs n g // Has.edge
    let succs n g   = outputs n g /@ G.Edge.dst
    let preds n g   = inputs n g  /@ G.Edge.src
    let edge n m g = match edge n m g with
      | Some e when Has.edge e -> Some e
      | _ -> None
    let has_edge n m g = match edge n m g with
      | None -> false
      | _ -> true

    let degree ?dir n g =
      let len = Seq.length in
      match dir with
      | None -> len (inputs n g) + len (outputs n g)
      | Some `In -> len (inputs n g)
      | Some `Out -> len (outputs n g)
  end


  module Edge = G.Edge

  include (G : Opaque.S with type t := t)
  include (G : Printable.S with type t := t)
end


let filtered (type t) (type n) (type e)
    (module G : Graph with type t = t
                       and type node = n
                       and type edge = e)
    ?(skip_node=fun _ -> false)
    ?(skip_edge=fun _ -> false) () :
  (module Graph with type t = t
                 and type node = n
                 and type edge = e) =
  let module R = Filtered(G)(struct
      type edge = e
      type node = n
      let edge e = not (skip_edge e)
      let node n = not (skip_node n)
    end) in
  (module R)

module type Morph = Isomorphism

module Mapper
    (G  : Graph)
    (N  : Morph with type s = G.node)
    (E  : Morph with type s = G.edge)
    (NL : Morph with type s = G.Node.label)
    (EL : Morph with type s = G.Edge.label)
= struct
  type t = G.t
  type node = N.t
  type edge = E.t

  let (/@) xs f = Seq.map xs ~f

  let empty = G.empty
  let is_directed = G.is_directed
  let nodes g = G.nodes g /@ N.forward
  let edges g = G.edges g /@ E.forward

  let number_of_nodes = G.number_of_nodes
  let number_of_edges = G.number_of_edges

  module Node = struct
    type t = node
    type edge = E.t
    type label = NL.t
    type graph = G.t
    open G.Node
    let create lab = N.forward (create (NL.backward lab))
    let label n = NL.forward (label (N.backward n))
    let mem n g = mem (N.backward n) g
    let succs n g = succs (N.backward n) g /@ N.forward
    let preds n g = preds (N.backward n) g /@ N.forward
    let inputs n g = inputs (N.backward n) g /@ E.forward
    let outputs n g = outputs (N.backward n) g /@ E.forward
    let insert n g = insert (N.backward n) g
    let remove n g = remove (N.backward n) g
    let update n l g = update (N.backward n) (NL.backward l) g
    let has_edge n m g = has_edge (N.backward n) (N.backward m) g
    let edge n m g =
      Option.(edge (N.backward n) (N.backward m) g >>| E.forward)
    let degree ?dir n g = degree ?dir (N.backward n) g
    include Opaque.Make(struct
        type t = node
        let compare x y = G.Node.compare (N.backward x) (N.backward y)
        let hash x = hash (N.backward x)
      end)
  end

  module Edge = struct
    type t = edge
    type node = N.t
    type label = EL.t
    type graph = G.t

    open G.Edge

    let create n m l =
      create (N.backward n) (N.backward m) (EL.backward l) |> E.forward
    let label e = label (E.backward e) |> EL.forward
    let src e = src (E.backward e) |> N.forward
    let dst e = dst (E.backward e) |> N.forward
    let mem e g = mem (E.backward e) g
    let insert e g = insert (E.backward e) g
    let update e l g = update (E.backward e) (EL.backward l) g
    let remove e g = remove (E.backward e) g
    include Opaque.Make(struct
        type t = edge
        let compare x y =
          G.Edge.compare (E.backward x) (E.backward y)
        let hash x = hash (E.backward x)
      end)
  end

  include (G : Opaque.S with type t := t)
  include (G : Printable.S with type t := t)
end


let view (type t)
    (type n) (type e) (type a) (type b)
    (type m) (type f) (type c) (type d)
    (module G : Graph with type t = t
                       and type node = n
                       and type edge = e
                       and type Node.label = a
                       and type Edge.label = b)
    ~node:(nf,nb) ~edge:(ef,eb)
    ~node_label:(nlf,nlb)
    ~edge_label:(elf,elb) :
  (module Graph with type t = t
                 and type node = m
                 and type edge = f
                 and type Node.label = c
                 and type Edge.label = d) =
  let module N = struct
    type s = n
    type t = m
    let forward = nf
    let backward = nb
  end in
  let module E = struct
    type s = e
    type t = f
    let forward = ef
    let backward = eb
  end in
  let module NL = struct
    type s = a
    type t = c
    let forward = nlf
    let backward = nlb
  end in
  let module EL = struct
    type s = b
    type t = d
    let forward = elf
    let backward = elb
  end in
  let module M = Mapper(G)(N)(E)(NL)(EL) in
  (module M)


module Fixpoint = struct
  type ('n,'d) t = Solution : {
      steps : int option;
      iters : int;
      init : 'd;
      approx : ('n,'d,_) Map.t;
    } -> ('n,'d) t


  let create constraints init = Solution {
      steps=Some 0; iters=0;
      approx=constraints;
      init;
    }

  let iterations (Solution {iters}) = iters

  let get (Solution {approx; init}) n =
    match Map.find approx n with
    | None -> init
    | Some x -> x

  let is_fixpoint (Solution {steps; iters}) = match steps with
    | None -> iters > 0
    | Some steps -> iters < steps

  type ('a,'b) step =
    | Step of 'a
    | Done of 'b

  let continue x = Step x


  (* Kildall's Worklist Algorithm Implementation


     Kildall's Algorithm
     ===================

     Pseudocode:

     Given a set of node W, and a finite mapping A from nodes to
     approximations, a function F, the initial approximation I, and a
     start node B, the algorithm refines the mapping A, until a
     fixpoint is reached.


     {v
     let W = {B}
     for each node N in graph G:
        A[N] := I

     while W <> {}:
        pop a node N from W
        let OUT = F N A[N]
        for each successor S of N:
           let IN = A[S] /\ OUT
           if IN <> A[S]:
              A[S] := IN
              W := union(W,{S})
        end
     end
     v}


     If the meet operation (/\) induces a partial order over the set
     of approximations, and function F is monotonic, then the result
     would me the maximal fixpoint solution.


     Implementation
     ==============

     1. We do not distinguish between forward and backward problems,
     since a backward problem can be expressed as forward, on the
     reversed graph and inversed lattice. Thus, we express our
     algorithm as a forward problem with a meet semilattice. We do not
     require, of course, a user to provide a reverse graph, instead the
     flag [rev] could be used to virtually reverse the graph, and
     the exit node should be provided as a start node.

     2. Since the algorithm converges faster if a worklist is
     traversed in the reverse postorder we rank the graph nodes with
     their reverse postorder (rpost) numbers and use an array [nodes]
     for fast mapping from rpost numbers to nodes. We then represent
     the worklist as an integer set, and always pick the minimal
     element from the worklist.

     3. We also precompute a set of successors [succs] (as a set of
     their rpost numbers) for each node.

     4. In the loop body we use rpost numbers as node representations,
     and the finite mapping A is a mapping from integers to
     approximations.

     5. We optionally bound our loop with the maximum number of
     iterations, allowing an algorithm to terminate before it
     converges. Thus the result might be not a maximal fixpoint (i.e.,
     it might not be the greater lower bound for some if not all
     nodes, however, it should still be the over-approximation, given
     the correct meet, f,  and initial approximation.


     Caveats
     =======

     1. We allow only one start node. If it is specified, then only
     those nodes that are reachable from the start node will
     participate in the computation. So, unless, it is really desired,
     it is a good idea to introduce an artificial entry node, from
     which everything is reachable.

  *)
  let compute (type g n d)
      (module G : Graph with type t = g and type node = n)
      ?steps ?start ?(rev=false) ?step
      ~init:(Solution {approx; iters; init}) ~equal ~merge ~f g : (n,d) t =
    let nodes =
      reverse_postorder_traverse (module G) ~rev ?start g |>
      Sequence.to_array in
    let rnodes =
      Array.foldi nodes ~init:G.Node.Map.empty ~f:(fun i rnodes n ->
          Map.add rnodes ~key:n ~data:i) in
    let succs = Array.map nodes ~f:(fun n ->
        let succs = if rev then G.Node.preds else G.Node.succs in
        succs n g |> Sequence.fold ~init:Int.Set.empty ~f:(fun ns n ->
            match Map.find rnodes n with
            | None -> ns
            | Some i -> Set.add ns i)) in
    let step = match step with
      | None -> fun visits _ _ x -> visits,x
      | Some step -> fun visits n x x' ->
        let i = match Map.find visits n with
          | None -> 1
          | Some x -> x in
        let visits = Map.add visits ~key:n ~data:i in
        visits, step i nodes.(n) x x' in
    let get approx n : d = match Map.find approx n with
      | Some x -> x
      | None -> init in
    let step visits works approx = match Set.min_elt works with
      | None -> Done approx
      | Some n ->
        let works = Set.remove works n in
        let out = f nodes.(n) (get approx n) in
        succs.(n) |>
        Set.fold ~init:(visits,works,approx)
          ~f:(fun (visits,works,approx) n ->
              let ap = get approx n in
              let ap' = merge (out : d) (ap : d) in
              let visits,ap' = step visits n ap ap' in
              if equal ap ap' then (visits,works,approx)
              else visits,
                   Set.add works n,
                   Map.add approx ~key:n ~data:ap') |>
        continue in
    let can_iter iters = match steps with
      | None -> true
      | Some steps -> iters < steps in
    let make_solution iters approx = Solution {
        steps;
        iters;
        init;
        approx = Map.fold approx ~init:G.Node.Map.empty
            ~f:(fun ~key:n ~data approx ->
                Map.add approx ~key:nodes.(n) ~data);
      } in
    let rec loop visits iters works approx =
      if can_iter iters then match step visits works approx with
        | Done approx -> make_solution iters approx
        | Step (visits,works,approx) -> loop visits (iters+1) works approx
      else make_solution iters approx in
    let works = List.init (Array.length nodes) ident in
    let approx = Map.fold approx ~init:Int.Map.empty
        ~f:(fun ~key:node ~data approx ->
            match Map.find rnodes node with
            | None -> approx
            | Some n -> Map.add approx ~key:n ~data) in
    loop Int.Map.empty iters (Int.Set.of_list works) approx
end

let fixpoint = Fixpoint.compute
