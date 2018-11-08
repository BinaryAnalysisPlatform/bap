(*

  This tests are based on Theorems and Lemmas from [1].


  [1]: Offner, Carl D. "Notes on graph algorithms used in optimizing
  compilers." Notes from University of Massachusetts, Boston (2013).

 *)

open Core_kernel.Std
open Graphlib.Std
open Bap.Std
open OUnit2
open Format

(** required interface for functor testing different algorithm  *)
module type Graph_for_algo = Graph
  with type Node.label = int
   and type Edge.label = unit

module Test_algo(Gl : Graph_for_algo) = struct
  module G = Graphlib.To_ocamlgraph(Gl)
  module Rand = Graph.Rand.P(G)
  module Dom = Graph.Dominator.Make(G)

  module Node = Gl.Node
  module Edge = Gl.Edge

  let node = Gl.Node.create
  let edge x y = Gl.Edge.create (node x) (node y) ()
  let node_printer x = sprintf "%d" @@ Gl.Node.label x


  type node_info = {
    enter : int;
    leave : int;
    rpost : int;
    pre   : int;
  }

  let empty = {
    enter = -1;
    leave = -1;
    rpost = -1;
    pre   = -1;
  }

  (* collects timestamps (time of entry, and time of leaving of a node,
     as well as preorder and reverse post order ordinals.  Lemma 1.2 is
     checked in a process of collecting. Also, some asserts based on
     definitions are also checked.  @return total time, spent in
     traversing, and a mapping from nodes to their numbering info.
     Note: time is incremented by any of the following events:
     enter_node, leave_node. Edges events are ignored. *)
  let timestamps ty gr : int * ('n,node_info,_) Map.t =
    Graphlib.depth_first_search ty gr ~init:(0,Node.Map.empty)
      ~enter_node:(fun pre node (time,stamps) ->
          time + 1, Map.change stamps node ~f:(function
              | None -> Some {empty with pre; enter = time}
              | _ -> assert_failure "Node was entered several times"))
      ~leave_node:(fun rpost node (time,stamps) ->
          time + 1, Map.change stamps node ~f:(function
              | None -> assert_failure "Node was left without entering"
              | Some info ->
                assert_bool "Lemma 1.2: entry[x] < leave[x]" @@
                (info.enter < time);
                Some {info with rpost; leave = time}))

  module Span = struct
    type t = {
      children : Node.Set.t Node.Map.t;
      iparents : Node.t Node.Map.t;
    }

    let empty = {
      children = Node.Map.empty;
      iparents = Node.Map.empty;
    }

    let add_relation t ~parent ~child = {
      children = Map.change t.children parent( function
          | None -> Some (Node.Set.singleton child)
          | Some cs when Set.mem cs child ->
            assert_failure "Child was already adopted"
          | Some cs -> Some (Set.add cs child));
      iparents = Map.change t.iparents child ~f:(function
          | None -> Some parent
          | Some parent -> assert_failure "Child has more than one parent")
    }

    let children t parent = match Map.find t.children parent with
      | None -> Node.Set.empty
      | Some children -> children

    let parent t child = Map.find t.iparents child

    (* computes a set of all descendands of a given parent
       (transitive closure) *)
    let rec descendants t parent =
      (* according to [1] each node is descendant of itself. *)
      let init = Node.Set.singleton parent in
      Set.fold (children t parent) ~init ~f:(fun ds parent ->
          Set.union ds (descendants t parent))

    (* computes a set of all ancestors. *)
    let ancestors t child =
      let rec loop parents child = match parent t child with
        | None -> parents
        | Some parent -> loop (Set.add parents parent) parent in
      loop Node.Set.empty child
  end

  let build_tree g =
    Graphlib.depth_first_search (module Gl) g ~init:Span.empty
      ~enter_edge:(fun kind edge tree -> match kind with
          | `Tree -> Span.add_relation tree
                       ~parent:(Edge.src edge)
                       ~child:(Edge.dst edge)
          | _ -> tree)

  let lemma_1_3 stamps ctxt : bool =
    let is_sorted = List.is_sorted ~compare:Int.compare in
    let nodes = Map.to_sequence stamps in
    Seq.cartesian_product nodes nodes |>
    Seq.for_all ~f:(fun ((_,x),(_,y)) ->
        is_sorted [x.enter; x.leave; y.enter; y.leave] ||
        is_sorted [x.enter; y.enter; y.leave; x.leave] ||
        is_sorted [y.enter; x.enter; x.leave; y.leave] ||
        is_sorted [y.enter; y.leave; x.enter; x.leave])


  (** Lemma 1.4:
      pre[x] < pre[y] && rpost[x] < rpost[y] <=> x `is_ancestor_of` y *)
  let lemma_1_4 stamps tree ctxt : bool =
    let prop x y = x.pre < y.pre && x.rpost < y.rpost in
    let nodes = Map.to_sequence stamps in
    Seq.cartesian_product nodes nodes |>
    Seq.for_all ~f:(fun ((nx,x),(ny,y)) ->
        prop x y ==> Set.mem (Span.ancestors tree ny) nx)
    &&
    Seq.for_all nodes ~f:(fun (ny,y) ->
        Set.for_all (Span.ancestors tree ny) ~f:(fun nx ->
            match Map.find stamps nx with
            | None -> assert_failure "unstamped node"
            | Some x -> prop x y))

  let theorem_1_5 stamps graph ctxt : bool =
    Graphlib.depth_first_search (module Gl) graph ~init:true
      ~enter_edge:(fun kind e -> function
          | false -> false
          | true ->
            let x = match Map.find stamps (Gl.Edge.src e) with
              | None -> assert_failure "edge with unstamped src"
              | Some x -> x in
            let y = match Map.find stamps (Gl.Edge.dst e) with
              | None -> assert_failure "edge with unstamped dst"
              | Some y -> y in
            match kind with
            | `Back    -> x.pre >= y.pre && x.rpost >= y.rpost
            | `Tree    -> x.pre < y.pre && x.rpost < y.rpost
            | `Cross   -> x.pre > y.pre && x.rpost < y.rpost
            | `Forward -> x.pre < y.pre && x.rpost < y.rpost)


  let test g =
    let time,stamps = timestamps (module Gl) g in
    let tree = build_tree g in
    assert_bool "Total time = |V|/2" (time / 2 = Gl.number_of_nodes g);
    let test prep ctxt = assert_bool "doesn't hold" (prep ctxt) in
    [
      "L 1.3" >:: test @@ lemma_1_3 stamps;
      "L 1.4" >:: test @@ lemma_1_4 stamps tree;
      "T 1.5" >:: test @@ theorem_1_5 stamps g
    ]

  let random_graph () =
    let v = Random.int 500 + 1 in
    let e = v * 3 / 2 in
    Rand.labeled (fun _ _ -> ()) ~loops:true ~e ~v ()

  let randoms =
    List.init 100 ~f:(fun n ->
        sprintf "Graph.%d" n >::: test @@ random_graph ())

  let random_flowgraph size =
    let g =
      Seq.range 0 size |> Seq.fold ~init:Gl.empty ~f:(fun g i ->
          Gl.Node.insert (node i) g) in
    let g =
      Seq.range 0 (size-1) |> Seq.fold ~init:g ~f:(fun g i ->
          Gl.Edge.insert (edge i (i+1)) g) in
    Seq.range 0 size |> Seq.fold ~init:g ~f:(fun g i ->
        if Random.float 1.0 < 0.3 then
          let dst =
            if Random.float 1.0 < 0.2
            then Random.int (i + 1)
            else i + Random.int (size - i) in
          Gl.Edge.insert (edge i dst) g
        else g)


  let compare_dom gr ctxt =
    let size = Gl.number_of_nodes gr in
    let exp_idom = Dom.compute_idom gr (node 0) in
    let dom_tree = Graphlib.dominators (module Gl) gr (node 0) in
    let got_idom n = match Tree.parent dom_tree n with
      | None -> assert_failure "child has no parents"
      | Some p -> p in
    Seq.range 1 size |> Seq.map ~f:node |> Seq.iter ~f:(fun n ->
        assert_equal ~ctxt ~printer:node_printer
          (exp_idom n) (got_idom n))

  let compare_dom_frontier gr ctxt =
    let idom = Graphlib.dominators (module Gl) gr (node 0) in
    let dfrt =
      Graphlib.dom_frontier (module Gl) gr idom in
    let exp_idom = Dom.compute_idom gr (node 0) in
    let dom_tree = Dom.idom_to_dom_tree gr exp_idom in
    let exp_dfrt = Dom.compute_dom_frontier gr dom_tree exp_idom in
    let set_of_frontier n = Frontier.enum dfrt n |>
                            Seq.to_list_rev in
    Seq.range 0 (Gl.number_of_nodes gr) |>
    Seq.map ~f:node |>  Seq.iter ~f:(fun n ->
        let exp = Node.Set.of_list (exp_dfrt n) in
        let got = Node.Set.of_list (set_of_frontier n) in
        assert_equal ~ctxt  ~cmp:Node.Set.equal exp got)

  let compare_scc gr ctxt =
    let scc = Graphlib.strong_components (module Gl) gr in
    let module SCC = Graph.Components.Make(G) in
    let _,comp_num = SCC.scc gr in
    let our_equiv = Partition.equiv scc in
    let exp_equiv x y = comp_num x = comp_num y in
    Seq.cartesian_product (Gl.nodes gr) (Gl.nodes gr) |>
    Seq.iter ~f:(fun (x,y) ->
        assert_equal ~ctxt  ~printer:string_of_bool
          (exp_equiv x y) (our_equiv x y))


  let doms = List.concat @@ List.init 100 ~f:(fun n ->
      let size = 1 + Random.int 200 in
      let gr = random_flowgraph size in
      [
        sprintf "Dom.%d" n >:: compare_dom gr;
        sprintf "DomF.%d" n >:: compare_dom_frontier gr;
        sprintf "Scc.%d" n >:: compare_scc gr;
      ])

  let nodes g =
    Gl.nodes g |> Seq.fold ~init:Gl.Node.Set.empty ~f:Set.add

  let edges g =
    Gl.edges g |> Seq.fold ~init:Gl.Edge.Set.empty ~f:Set.add

  let equal_sets s1 s2 ctxt =
    assert_equal ~ctxt ~cmp:Set.equal s1 s2

  let setops = List.concat @@ List.init 100 ~f:(fun n ->
      let g1 = random_graph () in
      let g2 = random_graph () in
      let is elems op check ctxt =
        let g = op g1 g2 in
        let ns,n1,n2 = elems g, elems g1, elems g2 in
        assert_equal ~ctxt ~cmp:Set.equal ns (check n1 n2) in
      let union = Graphlib.union (module Gl) in
      let inter = Graphlib.inter (module Gl) in
      [
        sprintf "Union.%d.nodes" n >:: is nodes union Set.union;
        sprintf "Union.%d.edges" n >:: is edges union Set.union;
        sprintf "Inter.%d.nodes" n >:: is nodes inter Set.inter;
        sprintf "Inter.%d.edges" n >:: is edges inter Set.inter;
      ])

  let suite name =
    name >::: [
      "Random Graphs" >::: randoms;
      "Random Flow Graphs" >::: doms;
      "Random Set ops" >::: setops
    ]
end


module type Factory = sig
  type t
  val create : unit -> t
  module E : Graph with type node = t and type Edge.label = unit
  module G : Graph with type node = t and type Edge.label = unit
end


module Construction(Factory : Factory) = struct
  open Factory
  type graphs = E.t * G.t


  (* we're using comparison function from G module as we do not trust
     in the E module (it may be from ocamlgraph using the polymorphic
     compare function) *)
  module Nodes = G.Node.Set
  module Edges = Set.Make(struct
      type t = G.Node.t * G.Node.t * G.Node.t [@@deriving compare]
      let sexp_of_t = sexp_of_opaque
      let t_of_sexp = opaque_of_sexp
    end)

  let make_edges (type g)
      (module G : Graph with type t = g
                         and type node = t
                         and type Edge.label = unit) g =
    G.edges g |> Seq.map ~f:(fun e ->
        G.Edge.src e, G.Edge.dst e, G.Edge.src e) |>
    Seq.fold ~init:Edges.empty ~f:Set.add

  let compare e g : int =
    let ees = make_edges (module E) e in
    let ges = make_edges (module G) g in
    match Edges.compare ees ges with
    | 0 -> 0
    | n ->
      let set enum g =
        enum g |> Seq.fold ~init:Nodes.empty ~f:Set.add in
      Nodes.compare (set E.nodes e) (set G.nodes g)

  let random_elt number enum e =
    let n = number e in
    if n > 0 then Seq.nth (enum e) (Random.int n)
    else None

  let random_node = random_elt E.number_of_nodes E.nodes
  let random_edge = random_elt E.number_of_edges E.edges

  let insert_edge (e,g) : graphs =
    let src,dst = Factory.(create (), create ()) in
    E.Edge.insert (E.Edge.create src dst ()) e,
    G.Edge.insert (G.Edge.create src dst ()) g

  let insert_node (e,g) : graphs =
    let x = Factory.create () in
    E.Node.insert x e,
    G.Node.insert x g

  let remove_node (e,g) : graphs =
    match random_node e with
    | None -> (e,g)
    | Some n -> E.Node.remove n e, G.Node.remove n g

  let remove_edge (e,g) : graphs =
    match random_edge e with
    | None -> (e,g)
    | Some edge ->
      let src = E.Edge.src edge in
      let dst = E.Edge.dst edge in
      let lab = E.Edge.label edge in
      E.Edge.remove (E.Edge.create src dst lab) e,
      G.Edge.remove (G.Edge.create src dst lab) g

  let constructives = [insert_node; insert_edge]
  let destructives  = [remove_node; remove_edge]

  let constructive_scheme : (graphs -> graphs) list = List.concat [
      constructives; constructives; destructives
    ]

  let validate (e,g) ctxt =
    if compare e g <> 0 then
      eprintf "expected:\n%a\ngot:\n%a\n%!" E.pp e G.pp g;
    assert_bool "structures differ" (compare e g = 0)

  let run (scheme : (graphs -> graphs) list) length ctxt =
    let scheme = Array.of_list scheme in
    let nextop g = scheme.(Random.int (Array.length scheme)) g in
    let rec loop g = function
      | 0 -> g
      | n ->
        validate g ctxt;
        loop (nextop g) (n-1) in
    loop (E.empty, G.empty) length |> ignore

  let suite : test =
    "constructive" >:: run constructive_scheme 1000
end


module ODIU =
  Graphlib.Of_ocamlgraph(Graph.Persistent.Digraph.Concrete(Int))

module OBIU =
  Graphlib.Of_ocamlgraph
    (Graph.Persistent.Digraph.ConcreteBidirectional(Int))

module OBII =
  Graphlib.Of_ocamlgraph
    (Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
       (Int)(struct include Int let default = 0 end))

module OBSS =
  Graphlib.Of_ocamlgraph
    (Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
       (Tid)(struct include Tid
       let default = Tid.create () end))

module Intu = Graphlib.Make(Int)(Unit)

let graphs_for_algo : (module Graph_for_algo) list = [
  (module ODIU);
  (module OBIU);
  (module Intu);
]

module Int100 : Factory = struct
  type t = int
  let create () = Random.int 100
  module E = OBIU
  module G = Intu
end

module Test_IR = struct
  module G = Graphs.Ir

  let entry = Blk.create ()
  let b1 = Blk.create ()
  let b2 = Blk.create ()
  let b3 = Blk.create ()
  let b4 = Blk.create ()
  let b5 = Blk.create ()
  let b6 = Blk.create ()
  let exit = Blk.create ()

  let i = Var.create "i" reg32_t
  let j = Var.create "j" reg32_t
  let k = Var.create "k" bool_t

  let _1 = Bil.int (Word.one 32)
  let _2 = Bil.int (Word.of_int32 2l)
  let _F = Bil.int Word.b0
  let _T = Bil.int Word.b1

  let def var exp b = Term.append def_t b @@ Def.create var exp
  let cond blk cond t f =
    let jt = Jmp.create_goto ~cond (Label.direct (Term.tid t)) in
    let jf = Jmp.create_goto (Label.direct (Term.tid f)) in
    let blk = Term.append jmp_t blk jt in
    Term.append jmp_t blk jf
  let goto dst src = Term.append jmp_t src @@
    Jmp.create_goto (Label.direct (Term.tid dst))

  let entry = entry |>
              goto b1

  let b1 = b1       |>
           def k _F |>
           def i _1 |>
           def j _2 |>
           goto b2

  let b2 = cond b2 Bil.(var i <= var j) b3 b4

  let b3 = b3                     |>
           def j Bil.(var j * _2) |>
           def k _T               |>
           def i Bil.(var i + _1) |>
           goto b2

  let b4 = cond b4 Bil.(var k) b5 b6


  let b5 =
    let call = Call.create ()
        ~return:(Label.direct (Term.tid exit))
        ~target:(Label.indirect (Bil.var j)) in
    let use = Jmp.create_call call in
    Term.append jmp_t b5 use


  let b6 = def i Bil.(var i + _1) b6 |> goto exit

  let sub_of_blks blks =
    let sub = Sub.create ~name:"example" () in
    List.fold blks ~init:sub ~f:(Term.append blk_t)

  let blks = [entry; b1; b2; b3; b4; b5; b6; exit]

  let sub = sub_of_blks blks

  let etalon = Sub.to_cfg sub

  let insert blks =
    let g = List.fold blks ~init:G.empty ~f:(fun g blk ->
        G.Node.insert (G.Node.create blk) g) in
    let etalon = G.edges g |> Seq.fold ~init:etalon ~f:(fun etalon e ->
        G.Edge.remove e etalon) in
    [
      "all edges inserted" >::(fun ctxt ->
          assert_equal ~ctxt ~cmp:Int.equal ~printer:Int.to_string
            0 (G.number_of_edges etalon));
      "all nodes connected" >:: fun ctxt ->
        assert_bool "doesn't hold" @@
        Seq.for_all (G.nodes g) ~f:(fun n -> G.Node.degree n g <> 0)
    ]


  let insert_randomly =
    List.(range 0 100 >>= fun _ -> insert (List.permute blks))

  let update_labeled ctxt =
    let module G = Graphlib.Labeled(Int)(String)(Unit) in
    let n1 = {node = 1; node_label = "1"} in
    let n2 = {node = 2; node_label = "2"} in
    let s1 = {node = 1; node_label = "one"} in
    let s2 = {node = 2; node_label = "two"} in
    let g1 = Graphlib.create (module G) ~edges:[n1,n2,()] () in
    let g2 = G.Node.update n1 s1 g1 in
    let g3 = G.Node.update n2 s2 g2 in
    let ps = G.Node.preds s2 g3 in
    let ss = G.Node.succs s1 g3 in
    match Seq.to_list ps @ Seq.to_list ss with
    | [{node_label="one"}; {node_label="two"}] -> ()
    | [{node_label="one"}; {node_label=l}] -> assert_failure "bad out"
    | [{node_label=l}; {node_label="two"}] -> assert_failure "bad inc"
    | [] | [_] -> assert_failure "egde(n1,n2) doesn't exist"
    | _ -> assert_failure "bad out and inc"

  let (++) g x = G.Node.(insert (create x) g)
  let (--) g x = G.Node.(remove (create x) g)
  let has = ident
  let hasn't = not
  let nil = G.empty

  let edges n g ctxt =
    assert_bool "failed" (G.number_of_edges g = n)

  let nodes n g ctxt =
    assert_bool "failed" (G.number_of_nodes g = n)

  let edge has g x y ctxt =
    assert_bool "failed"
      (has (G.Node.(has_edge (create x) (create y) g)))

  let suite () = [
    "random insert" >::: insert_randomly;
    "[entry] has no edges" >:: edges 0 (nil ++ entry);
    "[b1] has no edges" >:: edges 0 (nil ++ b1);
    "[entry] has one block" >:: nodes 1 (nil ++ entry);
    "[b1] has one block" >:: nodes 1 (nil ++ b1);
    "b2 -> b3 in [b2;b3]" >:: edge has (nil ++ b2 ++ b3) b2 b3;
    "b3 -> b2 in [b2;b3]" >:: edge has (nil ++ b2 ++ b3) b3 b2;
    "[b2] has no edges" >:: edges 0 (nil ++ b2 ++ b3 -- b3);
    "[b2] has one block" >:: nodes 1 (nil ++ b2 ++ b3 -- b3);
    "no b2 -> b3 in []" >:: edge hasn't (nil ++ b2 ++ b3 -- b2 -- b3) b2 b3;
    "no b3 <- b2 in []" >:: edge hasn't (nil ++ b2 ++ b3 -- b2 -- b3) b3 b2;
    "b2 -> b3 in [b2-b3+b3]" >:: edge has (nil ++ b2 -- b3 ++ b3) b2 b3;
    "b3 -> b2 in [b2-b3+b3]" >:: edge has (nil ++ b2 -- b3 ++ b3) b3 b2;
    "b2 -> b3 in [b2+b3+b3]" >:: edge has (nil ++ b2 ++ b3 ++ b3) b2 b3;
    "b3 -> b2 in [b2+b3+b3]" >:: edge has (nil ++ b2 ++ b3 ++ b3) b3 b2;
    "2 edges in [b2+b3+b3]" >:: edges 2 (nil ++ b2 ++ b3 ++ b3);
    "2 edges in [b2-b3+b3]" >:: edges 2 (nil ++ b2 -- b3 ++ b3);
    "2 nodes in [b2+b3+b3]" >:: nodes 2 (nil ++ b2 ++ b3 ++ b3);
    "2 nodes in [b2-b3+b3]" >:: nodes 2 (nil ++ b2 -- b3 ++ b3);
    "update_labeled " >:: update_labeled
  ]

end

module Test_int100 = Construction(Int100)

module Test_partition = struct

  module P = Partition

  let add x s = Set.add s x

  let s = Set.empty Int.comparator
          |> add 0
          |> add 1
          |> add 2
          |> add 3
          |> add 4
          |> add 5
          |> add 6
          |> add 7
          |> add 8
          |> add 9
          |> add 10

  let n = Set.length s

  let trivial p _ = assert_bool "failed" (P.number_of_groups p = 1)

  let discrete p _ = assert_bool "failed" (P.number_of_groups p = n)

  let union p x y _ = assert_bool "failed" (P.equiv p x y)

  let refine p equiv _ = assert_bool "failed"
      (Seq.for_all (P.groups p)
         ~f:(fun g ->
             let x = Group.top g in
             Seq.for_all (Group.enum g) ~f:(fun y -> equiv x y)))

  let equiv x y = x - y mod 2 = 0

  let cmp x y = x - y

  let suite () = [
    "Trivial invariant" >:: trivial (P.trivial s);
    "Discrete invariant" >:: discrete (P.discrete s);
    "Union invariant" >:: union (P.union (P.discrete s) 1 2) 1 2;
    "Refine invariant" >:: refine (P.refine (P.trivial s) equiv cmp) equiv
  ]
end

let suite () =
  "Graph" >::: [
    "Algo" >:::
    List.mapi graphs_for_algo ~f:(fun n (module G) ->
        let module Test = Test_algo(G) in
        Test.suite (sprintf "%d" n));
    "Construction" >::: [Test_int100.suite];
    "IR" >::: Test_IR.suite ();
    "Partition" >::: Test_partition.suite ()
  ]
