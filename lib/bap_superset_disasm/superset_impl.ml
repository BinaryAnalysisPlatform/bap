open Bap.Std
open Regular.Std
open Core
open Or_error
open Graphlib.Std
   
module Dis = Disasm_expert.Basic

type elem = mem * (Dis.full_insn option)

module G = Graphlib.Make(Addr)(Unit)

type t = {
  arch        : arch;
  filename    : string option;
  main_entry  : addr option;
  sections    : value memmap;
  brancher    : Brancher.t;
  endianness  : endian option;
  lifter      : lifter;
  insns       : (mem * (Dis.full_insn option)) Addr.Table.t;
  lifted      : bil Addr.Table.t;
  insn_risg   : G.t;
  bad         : Addr.Hash_set.t;
  keep        : Addr.Hash_set.t;
  (* marked data  *)
  (* visited *)
  (* union_find *)
}

let of_components
    ?main_entry ?insns ?insn_risg ?lifted ?segments ?endianness ?
    filename arch =
  let insn_risg =
    match insn_risg with
    | Some insn_risg -> insn_risg
    | None -> Graphlib.create (module G) () in
  let segments = Option.value segments ~default:Memmap.empty in
  let insns  = Option.value insns ~default:(Addr.Table.create ()) in
  let lifted = Option.value lifted ~default:(Addr.Table.create ()) in
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  {
    arch        = arch;
    filename;
    sections    = segments;
    brancher    = Brancher.of_bil arch;
    endianness  = None;
    lifter      = lifter;
    main_entry;
    insns;
    insn_risg;
    lifted;
    bad         = Addr.Hash_set.create ();
    keep        = Addr.Hash_set.create ();
  }

module M = struct
  module G = struct
    include G
    module V = G.Node
    type vertex = V.t
    module E = G.Edge
  end
  include Graphlib.To_ocamlgraph(G)
  let empty = G.empty
  let iter_vertex f g =
    Seq.iter (G.nodes g) ~f
  let iter_succ f g v =
    Seq.iter (G.Node.succs v g) ~f
  let fold_succ f g v init =
    Seq.fold ~init (G.Node.succs v g) ~f:(fun v g -> f g v)
  let fold_vertex f g init =
    Seq.fold ~init (G.nodes g) ~f:(fun v g -> f g v)
  let is_directed = true

  let iter_edges_e f g =
    Seq.iter (G.edges g) ~f
  let iter_edges f g =
    Seq.iter (G.edges g) ~f:(fun e ->
        f (G.Edge.src e) (G.Edge.dst e)
      )
  let remove_edge e g =
    G.Edge.remove e g
  let remove_edge_e v1 v2 g =
    let e = G.Edge.create v1 v2 () in
    remove_edge e g
  let add_edge e g =
    G.Edge.insert e g
  let add_edge_e v1 v2 g =
    let e = G.Edge.create v1 v2 () in
    add_edge e g
  let remove_vertex v g =
    G.Node.remove v g
  let add_vertex v g = G.Node.insert v g
  let copy g =
    let gcpy = Graphlib.create (module G) () in
    Seq.fold ~init:gcpy G.(edges g) ~f:(fun gcpy e ->
        add_edge e gcpy
      )
end

module Topological =
  Graph.Topological.Make(M)
module StrongComponents = Graph.Components.Make(M)
(*module DiscreteComponents = Components.Undirected(G)*)
module P = Graph.Builder.P(Graphlib.To_ocamlgraph(G))
module Oper = Graph.Oper.Make(P)
module Dfs        = Graph.Traverse.Dfs(M)

type colored_superset = G.t * Addr.Hash_set.t String.Map.t
                        * elem Addr.Table.t
                      
module Make(T : sig val instance : colored_superset end) = struct
  open T
  module Dottable = struct
    type t = colored_superset

    module V = struct
      type t = M.V.t
    end

    module E = struct
      type t = M.E.t
      let src = M.E.src
      let dst = M.E.dst
    end

    let iter_vertex f (g, _, _) =
      M.iter_vertex f g

    let iter_edges_e f (g, _, _) =
      M.iter_edges_e f g

    let graph_attributes _ = [
      `Fontsize 14;
    ]
    let default_vertex_attributes gr = [
      `Shape `Box; 
      (*`Height 1.0*.Memory.(length mem);*)
      `Fontsize 14;
      `Fontcolor 0x666699;
      `Fontname "Monospace";
      `Width 1.0
    ]

    let red = 0xff0000
    let green = 0x009900
    let yellow = 0xffff00
    let blue = 0x0000ff
    let orange = 0xff6600
    let purple = 0x660066
    let brown = 0x663300
    let cyan = 0x0099cc

    let vertex_name name =
      let fmt = Format.str_formatter in
      Addr.(pp_generic ~prefix:`none ~suffix:`none ~format:`dec
              fmt name);
      Format.flush_str_formatter ()

    let vertex_attributes v =
      let default_attrs =
        [
          `Label ((vertex_name v));
        ] in
      let g, colors, insns = instance in
      let contains name =
        match Map.find colors name with
        | Some(s) ->
          Hash_set.mem s v
        | None -> false in
      let find_update default_attrs name color =
        if contains name then
          `Color color :: default_attrs
        else default_attrs in
      let default_attrs =
        find_update default_attrs "False Negatives" red in
      let default_attrs =
        find_update default_attrs "True Positives" green in
      let default_attrs =
        find_update default_attrs "False Positives" yellow in
      let default_attrs =
        match List.hd default_attrs with
        | Some (`Color _) -> 
          default_attrs
        | _ -> `Color 0X660000 :: default_attrs  in
      match Addr.Table.find insns v with
      | Some(mem,insn) ->
        let len = float_of_int Memory.(length mem) in
        `Height (1.0 *. len) ::
        default_attrs
      | None -> default_attrs


    let get_subgraph _ = None
    let default_edge_attributes _ = [
      `Penwidth 1.0;
      `Arrowsize 0.5;
      `Headport `N;
      `Tailport `S;
      `Labelfloat true;
    ]

    let edge_attributes e =
      (*let (src,dst) = M.E.src e,M.E.dst e in*)
      (*let color,weight = match kind,arity with
        | `Fall,`Many -> 0x660000, 4
        | `Fall,`Mono -> 0x000066, 8
        | `Cond,_ -> 0x006600, 2
        | `Jump,_ -> 0x000066, 2 in*)
      [
        (*`Color color;*)
        (*`Weight weight;*)
      ]
  end
  module Dot = Graph.Graphviz.Dot(Dottable)
end

let fold_component ?visited ~pre ~post g accu addr =
  let visited = Option.value visited
      ~default:(Addr.Hash_set.create ()) in
  let s = Stack.create () in
  (* invariant: [h] contains exactly the vertices which have been pushed *)
  let push v =
    if not (Hash_set.mem visited v) then begin
      Hash_set.add visited v;
      Stack.push s v
    end
  in
  push addr;
  let rec loop acc =
    match Stack.pop s with
    | Some v ->
      let acc = pre acc v in
      M.iter_succ push g v;
      loop @@ post acc v
    | None -> acc
  in
  loop accu

