open Bap_types.Std
open Bap_image_std
open Graph
open Core_kernel.Std

module G = Imperative.Digraph.ConcreteBidirectional(struct 
    type t = Addr.t 
    let compare = Addr.compare
    let hash = Addr.hash
    let equal = Addr.equal
  end)
type t = G.t

module P = Persistent.Digraph.ConcreteBidirectional(struct
    type t = Addr.t
    let compare = Addr.compare
    let hash = Addr.hash
    let equal = Addr.equal
  end)

module Kruskal = Kruskal.Make(G)(struct
    type t = G.E.label
    let compare _ _ = 0
  end)

module Topological = Topological.Make(G)
module Dominator = Dominator.Make(G)
module Oper = Oper.I(G)
module StrongComponents = Components.Make(G)
(*module DiscreteComponents = Components.Undirected(G)*)
module Dfs        = Graph.Traverse.Dfs(G)
module Bfs        = Graph.Traverse.Bfs(G)
module Path       = Path.Check(G)
module GmlOut     = Gml.Print(G)(struct 
    let node (label : G.V.label) = 
      [ "addr", Gml.String (Addr.to_string label) ]
    let edge _ = []
  end)
module B = struct
  module G = struct
    include P
  end
  include P
  let copy g = g
  let empty () = P.empty
end
module GmlIn      = Gml.Parse(B)(struct
    let node (labels : Gml.value_list) = 
      match labels with
      | [] -> assert false
      | fail :: [] -> assert false
      | (id, idval) :: (s, gmlval) :: _ -> 
        match idval, gmlval with
        | Gml.Int(idval), Gml.String(addr) -> 
          B.G.V.label Addr.(of_string addr)
        | _ -> assert false

    let edge (labels : Gml.value_list) = ()
  end)
module Gml = struct
  include GmlIn
  include GmlOut
  let parse gmlstr = 
    let pgraph = parse gmlstr in
    let igraph = G.create () in
    P.iter_edges (fun src target -> 
        let src    = B.G.V.create src in
        let target = B.G.V.create target in
        G.add_edge igraph src target;
      ) pgraph;
    igraph
end

let add ?superset_risg mem insn =
  let superset_risg =
    Option.value superset_risg ~default:(G.create ()) in
  let src = Memory.min_addr mem in
  let bad = Addr.of_int ~width:(Addr.bitwidth src) 0 in
  match insn with
  | Some(insn) ->
    G.add_vertex superset_risg src;
  | None -> G.add_edge superset_risg bad src

let subgraph insn_risg subgraph =
  let g = G.create () in
  Hash_set.iter subgraph ~f:(fun addr ->
      G.add_vertex g addr;
      G.iter_succ
        (fun s ->
           if Hash_set.mem subgraph s then
             G.add_edge g addr s
        ) insn_risg addr;
      G.iter_pred
        (fun s ->
           if Hash_set.mem subgraph s then
             G.add_edge g s addr
        ) insn_risg addr;
    );
  g

let exits_of_isg insn_isg component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_exits addr -> 
      G.fold_pred (fun ancestor potential_exits ->
          if not Set.(mem component ancestor) then
            Set.add potential_exits ancestor
          else potential_exits
        ) insn_isg addr potential_exits
    )


let risg_of_raw_superset ?superset_risg raw_superset =
  let superset_risg = Option.value superset_risg ~default:(G.create ()) in
  List.iter raw_superset ~f:(fun (mem, insn) ->
      add ~superset_risg mem insn
    );
  superset_risg

let conflicts_within_insn_at ?mem ?conflicts insn_map addr =
  let mem = Option.value mem ~default:(Map.mem insn_map) in
  let conflicts = Option.value conflicts ~default:Addr.Set.empty in
  let rec within_insn conflicts insn_map cur_addr len =
    if Addr.(cur_addr >= (addr ++ len)) then
      conflicts
    else
      let conflicts = if mem cur_addr then
          let conflicts = Set.add conflicts addr in
          Set.add conflicts cur_addr
        else conflicts in 
      within_insn conflicts insn_map Addr.(cur_addr ++ 1) len in
  match Map.find insn_map addr with
  | Some ((mem, _)) ->
    (* look within the body for instructions *)
    let len = (Memory.length mem) in
    within_insn conflicts insn_map Addr.(addr ++ 1) len
  | None -> conflicts

let conflicts_within_insns insn_map keep =
  Set.fold keep ~init:Addr.Set.empty
    ~f:(fun conflicts addr -> 
        conflicts_within_insn_at 
          ~conflicts insn_map addr
      )

let find_all_conflicts ?mem insn_map =
  List.fold Map.(keys insn_map) ~init:Addr.Set.empty
    ~f:(fun conflicts addr -> 
      conflicts_within_insn_at ?mem ~conflicts insn_map addr
    )

let seq_of_addr_range addr len = 
  let open Seq.Generator in
  let rec gen_next_addr cur_addr = 
    if Addr.(cur_addr >= (addr ++ len)) then
      return ()
    else
      yield cur_addr >>=  fun () -> 
      let next_addr = Addr.succ cur_addr in
      gen_next_addr next_addr
  in run (gen_next_addr Addr.(succ addr))

let range_seq insn_map =
  let map_seq = Addr.Map.to_sequence insn_map in
  Seq.bind map_seq (fun (addr, (mem, _)) -> 
      seq_of_addr_range addr (Memory.length mem)
    )

let range_seq_of_conflicts ~mem addr len = 
  let range_seq = seq_of_addr_range addr len in
  Seq.filter range_seq ~f:mem

(* TODO do not need to use insn_cfg. Could use superset type *)
let seq_of_all_conflicts insn_map insn_isg = 
  let insn_map_seq = Addr.Map.to_sequence insn_map in
  let check_mem = Addr.Map.(mem insn_map) in
  Seq.bind insn_map_seq (fun (addr, (mem, _)) -> 
      range_seq_of_conflicts ~mem:check_mem addr (Memory.length mem)
    )

let conflict_seq_at insn_map addr =
  let check_mem = Addr.Map.(mem insn_map) in
  match Map.find insn_map addr with
  | Some(mem, _) -> 
    let len = Memory.length mem  in
    range_seq_of_conflicts ~mem:check_mem addr len
  | None -> Seq.empty

let parent_conflict_at insn_risg insn_map addr =
  let children = G.pred insn_risg addr in
  List.fold children ~init:Addr.Set.empty ~f:(fun cparents child -> 
      let parents = G.succ insn_risg child in
      List.fold parents ~init:cparents ~f:(fun cparents parent -> 
          if not Addr.(parent = addr) then
            match Map.find insn_map parent with
            | Some(mem, _) -> 
              let len = Memory.length mem in
              if Addr.(parent < addr) && Addr.(addr < (parent ++ len)) then
                Set.add cparents parent
              else cparents
            | None -> cparents
          else cparents
        )
    )

let mergers_of_isg insn_isg = 
  G.fold_vertex (fun addr mergers ->
      if G.out_degree insn_isg addr > 1 then
        Addr.Set.add mergers addr
      else mergers) insn_isg Addr.Set.empty

let is_entry insn_isg addr = 
  G.in_degree insn_isg addr  = 0 &&
  G.out_degree insn_isg addr > 0

let entries_of_isg insn_isg = 
  G.fold_vertex (fun addr accu ->
      if is_entry insn_isg addr then
        (Hash_set.add accu addr; accu)
      else accu)
    insn_isg (Addr.Hash_set.create ())

let is_branch insn_risg addr =
  G.in_degree insn_risg addr = 2

let get_branches insn_risg =
  let branches = Addr.Hash_set.create () in
  G.iter_vertex (fun vert -> 
      if is_branch insn_risg vert then
        Hash_set.add branches vert;
    ) insn_risg;
  branches

let get_loop_addrs insn_risg = 
  let loop_addrs = 
    StrongComponents.scc_list insn_risg in
  List.fold_left loop_addrs ~init:Addr.Set.empty 
    ~f:(fun loop_addrs loop -> 
        List.fold_left ~init:loop_addrs loop ~f:(fun loop_addrs addr -> 
            Set.add loop_addrs addr
          )
      )

let iter_component ?(terminator=(fun _ -> true))
    ?visited ?(pre=fun _ -> ()) ?(post=fun _ -> ()) g v =
  let visited = Option.value visited 
      ~default:(Addr.Hash_set.create ()) in
  let rec visit v =
    Hash_set.add visited v;
    pre v;
    G.iter_succ
      (fun w ->
         if (not (Hash_set.mem visited w)) && (terminator w) then
           visit w) g v;
    post v
  in visit v

let fold_component ?visited ~pre ~post i g v0 =
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
  push v0;
  let rec loop acc =
    match Stack.pop s with
    | Some v ->
      let acc = pre acc v in
      G.iter_succ push g v;
      loop @@ post acc v
    | None -> acc
  in
  loop i

let get_depth insn_risg x =
  let depth = ref 0 in
  let deepest = ref 0 in
  let pre x = depth := !depth + 1 in
  let post x =
    deepest := max !deepest !depth;
    depth := !depth - 1; in
  iter_component insn_risg ~pre ~post x;
  !deepest

let collect_target_entries visited insn_risg insn_isg addr = 
  let target_entries = Addr.Hash_set.create () in
  let pre t = 
    if is_entry insn_risg t then
      Hash_set.add target_entries t in
  let post _ = () in
  iter_component ~visited ~pre ~post insn_isg addr;
  target_entries

let activate_descendants active insn_isg addr = 
  let pre _ = ()in
  let post _ = () in
  iter_component ~visited:active ~pre ~post insn_isg addr
