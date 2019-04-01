open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

let visit ?visited ~pre ~post superset entries =
  let visited = Option.value visited
      ~default:(Addr.Hash_set.create ()) in
  let pre addr =
    Hash_set.add visited addr;
    pre addr in
  let insn_risg = Superset.get_graph superset in
  Hash_set.iter entries ~f:(fun addr ->
      if not (Hash_set.mem visited addr) then
        Superset_risg.iter_component ~visited ~pre ~post insn_risg addr
    )

let visit_with_deltas ?pre ?post ~is_option superset entries =
  let pre = Option.value pre ~default:(fun _ _ -> ()) in
  let post = Option.value post ~default:(fun _ _ -> ()) in
  let deltas = ref (Decision_tree_set.calculate_deltas
                      superset ~entries is_option) in
  let pre addr = 
    pre !deltas addr in
  let post addr = 
    post !deltas addr;
    deltas := Map.remove !deltas addr
  in
  visit ~pre ~post superset entries

let visit_by_block superset
    ?(pre=(fun _ _ _ -> ())) ?(post=(fun _ _ _ -> ())) entries = 
  let insn_risg = Superset.get_graph superset in
  let (jmps,targets) = Superset_risg.G.fold_edges (fun src target (jmps,targets) -> 
      let is_branch = Superset_risg.is_branch insn_risg target in
      let is_jmp_edge = not (Superset.is_fall_through superset src target) in
      if is_branch && is_jmp_edge then
        (Map.set jmps src target, Set.add targets target)
      else (jmps, targets)
    ) insn_risg (Addr.Map.empty,Addr.Set.empty) in
  (*let loop_addrs = Superset_risg.get_loop_addrs insn_risg in
    let jmps = Set.fold loop_addrs ~init:jmps ~f:(fun jmps addr -> 
      match Map.find jmps addr with
      | Some(j) -> 
    if Set.mem loop_addrs j then
          Map.remove jmps j
        else jmps
      | None -> jmps
    ) in*)
  Map.iteri jmps ~f:(fun ~key ~data -> 
      Superset_risg.G.remove_edge insn_risg key data;
    );
  let entries = Superset_risg.entries_of_isg insn_risg in
  let visited = Addr.Hash_set.create () in
  let rec visit v =
    Hash_set.add visited v;
    pre jmps targets v;
    Superset_risg.G.iter_succ
      (fun w -> if not (Hash_set.mem visited w) then visit w else pre jmps targets w)
      insn_risg v;
    post jmps targets v;
  in 
  Hash_set.iter entries ~f:visit;
  Map.iteri jmps ~f:(fun ~key ~data -> 
      Superset_risg.G.add_edge insn_risg key data;
    )

