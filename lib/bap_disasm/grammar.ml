open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

let identify_branches superset =
  let deferred = ref Addr.Map.empty in
  let insn_risg = Superset.get_graph superset in
  let entries = Superset_risg.entries_of_isg insn_risg in
  (* need to create a sequence of non-fall through edges *)
  let insns = Addr.Hash_set.create () in
  let branches = Addr.Hash_set.create () in
  let tag_branches addr =
    if Superset_risg.G.in_degree insn_risg addr = 2 then
      let inbound = Superset_risg.G.pred insn_risg addr in
      List.iter inbound ~f:(fun child -> 
          (* check for edges between instructions that are not
             fall through, but for which  *)
          if Hash_set.mem insns child then
            let ft = Superset.fall_through_of superset addr in
            if not Addr.(ft = child) && 
               not Addr.(addr = child) then
              deferred := Map.set !deferred ft (child, addr)
        );
  in
  let confirm_branches addr = 
    match Map.find !deferred addr with
    | Some (child, branch) -> 
      if Hash_set.mem insns child then
        Hash_set.add branches branch
    | None -> ()      
  in
  let pre addr = 
    Hash_set.add insns addr;
    tag_branches addr
  in
  let post addr = 
    Hash_set.remove insns addr in
  Traverse.visit 
    ~pre ~post superset entries;
  let pre addr =
    Hash_set.add insns addr;
    confirm_branches addr
  in
  Traverse.visit 
    ~pre ~post superset entries;
  branches

let increment_map_at m ?(x=1) addr =
  m := Map.update !m addr
      ~f:(fun hits -> Option.value_map hits ~default:1
             ~f:(fun hits -> hits +x));
  Option.value ~default:x Map.(find !m addr)

let linear_branch_sweep superset entries =
  let jmp_hit_cnt = ref Addr.Map.empty in
  let update_hit_count = increment_map_at jmp_hit_cnt in
  let pre jmps targets addr =
    if Set.mem targets addr then (
      ignore (update_hit_count addr);
    );
    match Map.find jmps addr  with
    | Some(branch) ->
      ignore (update_hit_count branch);
    | None -> ();
  in
  let post _ _ _ = () in
  Traverse.visit_by_block superset ~pre ~post entries;
  let final_jmps = Addr.Hash_set.create () in
  Map.iteri !jmp_hit_cnt ~f:(fun ~key ~data  -> 
      let jmp_addr = key in
      let cnt = data in
      if cnt = 2 then
        Hash_set.add final_jmps jmp_addr;
    );
  final_jmps

let tag_callsites visited ?callsites superset =
  let insn_risg = Superset.get_graph superset in
  let callsites = Option.value callsites 
      ~default:(Superset.get_callsites ~threshold:6 superset) in
  let insn_isg  = Superset_risg.Oper.mirror insn_risg in
  Hash_set.iter callsites ~f:(fun callsite ->
      Superset_risg.iter_component ~visited insn_isg callsite;
    );
  superset

(* The objective here is to tag grammar structures while traversing *)
(* topologically in such a manner that we can converge the *)
(* probability of recognizing an intended sequence by the *)
(* compiler. After we've hit some recognition threshold, we begin *)
(* traversing forward from some activation point whereby we trim *)
(* occlusive instructions. To recognize grammars, we have several *)
(* means: one, loops are strongly connected components, and if *)
(* sequences must branch at some point only to reify at a common *)
(* point, expressing a path by which they can finally rejoin. *)
let tag_by_traversal ?(threshold=8) superset =
  let insn_risg = Superset.get_graph superset in
  let visited = Addr.Hash_set.create () in
  (* TODO should be either in it's own module and designated function *)
  let callsites = Superset.get_callsites ~threshold:6 superset in
  let superset = tag_callsites visited ~callsites superset in
  let superset = Invariants.tag_layer_violations superset in
  let superset = Invariants.tag_branch_violations superset in
  let insn_isg  = Superset_risg.Oper.mirror insn_risg in
  let entries = Superset_risg.entries_of_isg insn_risg in
  let branches = Superset_risg.get_branches insn_risg in
  (*let branches = identify_branches superset in*)
  (*let branches = linear_branch_sweep superset entries in*)
  (* TODO should delete this printf *)
  printf "detected %d if structures, %d callsites\n" 
    (Hash_set.length branches) Hash_set.(length callsites);
  let cur_total = ref 0 in
  let positives = ref [] in
  let entry = ref None in
  let tps = Addr.Hash_set.create () in
  (* In the case that our current starting point, entry, is none, set *)
  (* it to being the address of the lambda parameter, addr. Then, set *)
  (* the current total number of recognized grammar items to zero, *)
  (* as well as the positives since we're starting over *)
  let pre addr = 
    if Option.is_none !entry then (
      entry := Some(addr);
      cur_total := 0;
      positives := [];
    );
    if Hash_set.mem branches addr || Hash_set.mem callsites addr then (
      cur_total := !cur_total + 1;
      positives := addr :: !positives;
      if !cur_total >= threshold then (
        let open Option in 
        (* TODO could add the remaining part of the positives from threshold *)
        ignore (List.nth !positives threshold >>| 
                (fun convergent_point ->
                   Hash_set.add tps convergent_point));
      ) 
    ) in
  (* TODO, post does not take into account that increments may occur *)
  (* across layers and keep those isolated *)
  let post addr =
    entry := Option.value_map !entry ~default:!entry
        ~f:(fun e -> if e = addr then None else Some(e));
    if Hash_set.mem branches addr || Hash_set.mem callsites addr then (
      cur_total := !cur_total - 1;
      match !positives with
      | _ :: remaining -> positives := remaining
      | [] -> ();
    ) in
  Traverse.visit
    ~pre ~post superset entries;
  printf "marked %d convergences\n" (Hash_set.length tps);
  let visited = Addr.Hash_set.create () in
  Hash_set.iter tps ~f:(fun tp -> 
      if not (Hash_set.mem visited tp) then (
        Superset_risg.Dfs.prefix_component (fun tp -> 
            let mark_bad addr =
              if Superset_risg.G.mem_vertex insn_risg addr then (
                Superset.mark_bad superset addr
              ) in
            Superset.with_data_of_insn superset tp ~f:mark_bad;
            Hash_set.add visited tp;
          ) insn_isg tp;
      )
    );
  Hash_set.iter visited 
    ~f:(fun tp -> Superset.clear_bad superset tp);
  superset
