open Core
open Bap.Std

(** In superset disassembly, branches can occur from within the
    bodies of instructions originally intended by the
    compiler. Therefore, identify branches tries to visit starting
    from entries, and see what set of branches include some child
    that is a descendent from which the traversal originated. *)
let identify_branches superset =
  let deferred = ref Addr.Map.empty in
  let entries = Superset.entries_of_isg superset in
  (* need to create a sequence of non-fall through edges *)
  let insns = Addr.Hash_set.create () in
  let branches = Addr.Hash_set.create () in
  let tag_branches addr =
    if Superset.is_branch superset addr then
      let inbound = Superset.ISG.descendants superset addr in
      List.iter inbound ~f:(fun child -> 
          (* check for edges between instructions that are not
             fall through, but for which  *)
          if Hash_set.mem insns child then
            let ft = Superset.fall_through_of superset addr in
            if not Addr.(ft = child) && 
               not Addr.(addr = child) then
              deferred := Map.set !deferred ~key:ft ~data:(child, addr)
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

(** This searches through the set of blocks starting from entries for
    branches that got hit at least twice. The idea is to respect the
    diamond structure of control flow, which is that both sides
    around a conditional (diamond) must be constructed in the cfg
    statically in order for the final target to be sound
    w.r.t. assembler rules. Using this technique, can filter false
    positive branches. *)
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
  let _ = Traverse.visit_by_block superset ~pre ~post entries in
  let final_jmps = Addr.Hash_set.create () in
  Map.iteri !jmp_hit_cnt ~f:(fun ~key ~data  -> 
      let jmp_addr = key in
      let cnt = data in
      if cnt = 2 then
        Hash_set.add final_jmps jmp_addr;
    );
  final_jmps

(** The objective here is to tag grammar structures while traversing 
    topologically in such a manner that we can converge the 
    probability of recognizing an intended sequence by the 
    compiler. After we've hit some recognition threshold, we begin 
    traversing forward from some activation point whereby we trim 
    occlusive instructions. To recognize grammars, we have several
    means: one, loops are strongly connected components, and if 
    sequences must branch at some point only to reify at a common 
    point, expressing a path by which they can finally rejoin. *)
let tag_by_traversal ?(threshold=8) superset =
  let visited = Addr.Hash_set.create () in
  (*let callsites = Superset.get_callsites ~threshold:6 superset in
  let superset = tag_callsites visited ~callsites superset in
  let superset = Invariants.tag_layer_violations superset in
  let superset = Invariants.tag_branch_violations superset in*)
  let entries = Superset.entries_of_isg superset in
  let branches = Superset.get_branches superset in
  (*let branches = identify_branches superset in*)
  (*let branches = linear_branch_sweep superset entries in*)
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
    if Hash_set.mem branches addr then (
      cur_total := !cur_total + 1;
      positives := addr :: !positives;
      if !cur_total >= threshold then (
        let open Option in 
        ignore (List.nth !positives threshold >>| 
                (fun convergent_point ->
                   Hash_set.add tps convergent_point));
      ) 
    ) in
  let post addr =
    entry := Option.value_map !entry ~default:!entry
        ~f:(fun e -> if Addr.(e = addr) then None else Some(e));
    if Hash_set.mem branches addr then (
      cur_total := !cur_total - 1;
      match !positives with
      | _ :: remaining -> positives := remaining
      | [] -> ();
    ) in
  Traverse.visit ~visited
    ~pre ~post superset entries;
  Hash_set.iter tps ~f:(fun tp -> 
      if not (Hash_set.mem visited tp) then (
        Traverse.with_descendents_at superset tp ~pre:(fun tp -> 
            let mark_bad addr =
              if Superset.ISG.mem_vertex superset addr then (
                Superset.Core.mark_bad superset addr
              ) in
            Superset.Occlusion.with_data_of_insn superset tp ~f:mark_bad;
            Hash_set.add visited tp;
          ) ;
      )
    );
  Hash_set.iter visited 
    ~f:(fun tp -> Superset.Core.clear_bad superset tp);
  superset

    
let parents_of_insns superset component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_parents addr -> 
      List.fold (Superset.ISG.ancestors superset addr)
        ~init:potential_parents
        ~f:(fun potential_parents ancestor ->
            if not Set.(mem component ancestor) then
              Set.add potential_parents ancestor
            else potential_parents
          ) 
    )

let addrs_of_loops loops =
  List.fold_left loops ~init:Addr.Set.empty
    ~f:(fun keep loop ->
      Addr.Set.(union keep (of_list loop))
    )
  
let filter_loops ?(min_size=20) loops =
  let loops =
    List.filter loops ~f:(fun l -> List.length l > min_size) in
  addrs_of_loops loops

let addrs_of_filtered_loops ?(min_size=20) superset =
  filter_loops ~min_size @@ Superset.ISG.raw_loops superset

(** In the body of a loop, instructions fall through eventually to
    themselves, which amounts to effectively a trigger of an
    invariant. But the level at which invariants operate is too fine
    grained to see the consequence propagated from conflicts that are
    potentially in loops that are many instructions long. This
    function cleanses the bodies of instructions that occur in loops
    of a minimum size. *)
let tag_loop_contradictions ?(min_size=20) superset =
  let keep = addrs_of_filtered_loops ~min_size superset in
  (* Here we have to be careful; we only want to find instructions
     that occur within a loop that produce a self-contradiction *)
  let parents = parents_of_insns superset keep in
  let to_remove = 
    Superset.Occlusion.conflicts_within_insns superset keep in
  let to_remove = Set.inter to_remove parents in
  let to_remove = Set.diff to_remove keep in
  Set.iter to_remove ~f:(Superset.Core.mark_bad superset);
  superset

let default_tags = [tag_loop_contradictions]
