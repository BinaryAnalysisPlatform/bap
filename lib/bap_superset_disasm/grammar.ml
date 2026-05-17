open Core
open Bap.Std

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

