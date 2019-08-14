open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Graphlib.Std
open Graph

(** The decision set represents a set of potentially inter-dependent
      decision trees and potential requirements of selection at each
      node. Although a graph is used, the actual structure is acyclic.
      The addr -> terminal map expresses relationships between the
      graph with which it is paired with other members of the
      enclosing decision_tree_set *)

let conflicts_of_entries entries insn_map =
  let visited_entries = Addr.Hash_set.create () in
  Hash_set.fold entries ~init:[] ~f:
    (fun conflicted_entries entry -> 
       if not (Hash_set.mem visited_entries entry) then (
         Hash_set.add visited_entries entry;
         let in_entry_conflicts = 
           Superset_risg.conflicts_within_insn_at insn_map entry in
         let conflicts = Addr.Hash_set.create () in
         Hash_set.add conflicts entry;
         Set.iter in_entry_conflicts 
           ~f:(fun conflict ->
               (* A conflict that an entry may have may or may not *)
               (* itself be an entry. *)
               if Hash_set.mem entries conflict then (
                 Hash_set.add visited_entries conflict;
                 Hash_set.add conflicts conflict;
               )
             );
         if (Hash_set.length conflicts) > 1 then (
           conflicts :: conflicted_entries
         ) else conflicted_entries
       ) else conflicted_entries
    )

let tails_of_conflicts conflicts insn_isg = 
  let possible_tails = Superset_risg.mergers_of_isg insn_isg in
  (* This tail is the particular instruction
     that is the fall through target of several potential
     competitors. We use this instruction against the
     leaders map because those will be the ones that fall
     through to the tail; the tail can then be associated with
     those that lead into it. *)
  let tails, _ = Set.fold ~init:(Addr.Map.empty, Addr.Set.empty)
      ~f:(fun (tails, added_choices) possible_tail -> 
          (* For each edge from tail, lookup the respective vertex; if it *)
          (* is in the conflicts set, then it gets added to a sheath *)
          (* of choices. *)
          let f sheath poss_conflict =
            let not_added = not (Set.mem added_choices poss_conflict) in
            let is_conflict = Set.mem conflicts poss_conflict in
            let is_connected = 
              match Superset_risg.G.find_all_edges
                      insn_isg possible_tail poss_conflict with
              | [] -> false | _ -> true in
            if not_added && is_conflict && is_connected then
              poss_conflict :: sheath
            else sheath in
          let sheath = List.fold_left
              (Superset_risg.G.succ insn_isg possible_tail) ~init:[] ~f
          in
          match sheath with
          | [] | _ :: []-> tails, added_choices
          | _ -> 
            let added_choices =
              Set.inter added_choices (Addr.Set.of_list sheath) in
            (Addr.Map.set tails ~key:possible_tail ~data:sheath, added_choices)
        ) possible_tails in 
  tails

let decision_tree_of_entries conflicted_entries entries tails insn_isg =
  let visited = Addr.Hash_set.create () in
  let visited_choices = Addr.Hash_set.create () in
  let add_choices decision_tree current_vert = 
    let unvisited =
      not (Hash_set.mem visited_choices current_vert) in
    if unvisited then
      let possible_tail = current_vert in
      match Addr.Map.find tails possible_tail with
      | Some(sheath) ->
        List.iter sheath ~f:(fun competitor ->
            Hash_set.add visited_choices competitor;
            Superset_risg.G.add_edge decision_tree possible_tail
              competitor;
          );
      | _ -> ()
    else ();
  in
  let link_zero decision_tree entry =
    let width = Addr.bitwidth entry in
    let zero = Addr.(of_int ~width 0) in
    Superset_risg.G.add_edge decision_tree zero entry
  in
  let f decision_tree entry =
    let width = Addr.bitwidth entry in
    let saved_vert = ref @@
      Addr.of_int ~width 0 in
    let link_choices current_vert =
      add_choices decision_tree entry;
      let contained = Superset_risg.G.mem_vertex
          decision_tree current_vert in
      let is_new = Hash_set.mem visited current_vert in
      if contained && is_new then (
        if not @@ Superset_risg.G.mem_edge decision_tree !saved_vert
            current_vert then (
          Superset_risg.G.add_edge decision_tree !saved_vert
            current_vert;
        );
        saved_vert := current_vert;
      );
      Hash_set.add visited current_vert
    in
    (* Would like to have fold_component; not available in this
       version *)
    Superset_risg.Dfs.prefix_component link_choices insn_isg entry;
  in
  let conflicted_trees = 
    List.filter_map conflicted_entries ~f:(fun conflicted ->
        if Hash_set.length conflicted > 0 then
          let decision_tree = Superset_risg.G.create () in
          let f entry = 
            if not (Hash_set.mem visited entry) then (
              link_zero decision_tree entry;
              f decision_tree entry) in
          Hash_set.iter conflicted ~f;
          Some(decision_tree)
        else None
      ) in
  Hash_set.fold entries ~init:conflicted_trees 
    ~f:(fun all_trees entry ->
        if not (Hash_set.mem visited entry) then
          let decision_tree = Superset_risg.G.create () in
          f decision_tree entry;
          if Superset_risg.G.nb_vertex decision_tree > 0 then
            decision_tree :: all_trees
          else all_trees
        else (all_trees)
      )


(** Accepts a per instruction control flow graph, and a map from addr *)
(** to (mem, insn) *)
let decision_trees_of_superset superset = 
  let open Superset in
  let insn_map = Superset.get_map superset in
  let insn_risg = Superset.get_graph superset in
  (* Here, for each vertex, look up the insn from the map and *)
  (* identify conflicts. *)
  let conflicts = Superset_risg.find_all_conflicts insn_map in
  (* entries variable:
     We want to know the superset of all nodes that could be the
     terminating point that would otherwise be the return instruction
     of a function. *)
  let entries = Superset_risg.entries_of_isg insn_risg in  
  (*
     we need to keep track of the subset of potential choices 
     that fall in line with the normal control flow graph, and
     leave the discovery of overlapping redirection to a 
     second pass, in order that when we do a map over all 
     instructions to check for conflicts, we know which are tails
     in order to properly construct the sheath type.
  *)
  let tails = tails_of_conflicts conflicts insn_risg in
  (* It may be that some entries are accidental indirections that *)
  (* happen to preside at the intended entry. These must map to to an *)
  (* entirely distinct interpretation. *)
  let conflicted_entries = conflicts_of_entries entries insn_map in
  (* For each of the potentially conflicting entries, construct a *)
  (* decision tree. *)
  let decision_trees = decision_tree_of_entries
      conflicted_entries entries tails insn_risg in
  decision_trees

let calculate_deltas superset ?entries is_option = 
  let insn_risg = Superset.get_graph superset in
  let entries = Option.value entries 
      ~default:(Superset_risg.entries_of_isg insn_risg) in
  let add_data_of_insn dataset at = 
    Superset.with_data_of_insn superset at ~f:(Hash_set.add dataset)
  in
  let deltas = ref Addr.Map.empty in
  let delta = ref None in
  let make_deltas addr =
    let insns, datas = 
      match !delta with
      | Some (insns, datas) -> (insns, datas) 
      | None ->     
        let insns = Addr.Hash_set.create () in
        let datas = Addr.Hash_set.create () in
        delta := Some(insns, datas);
        insns, datas in
    if is_option addr then (
      deltas := Addr.Map.set !deltas addr (insns, datas);
      delta := None
    ) else (
      add_data_of_insn datas addr;
      Hash_set.add insns addr;
    )
    (* else if is in entries then store the delta in the deltas map *)
  in
  let visited = Addr.Hash_set.create () in
  Hash_set.iter entries 
    ~f:(Superset_risg.iter_component ~visited ~post:make_deltas insn_risg);
  !deltas

(* TODO Need to find some way to cache results *)
(* TODO could split this into tail_options *)
let insn_is_option superset addr = 
  let open Superset in
  let len = Superset.len_at superset addr in
  let bound = Addr.(addr ++ len) in
  let insn_risg = Superset.get_graph superset in
  let previous = Superset_risg.G.pred insn_risg addr in
  List.fold ~init:false previous ~f:(fun current descedant -> 
      if not current then
        let further = Superset_risg.G.succ insn_risg descedant in
        List.fold ~init:current further ~f:(fun current opt -> 
            if not current then
              if Addr.(addr <= opt) && Addr.(opt < bound) then
                true
              else false
            else current
          )
      else current
    )
