open Core
open Bap.Std
open Graphlib.Std

module G = Graphlib.Make(Addr)(Unit)

         
(** The decision tree represents a set of potentially inter-dependent
    decision trees and potential ramifications of selection at each
    node. The objective is to present to the user a clean interface
    by which to construct mutually compatible decisions, since it it
    possible for a blithely written analysis to piece together many
    decisions that are not fit with the whole. *)

type decision_tree = {
    tree : G.t;
    starts : Addr.Hash_set.t;
  }
type decision_forest = decision_tree list
type 'a possibility
type 'a choice
type 'a consequence
type tail
   
let count tree =
  List.length tree
let with_trees (tree : decision_forest)  =
  List.fold tree
   
(** For any given entry, calculate the conflicts, and filter the set
    down to lists of entries that conflict with one another. *)
let conflicts_of_entries superset entries =
  let visited_entries = Addr.Hash_set.create () in
  Hash_set.fold entries ~init:[] ~f:
    (fun conflicted_entries entry -> 
       if not (Hash_set.mem visited_entries entry) then (
         Hash_set.add visited_entries entry;
         let in_entry_conflicts = 
           Superset.Occlusion.conflicts_within_insn_at superset entry in
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

(** Calculate the set potential points where occlusive instructions
    could rejoin to a common target, such as cease when falling
    through to the same instruction. Calculate the tail, or the join
    target and the conflicts that led into that. *)
let tails_of_conflicts superset conflicts =
  let possible_tails = Superset.mergers superset in
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
            let is_connected = Superset.ISG.check_connected
                superset possible_tail poss_conflict in
            if not_added && is_conflict && is_connected then
              poss_conflict :: sheath
            else sheath in
          let sheath = List.fold_left
              (Superset.ISG.ancestors superset possible_tail) ~init:[] ~f
          in
          match sheath with
          | [] | _ :: []-> tails, added_choices
          | _ -> 
            let added_choices =
              Set.inter added_choices (Addr.Set.of_list sheath) in
            (Addr.Map.set tails ~key:possible_tail ~data:sheath, added_choices)
        ) possible_tails in 
  tails

let add_edge dtr v1 v2 =
  let g = dtr.tree in
  let e = G.Edge.create v1 v2 () in
  let g = G.Edge.insert e g in
  { dtr with tree = g }

let mem_edge dtr v1 v2 =
  let g = dtr.tree in
  let e = G.Edge.create v1 v2 () in
  G.Edge.mem e g

let new_dtree () =
  let tree = Graphlib.create (module G) () in
  let starts = Addr.Hash_set.create () in
  { tree; starts; }

module DecisionTree = struct
  let count dtree =
    (G.number_of_nodes dtree.tree) + (Hash_set.length dtree.starts)
  let mem addr dtree = G.Node.mem addr dtree.tree
                       || Hash_set.mem dtree.starts addr
end                     

(** Starting from each entry in the superset, identify the tails and
    build a decision tree that allows to jump from conflict to
    conflict and review the options. *)
let decision_tree_of_entries superset conflicted_entries entries tails =
  let visited = Addr.Hash_set.create () in
  let add_choices decision_tree current_vert = 
    let unvisited =
      not (Hash_set.mem visited current_vert) in
    if unvisited then
      let possible_tail = current_vert in
      match Addr.Map.find tails possible_tail with
      | Some(sheath) ->
        List.fold sheath ~init:decision_tree ~f:(fun decision_tree competitor ->
            add_edge decision_tree possible_tail competitor
          );
      | _ -> decision_tree
    else decision_tree;
  in
  let link_start decision_tree entry =
    Hash_set.add decision_tree.starts entry;
    { decision_tree with starts = decision_tree.starts }
  in
  let f decision_tree entry =
    let saved_vert = ref entry in
    let link_choices decision_tree current_vert =
      let decision_tree = add_choices decision_tree entry in 
      let contained = DecisionTree.mem current_vert decision_tree in
      let is_new = Hash_set.mem visited current_vert in
      let decision_tree = 
        if contained && is_new then (
          let decision_tree = 
            if not @@ mem_edge decision_tree !saved_vert
                        current_vert then (
              add_edge decision_tree !saved_vert
                current_vert
            ) else decision_tree in
          saved_vert := current_vert;
          decision_tree
        ) else decision_tree in
      decision_tree
    in
    Superset.ISG.dfs_fold superset ~visited decision_tree 
      ~post:(fun g v -> g) ~pre:link_choices entry
  in
  let conflicted_trees = 
    List.filter_map conflicted_entries ~f:(fun conflicted ->
        if Hash_set.length conflicted > 0 then
          let decision_tree = new_dtree () in
          let f decision_tree entry = 
            if not (Hash_set.mem visited entry) then (
              let decision_tree = link_start decision_tree entry in
              f decision_tree entry) else decision_tree
          in
          let decision_tree =
            Hash_set.fold ~init:decision_tree conflicted ~f in
          Some(decision_tree)
        else None
      ) in
  Hash_set.fold entries ~init:conflicted_trees 
    ~f:(fun all_trees entry ->
        if not (Hash_set.mem visited entry) then
          let decision_tree = new_dtree () in
          let decision_tree = f decision_tree entry in
          if DecisionTree.count decision_tree > 0 then
            decision_tree :: all_trees
          else all_trees
        else (all_trees)
      )

(** Accepts a superset, and calculates the decision trees over groups
    of instructions. The returned trees index from tails to the
    options available. *)
let decision_trees_of_superset superset = 
  (* Here, for each vertex, look up the insn from the map and *)
  (* identify conflicts. *)
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  (* entries variable:
     We want to know the superset of all nodes that could be the
     terminating point that would otherwise be the return instruction
     of a function. *)
  let entries = Superset.entries_of_isg superset in  
  (*
     we need to keep track of the subset of potential choices 
     that fall in line with the normal control flow graph, and
     leave the discovery of overlapping redirection to a 
     second pass, in order that when we do a map over all 
     instructions to check for conflicts, we know which are tails
     in order to properly construct the sheath type.
  *)
  let tails = tails_of_conflicts superset conflicts in
  (* It may be that some entries are accidental indirections that *)
  (* happen to preside at the intended entry. These must map to to an *)
  (* entirely distinct interpretation. *)
  let conflicted_entries = conflicts_of_entries superset entries in
  (* For each of the potentially conflicting entries, construct a *)
  (* decision tree. *)
  let decision_trees = decision_tree_of_entries
      superset conflicted_entries entries tails in
  decision_trees

let insn_is_option superset addr = 
  let open Superset in
  let len = Superset.Inspection.len_at superset addr in
  let bound = Addr.(addr ++ len) in
  let previous = Superset.ISG.descendants superset addr in
  List.fold ~init:false previous ~f:(fun current descedant -> 
      if not current then
        let further = Superset.ISG.ancestors superset descedant in
        List.fold ~init:current further ~f:(fun current opt -> 
            if not current then
              if Addr.(addr <= opt) && Addr.(opt < bound) then
                true
              else false
            else current
          )
      else current
    )

(** For a given superset that contains groups of instruction lineages
    as potential choices, calculate the result of picking a given
    choice as a delta. *)
let calculate_deltas ?entries ?is_option superset =
  let is_option =
    Option.value is_option
      ~default:(insn_is_option superset) in
  let entries = Option.value entries 
      ~default:(Superset.entries_of_isg superset) in
  let add_data_of_insn dataset at = 
    Superset.Occlusion.with_data_of_insn
      superset at ~f:(Hash_set.add dataset)
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
    ~f:(Traverse.with_ancestors_at
          ~visited ~post:make_deltas ?pre:None superset);
  !deltas

module Speculate = struct
  let weigh_possibilities _ _ = ()
  let make_choices x _ _ = x
end                         

(** A delta from decision trees is constructed and passed to the
    visitor functions during a visit. *)
let visit_with_deltas ?pre ?post ~is_option superset entries =
  let pre = Option.value pre ~default:(fun _ _ -> ()) in
  let post = Option.value post ~default:(fun _ _ -> ()) in
  let deltas = ref (calculate_deltas
                      superset ~entries ~is_option) in
  let pre addr = 
    pre !deltas addr in
  let post addr = 
    post !deltas addr;
    deltas := Map.remove !deltas addr
  in
  Traverse.visit ~pre ~post superset entries
