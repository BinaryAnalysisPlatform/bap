open Core_kernel.Std
open Superset_risg
open Common
open Graphlib.Std
open Graph
open Bap_types.Std
open Bap_image_std


let parents_of_insns insn_isg component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_parents addr -> 
      Superset_risg.G.fold_succ (fun ancestor potential_parents ->
          if not Set.(mem component ancestor) then
            Set.add potential_parents ancestor
          else potential_parents
        ) insn_isg addr potential_parents
    )

let filter_components ?(min_size=20) components = 
  List.fold_left components ~init:Addr.Set.empty
    ~f:(fun keep  component ->
        let component = Addr.Set.of_list component in
        if Set.length component > min_size then
          Addr.Set.(union keep component)
        else
          keep
      )

let tag_loop_contradictions ?(min_size=20) superset = 
  let insn_risg = Superset.get_graph superset in
  let insn_map = Superset.get_map superset in
  let keep = filter_components ~min_size @@ 
    StrongComponents.scc_list insn_risg in
  (* Here we have to be careful; we only want to find instructions
     that occur within a loop that produce a self-contradiction *)
  let parents = parents_of_insns insn_risg keep in
  let to_remove = 
    Superset_risg.conflicts_within_insns insn_map keep in
  let to_remove = Set.inter to_remove parents in
  let to_remove = Set.diff to_remove keep in
  printf "tagged %d contradictions of %d parents of %d to keep\n" 
    Set.(length to_remove)
    Set.(length parents)
    Set.(length keep);
  Set.iter to_remove ~f:(Superset.mark_bad superset);
  Superset.rebuild ~insn_map ~insn_risg superset

let default_tags = [tag_loop_contradictions]

let tagged_disasm_of_file ?(backend="llvm") bin =
  let superset = Trim.tagged_disasm_of_file 
      ~f:[(fun s x y z -> Superset.add_to_map s x y)]
      ~data:() ~backend bin in
  tag_loop_contradictions superset

(* TODO belongs elsewhere or is a duplicate *)
let trimmed_disasm_of_file ?(backend="llvm") bin =
  let superset = tagged_disasm_of_file ~backend bin in
  Trim.Default.trim superset

let sheaths_of_file ?(backend="llvm") bin = 
  let superset = tagged_disasm_of_file ~backend bin in
  superset, Decision_tree_set.decision_trees_of_superset superset

let trimmed_sheaths_of_file ?(backend="llvm") bin =
  let superset = Trim.Default.trim (tagged_disasm_of_file ~backend bin) in
  superset, Decision_tree_set.decision_trees_of_superset superset

(* TODO test the below functions *)
let iter_decision_set ?(backend="llvm") bin ~f = 
  let superset, decision_trees = trimmed_sheaths_of_file ~backend bin in
  List.iter decision_trees ~f

let fold_decision_set ~init ?(backend="llvm") bin ~f =
  let superset, decision_trees =
    trimmed_sheaths_of_file ~backend bin in
  List.fold decision_trees ~init ~f

