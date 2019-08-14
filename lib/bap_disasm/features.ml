open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_target_factory

module Insn    = Bap_disasm_insn
module Dis = Bap_disasm_basic

(* TODO could have a feature that correlates def-use to text section *)
let default_features = [
  "ImgEntry";
  (*"NoExit";*)
  (*"LoopsWithBreak";*)
  "BranchViolations";
  (*"LayerViolations";*)
  (*"TrimClamped";*)
  "TrimLimitedClamped";
  "Callsites3";
  (*"TrimFixpointGrammar";
    "TrimFixpointTails";*)
  (*"Clamped";
    "SCC";
    "LoopGrammar";
    "CallsiteLineage";
    "SSA";*)
  (*"FreeVarSSA";*)
  (*"Grammar";*)
  (*"Constant";*)
]
let default_features = List.rev default_features

let transform = Hash_set.fold ~init:Addr.Set.empty ~f:Set.add

let find_free_insns superset = 
  let insn_map = Superset.get_map superset in
  let insns = Map.to_sequence insn_map in
  let insn_risg = Superset.get_graph superset in
  let mem = Superset_risg.G.mem_vertex insn_risg in
  let all_conflicts = Addr.Hash_set.create () in
  let to_clamp =
    Seq.fold ~init:(Addr.Set.empty)
      ~f:(fun (to_clamp) (addr,(memory,_)) ->
        let len = Memory.length memory in
        let conflicts = Superset_risg.range_seq_of_conflicts
                          ~mem addr len in
        let no_conflicts = Seq.is_empty conflicts in
        Seq.iter conflicts ~f:(fun c ->
            Hash_set.add all_conflicts c);
        if no_conflicts && not Hash_set.(mem all_conflicts addr) then
          Set.add to_clamp addr
        else (
          to_clamp
        )
      ) insns in
  to_clamp
(*Hash_set.fold all_conflicts ~init:to_clamp ~f:Set.remove*)

let restricted_clamp superset = 
  let insn_risg = Superset.get_graph superset in
  let insn_map  = Superset.get_map   superset in
  let entries = Superset_risg.entries_of_isg insn_risg in
  let conflicts = Superset_risg.find_all_conflicts insn_map in
  let to_clamp = ref Addr.Set.empty in
  Hash_set.iter entries ~f:(fun entry -> 
      let b = ref false in
      let pre v = 
        if Addr.(v = entry) then
          b := false
        else if not (!b) then
          if Set.mem conflicts v then
            b := true
          else to_clamp := Set.add (!to_clamp) v
      in Superset_risg.Dfs.iter_component ~pre insn_risg entry;
    );
  !to_clamp

let extended_clamp superset = 
  let to_clamp = find_free_insns superset in
  let insn_map = Superset.get_map superset in
  let insn_risg = Superset.get_graph superset in
  (* TODO this doesn't merge with to_clamp, and the var names are misleading *)
  Set.fold to_clamp ~init:Addr.Set.empty ~f:(fun to_clamp clamp -> 
      let _, to_clamp = Superset_risg.Dfs.fold_component
          (fun addr (struck,to_clamp) ->
             if struck then (struck,to_clamp) else
               let conflicts = Superset_risg.conflicts_within_insn_at 
                   insn_map addr in
               let no_conflicts = Set.length conflicts = 0 in
               (*let conflicts = Superset_risg.parent_conflict_at
                   insn_risg insn_map addr in
                 let no_conflicts = Set.length conflicts = 0
                                  && no_conflicts in*)
               if no_conflicts then (struck, Set.(add to_clamp addr))
               else (true, to_clamp)
          ) (false, to_clamp) insn_risg clamp in to_clamp
    )

let extract_loop_addrs superset = 
  let insn_risg = Superset.get_graph superset in
  let loop_addrs = Superset_risg.StrongComponents.scc_list insn_risg in
  List.fold_left ~init:Addr.Map.empty loop_addrs
    ~f:(fun addrs loop ->
        if List.length loop >= 2 then
          Option.value ~default:addrs 
            Option.(map List.(hd loop) ~f:(fun addr -> 
                Map.set addrs addr loop))
        else addrs
      )

let extract_filtered_loop_addrs superset =
  let loop_addrs = extract_loop_addrs superset in
  Map.filteri loop_addrs ~f:(fun ~key ~data ->
      List.length data > 20)

let extract_constants superset =
  let width = Addr.bitwidth Superset.(get_base superset) in
  let s = Size.of_int_exn width in
  let addrs = Image.words Superset.(get_img superset) s in
  let insn_risg = Superset.get_graph superset in
  Seq.fold ~init:Addr.Map.empty Table.(to_sequence addrs) 
    ~f:(fun constants (m, constant) -> 
        if Superset.contains_addr superset constant
        && Superset_risg.G.(mem_vertex insn_risg constant) then
          Map.set constants Memory.(min_addr m) constant
        else constants
      )

let stddev_of hs average pmap = 
  let deviation,deg_free =
    Hash_set.fold ~init:(0.0,0) hs ~f:(fun (deviation,deg_free) addr -> 
        if Map.mem pmap addr then
          let d = (Option.(value_exn Map.(find pmap addr)) -. average) in
          let d = d *. d in
          (deviation +. d, (deg_free+1))
        else (deviation, (deg_free))
      ) in
  sqrt(deviation /. float_of_int (deg_free -1))

(* pre is called from descendant to ancestor order, so we want to
   check for usage and put that into a map, and then for define on
   post visitation, when coming back down from ancestors back to
   descendants (as execution would move). *)
let pre_ssa superset lift factors var_use addr =
  match Map.find (Superset.get_map superset) addr with
  | Some (mem, insn) ->
    let bil = lift (mem, insn) in
    Option.value_map ~default:() bil ~f:(fun (mem,bil) -> 
        let use_vars = Abstract_ssa.use_ssa bil in
        Set.iter use_vars ~f:(fun use_var -> 
            var_use := Map.set !var_use use_var addr
          )
      )
  | None -> ()

let pre_freevarssa superset lift factors var_use addr =
  match Map.find (Superset.get_map superset) addr with
  | Some (mem, insn) ->
    let bil = lift (mem, insn) in
    Option.value_map ~default:() bil ~f:(fun (mem,bil) -> 
        let use_vars = Abstract_ssa.use_freevars bil in
        Set.iter use_vars ~f:(fun use_var -> 
            var_use := Map.set !var_use use_var addr
          )
      )
  | None -> ()

let post_ssa_with superset lift var_use addr f = 
  match Map.find (Superset.get_map superset) addr with
  | Some (mem, insn) ->
    let bil = lift (mem, insn) in
    Option.value_map ~default:() bil ~f:(fun (mem,bil) -> 
        let use_vars = Abstract_ssa.use_ssa bil in
        Set.iter use_vars ~f:(fun use_var -> 
            var_use := Map.remove !var_use use_var;
          );
        let var_defs = Abstract_ssa.def_ssa bil in
        Set.iter var_defs ~f:(fun var_def -> 
            match Map.find !var_use var_def with
            | Some(waddr) ->
              if not Addr.(waddr = addr) then (
                f waddr addr
              )
            | None -> ()
          );
        Set.iter var_defs ~f:(fun write_reg -> 
            var_use := Map.remove !var_use write_reg
          )
      )
  | None -> ()

let post_freevarssa_with superset lift var_use addr f = 
  match Map.find (Superset.get_map superset) addr with
  | Some (mem, insn) ->
    let bil = lift (mem, insn) in
    Option.value_map ~default:() bil ~f:(fun (mem,bil) -> 
        let use_vars = Abstract_ssa.use_freevars bil in
        let var_defs = Abstract_ssa.def_freevars bil in
        Set.iter var_defs ~f:(fun var_def -> 
            match Map.find !var_use var_def with
            | Some(waddr) ->
              if not Set.(mem use_vars var_def) then (
                f waddr addr
              )
            | None -> ()
          );
        Set.iter use_vars ~f:(fun use_var -> 
            var_use := Map.remove !var_use use_var;
          );
        Set.iter var_defs ~f:(fun write_reg -> 
            var_use := Map.remove !var_use write_reg
          )
      )
  | None -> ()

let extract_ssa_to_map superset =
  let insn_risg = Superset.get_graph superset in
  let var_use = ref Exp.Map.empty in
  let defuse_map = ref Addr.Map.empty in
  let add_to_map def use = 
    defuse_map := Map.set !defuse_map def use in
  let module Target = (val target_of_arch 
                          Superset.(get_arch superset)) in  
  let lift (mem, insn) = 
    try Superset.lift_insn Target.lift (mem,insn) with _ -> None in
  let pre = pre_ssa superset lift () var_use in
  let post addr = post_ssa_with superset lift var_use
      addr add_to_map in
  let entries = Superset_risg.entries_of_isg insn_risg in
  Hash_set.iter entries ~f:(fun addr -> 
      Superset_risg.Dfs.iter_component ~pre ~post insn_risg addr;
      var_use := Exp.Map.empty
    );
  !defuse_map

let extract_freevarssa_to_map superset =
  let insn_risg = Superset.get_graph superset in
  let var_use = ref Var.Map.empty in
  let defuse_map = ref Addr.Map.empty in
  let add_to_map def use = 
    defuse_map := Map.set !defuse_map def use in
  let module Target = (val target_of_arch 
                          Superset.(get_arch superset)) in  
  let lift (mem, insn) = 
    try Superset.lift_insn Target.lift (mem,insn) with _ -> None in
  let pre = pre_freevarssa superset lift () var_use in
  let post addr = post_freevarssa_with superset lift var_use
      addr add_to_map in
  let entries = Superset_risg.entries_of_isg insn_risg in
  Hash_set.iter entries ~f:(fun addr -> 
      Superset_risg.Dfs.iter_component ~pre ~post insn_risg addr;
      var_use := Var.Map.empty
    );
  !defuse_map



type window = (addr * (mem * Dis.full_insn option)) list

let extract_cross_section_jmps superset = 
  let insn_risg = Superset.get_graph superset in
  let segments = Superset.get_segments superset in
  let cross_section_edges = Superset_risg.G.fold_edges
      (fun src dst csedges -> 
         let s1 = Table.find_addr segments src in
         let s2 = Table.find_addr segments dst in
         match s1, s2 with
         | Some (m1,_), Some (m2,_) ->
           let a1 = Memory.(min_addr m1) in
           let a2 = Memory.(min_addr m2) in
           if not Addr.(a1 = a2) then
             let ft1 = Superset.is_fall_through superset src dst in
             let ft2 = Superset.is_fall_through superset dst src in
             if (ft1 || ft2) then (
               Superset_risg.G.remove_edge insn_risg src dst;
               Map.set csedges src dst
             ) else csedges
           else csedges
         | _, _ -> csedges
      ) insn_risg Addr.Map.empty in
  cross_section_edges

let extract_trim_clamped superset = 
  let to_clamp = find_free_insns superset in
  let insn_risg = Superset.get_graph superset in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let insn_isg = Superset_risg.Oper.mirror insn_risg in
  Set.iter to_clamp ~f:(fun c -> 
      if not Hash_set.(mem visited c) then
        if Superset_risg.G.mem_vertex insn_isg c then (
          Superset.mark_descendents_at
            ~insn_isg ~visited ~datas superset c
        )
    );
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Set.(mem to_clamp d) then
        Superset.clear_bad superset d
    );
  Markup.check_convergence superset visited;
  superset

let time ?(name="") f x =
  let t = Sys.time() in
  let fx = f x in
  let s = sprintf "%s execution time: %fs\n" name (Sys.time() -. t) in
  print_endline s;
  fx
  
let extract_trim_limited_clamped superset = 
  let visited = Addr.Hash_set.create () in
  let callsites = Superset.get_callsites ~threshold:0 superset in
  let f s = Grammar.tag_callsites visited ~callsites s in
  let superset = time ~name:"tagging callsites: " f superset in
  let () = Markup.clear_bad superset in
  let superset = time ~name:"extract_trim_clamped "
                   extract_trim_clamped superset in
  Markup.check_convergence superset visited;
  superset

let fixpoint_descendants superset extractf depth = 
  let insn_isg = 
    Superset_risg.Oper.mirror Superset.(get_graph superset) in
  let rec fix_descendants cur_features d =
    if d >= depth then
      cur_features
    else
      let visited = Addr.Hash_set.create () in
      let subset_features = Addr.Hash_set.create () in
      Hash_set.iter cur_features ~f:(fun cur ->
          if not Hash_set.(mem visited cur) then
            Superset_risg.iter_component ~pre:(fun v ->
                if Hash_set.(mem cur_features v)
                && not Addr.(cur = v) then
                  Hash_set.add subset_features v
              ) ~visited insn_isg cur
          else Hash_set.add subset_features cur
        );
      fix_descendants subset_features (d+1)
  in
  let cur_features = extractf superset in
  fix_descendants cur_features 0

let fixpoint_map superset feature_pmap = 
  let insn_isg = 
    Superset_risg.Oper.mirror Superset.(get_graph superset) in
  let visited = Addr.Hash_set.create () in
  let entries = Superset_risg.entries_of_isg insn_isg in
  Hash_set.fold ~init:feature_pmap entries ~f:(fun feature_pmap cur -> 
      if not Hash_set.(mem visited cur) then
        let prev = ref [] in
        let feature_pmap = ref feature_pmap in
        Superset_risg.iter_component ~pre:(fun v ->
            match Map.find !feature_pmap v with
            | None -> ()
            | Some(p) ->
              prev :=  List.append p  !prev;
              feature_pmap := Map.set !feature_pmap v !prev;
          ) ~visited insn_isg cur;
        !feature_pmap
      else feature_pmap
    )

let fixpoint_grammar superset depth = 
  let extractf superset = 
    let insn_risg = Superset.get_graph superset in
    Superset_risg.get_branches insn_risg in
  fixpoint_descendants superset extractf depth

let fixpoint_ssa superset depth = 
  let extractf superset = 
    let ssa_map = extract_ssa_to_map superset in
    let ssa = Addr.Hash_set.create () in
    List.iter Map.(data ssa_map) ~f:Hash_set.(add ssa);
    ssa in
  fixpoint_descendants superset extractf depth

let fixpoint_freevarssa superset depth = 
  let extractf superset = 
    let freevars_map = extract_freevarssa_to_map superset in
    let freevars = Addr.Hash_set.create () in
    List.iter Map.(data freevars_map) ~f:Hash_set.(add freevars);
    freevars in
  fixpoint_descendants superset extractf depth

let fixpoint_tails superset = 
  let insn_risg = Superset.(get_graph superset) in
  let extractf superset =
    let insn_isg = 
      Superset_risg.Oper.mirror insn_risg in
    let insn_map = Superset.get_map superset in
    let conflicts = Superset_risg.find_all_conflicts insn_map in
    let tails_map = 
      Decision_tree_set.tails_of_conflicts conflicts insn_isg in 
    let tails = Addr.Hash_set.create () in
    List.iter Map.(keys tails_map) ~f:Hash_set.(add tails);
    tails
  in
  fixpoint_descendants superset extractf 4

let allfeatures = 
  "RestrictedClamped"      ::
  "ExtendedClamped"        ::
  "ClassicGrammar"         ::
  "LinearGrammar"          ::
  "UnfilteredGrammar"      ::
  "FalseBranchMap"         ::
  "FilteredFalseBranchMap" ::
  "UnfilteredSCC"          ::
  "UnionFindBranches"      :: (* TODO *)
  "UnionFindCompatible"    :: (* TODO *)
  "FreeVarSSA"             ::
  "MirrorSCC"              ::
  "JmpTargetIntersection"  :: (* TODO *)
  "FixpointCallsites"      :: (* TODO *)   
  "FixpointGrammar"        :: 
  "FixpointSSA"            :: (* TODO *)
  "FixpointFreevarSSA"     :: 
  "FixpointTails"          :: 
  default_features

type 'a extractor = ('a Superset.t -> Addr.Set.t)
type ('a,'b) mapextractor = ('a Superset.t -> 'b Addr.Map.t)
type 'a setfilter = ('a Superset.t -> Addr.Set.t -> Addr.Set.t)
type ('a, 'b) mapfilter = ('a Superset.t -> 'b Addr.Map.t -> 'b Addr.Map.t)
type 'a setexfilt = 'a extractor * 'a setfilter
type ('a, 'b) mapexfilt = ('a,'b) mapextractor * ('a, 'b) mapfilter
let unfiltered _ = ident
let exfiltset : (unit setexfilt) String.Map.t = String.Map.empty
let exfiltmap : ((unit, Addr.t) mapexfilt) String.Map.t = String.Map.empty

let exfiltset = String.Map.set exfiltset "FixpointGrammar"
    ((fun x -> transform (fixpoint_grammar x 0)), unfiltered)
let exfiltset = String.Map.set exfiltset "FixpointTails"
    ((fun x -> transform (fixpoint_tails x)), unfiltered)
let exfiltset = String.Map.set exfiltset "FixpointFreevarSSA"
    ((fun x -> transform (fixpoint_freevarssa x 0)), unfiltered)
let exfiltmap = String.Map.set
    exfiltmap "SSA" (extract_ssa_to_map, unfiltered)
let get_branches superset = 
  let insn_risg = Superset.get_graph superset in
  let branches = Superset_risg.get_branches insn_risg in
  transform branches
let branch_map_of_branches superset branches =
  let insn_risg = Superset.get_graph superset in
  let img = Superset.get_img superset in
  let name = Option.value_exn Image.(filename img) in
  let true_positives = Metrics.true_positives superset name in
  let branches = 
    Hash_set.fold true_positives ~init:branches ~f:Set.remove in
  Set.fold branches ~init:Addr.Map.empty ~f:(fun fpbranchmap fpbranch ->
      let target = 
        List.find_exn Superset_risg.G.(pred insn_risg fpbranch) 
          ~f:Superset.(is_fall_through superset fpbranch) in
      Map.set fpbranchmap fpbranch target
    )
let extract_fp_branches superset = 
  let branches = get_branches superset in
  branch_map_of_branches superset branches
let exfiltmap = String.Map.set
    exfiltmap "FalseBranchMap" (extract_fp_branches, unfiltered)
let extract_fp_branches superset = 
  let branches = get_branches superset in
  branch_map_of_branches superset branches
let extract_filter_fp_branches superset =
  let superset = Invariants.tag_layer_violations superset in
  let violations = Markup.collect_bad superset in
  let _ = Markup.clear_bad superset in
  let branches = get_branches superset in
  let branches = Set.diff branches (transform violations) in
  let superset = Invariants.tag_branch_violations superset in
  let violations = Markup.collect_bad superset in
  let _ = Markup.clear_bad superset in
  let branches = Set.diff branches (transform violations) in
  branch_map_of_branches superset branches
let exfiltmap = String.Map.set
    exfiltmap "FilteredFalseBranchMap" (extract_filter_fp_branches, unfiltered)
let exfiltmap = String.Map.set
    exfiltmap "FreeVarSSA" (extract_freevarssa_to_map, unfiltered)
let exfiltmap = String.Map.set
    exfiltmap "SSA" (extract_ssa_to_map, unfiltered)
let linear_grammar superset =
  let insn_risg = Superset.get_graph superset in
  let entries = Superset_risg.entries_of_isg insn_risg in
  transform Grammar.(linear_branch_sweep superset entries)
let exfiltset = String.Map.set
    exfiltset "LinearGrammar" (linear_grammar, unfiltered)
let exfiltset = String.Map.set
    exfiltset "UnfilteredGrammar" (get_branches, unfiltered)
let classic_grammar superset =
  transform Grammar.(identify_branches superset)
let branch_violations superset = 
  let superset = Invariants.tag_branch_violations superset in
  let violations = Markup.collect_bad superset in
  let _ = Markup.clear_bad superset in
  transform violations
let exfiltset = String.Map.set
    exfiltset "BranchViolations" (branch_violations, unfiltered)
let layer_violations superset = 
  let superset = Invariants.tag_layer_violations superset in
  let violations = Markup.collect_bad superset in
  let _ = Markup.clear_bad superset in
  transform violations
let exfiltset = String.Map.set
    exfiltset "LayerViolations" (layer_violations, unfiltered)
let filtered_grammar superset = 
  let violations = (layer_violations superset) in
  let branches = Set.diff (get_branches superset) violations in
  Set.diff branches (branch_violations superset)
let exfiltset = String.Map.set
    exfiltset "FilteredGrammar" (filtered_grammar, unfiltered)
let loop_grammar superset =
  let superset = Invariants.tag_layer_violations superset in
  let violations = Markup.collect_bad superset in
  let _ = Markup.clear_bad superset in
  let branches = get_branches superset in
  let branches = Set.diff branches (transform violations) in
  let superset = Invariants.tag_branch_violations superset in
  let violations = Markup.collect_bad superset in
  let _ = Markup.clear_bad superset in
  let branches = Set.diff branches (transform violations) in
  let loop_addrs = extract_loop_addrs superset in
  let loop_addrs = 
    Map.fold ~init:Addr.Set.empty loop_addrs ~f:(fun ~key ~data addrs -> 
        if List.length data >= 2 then
          List.fold ~init:addrs data ~f:Set.add
        else addrs
      ) in
  Set.filter branches ~f:(fun x -> Set.(mem loop_addrs x))
let exfiltset = String.Map.set
    exfiltset "LoopGrammar" (loop_grammar, unfiltered)
let exfiltset = String.Map.set
    exfiltset "ClassicGrammar" (classic_grammar, unfiltered)
let exfiltset = String.Map.set
    exfiltset "Callsites3" ((fun x -> transform (Superset.get_callsites ~threshold:6 x)), unfiltered)
let exfiltset = String.Map.set
    exfiltset "Clamped" (find_free_insns, unfiltered)
let exfiltset = String.Map.set
    exfiltset "RestrictedClamped" (restricted_clamp, unfiltered)
let exfiltset = String.Map.set
    exfiltset "ExtendedClamped" (extended_clamp, unfiltered)
let extract_loops superset = 
  let loop_addrs = extract_loop_addrs superset in
  Map.fold ~init:Addr.Set.empty loop_addrs ~f:(fun ~key ~data addrs -> 
      if List.length data >= 2 then
        List.fold ~init:addrs data ~f:Set.add
      else addrs
    )
let exfiltset = String.Map.set
    exfiltset "UnfilteredSCC" (extract_loops,unfiltered)
let extract_filter_loops superset = 
  let loop_addrs = extract_filtered_loop_addrs superset in
  Map.fold ~init:Addr.Set.empty loop_addrs ~f:(fun ~key ~data addrs -> 
      List.fold ~init:addrs data ~f:Set.add
    )  
let extract_loops_with_break superset =
  let loop_addrs = extract_loop_addrs superset in
  let insn_risg = Superset.get_graph superset in
  Map.fold ~init:Addr.Set.empty loop_addrs ~f:(fun ~key ~data loops -> 
      let loop = List.fold ~init:Addr.Set.empty data ~f:Set.add in
      let has_break = Seq.exists Seq.(of_list data)
          ~f:(fun addr -> 
              let targets = Superset_risg.G.pred insn_risg addr in
              Seq.exists Seq.(of_list targets) 
                ~f:(fun x -> not Set.(mem loop x))
            ) in
      if has_break then Set.union loops loop else loops
    ) 
let exfiltset = String.Map.set
    exfiltset "LoopsWithBreak" (extract_loops_with_break,unfiltered)
let exfiltset = String.Map.set
    exfiltset "SCC" (extract_filter_loops,unfiltered)
let extract_mirror_filter_loops superset = 
  let insn_risg = Superset.get_graph superset in
  let insn_risg = Superset_risg.Oper.mirror insn_risg in
  let superset = Superset.rebuild ~insn_risg superset in
  let loop_addrs = extract_filtered_loop_addrs superset in
  Map.fold ~init:Addr.Set.empty loop_addrs ~f:(fun ~key ~data addrs -> 
      List.fold ~init:addrs data ~f:Set.add
    )  
let exfiltset = String.Map.set
    exfiltset "MirrorSCC" (extract_mirror_filter_loops,unfiltered)
let extract_constants_to_set superset = 
  let constants = extract_constants superset in
  Map.fold constants ~init:Addr.Set.empty ~f:(fun ~key ~data consts -> 
      Set.add consts data
    )
let extract_exitless superset = 
  let returned = Addr.Hash_set.create () in
  let insn_risg = Superset.get_graph superset in
  let entries = Superset_risg.entries_of_isg insn_risg in
  Hash_set.iter entries ~f:(fun entry -> 
      Superset_risg.iter_component insn_risg
        ~pre:(Hash_set.add returned) entry
    );
  Superset_risg.G.fold_vertex (fun v exitless -> 
      if not (Hash_set.mem returned v) 
      then Set.add exitless v else exitless
    ) insn_risg Addr.Set.empty


let collect_descendants superset ?insn_isg ?visited ?datas targets = 
  let visited = Option.value visited ~default:(Addr.Hash_set.create ()) in
  let datas = Option.value datas ~default:(Addr.Hash_set.create ()) in
  let insn_isg = match insn_isg with
    | None -> 
      let insn_risg = Superset.get_graph superset in
      Superset_risg.Oper.mirror insn_risg 
    | Some insn_isg -> insn_isg in
  Hash_set.iter targets ~f:(fun v -> 
      if not Hash_set.(mem visited v) then
        Superset.mark_descendents_at ~insn_isg ~visited ~datas superset v      
    )

let exfiltset = String.Map.set
    exfiltset "NoExit" (extract_exitless, unfiltered)
let exfiltset = String.Map.set
    exfiltset "Constant" (extract_constants_to_set,unfiltered)
let extract_union_find_compatible superset =  
  Addr.Set.empty
(* TODO iterate over the superset and split it into the set of items
    that can be merged together tenatively. Add the clamped, constants and
    unfiltered grammar. For all added, maintain an Int.Map from union
    find id to number of features.  *)
let exfiltset = String.Map.set
    exfiltset "UnionFindCompatible" (extract_union_find_compatible,unfiltered)
let extract_union_find_branches superset =
  (*let insn_risg = Superset.get_graph superset in
    let insn_map  = Superset.get_map superset in
    let branches = Superset_risg.get_branches insn_risg in
    let components =
    Superset_risg.DiscreteComponents.components_list insn_risg in*)
  Addr.Set.empty
(*List.fold ~init:Addr.Set.empty components 
  ~f:(fun (compatible) component -> 
      List.fold component ~init:(compatible) 
        ~f:(fun (insns,datas) addr -> 
            let insns = Union_find.(create addr) :: insns in
            let conflicts = 
              Seq.filter ~f:(Map.mem insn_map)
                Superset_risg.(conflict_seq_at insn_map addr) in
            let datas = 
              Seq.fold ~init:datas conflicts ~f:(fun datas conflict ->
                  (Union_find.create conflict) :: datas) in
            insns, datas
          )
    )*)
let exfiltset = String.Map.set
    exfiltset "UnionFindBranches" (extract_union_find_branches,unfiltered)
let extract_img_entry superset = 
  let img = Superset.get_img superset in
  let s = sprintf "entry: %s" 
      Addr.(to_string Image.(entry_point img)) in
  print_endline s;
  Set.add Addr.Set.empty Image.(entry_point img)
let exfiltset = String.Map.set
    exfiltset "ImgEntry" (extract_img_entry, unfiltered)
let extract_trim_callsites superset =
  let visited = Addr.Hash_set.create () in
  let callsites = Superset.get_callsites ~threshold:2 superset in
  let protection = Superset.get_callsites ~threshold:0 superset in
  collect_descendants superset ~visited protection;
  Markup.clear_bad superset;
  let superset = Grammar.tag_callsites visited ~callsites superset in
  Markup.check_convergence superset visited;
  superset
let extract_trim_loops_with_break superset = 
  (*let loops = extract_loops_with_break superset in*)
  superset
let extract_trim_entry superset =
  let imgentry = extract_img_entry superset in
  Set.iter imgentry ~f:Superset.(mark_descendents_at superset);
  superset
let extract_trim_branch_violations superset = 
  Invariants.tag_branch_violations superset
let extract_trim_layer_violations superset =
  Invariants.tag_layer_violations superset
let extract_trim_noexit superset =
  let exitless = extract_exitless superset in
  Set.iter exitless ~f:Superset.(mark_bad superset);
  superset

let extract_trim_fixpoint_grammar superset =
  let gdesc = fixpoint_grammar superset 10 in
  let insn_risg = Superset.get_graph superset in
  let insn_isg  = Superset_risg.Oper.mirror insn_risg in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let callsites = Superset.get_callsites ~threshold:0 superset in
  let superset = Grammar.tag_callsites visited ~callsites superset in
  Markup.clear_bad superset;
  collect_descendants ~visited ~insn_isg superset gdesc;
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Hash_set.(mem gdesc d) then
        Superset.clear_bad superset d
    );
  Markup.check_convergence superset visited;
  Markup.check_convergence superset gdesc;
  superset  

let extract_trim_fixpoint_ssa superset =
  let gdesc = fixpoint_ssa superset 6 in
  let insn_risg = Superset.get_graph superset in
  let insn_isg  = Superset_risg.Oper.mirror insn_risg in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let callsites = Superset.get_callsites ~threshold:0 superset in
  (*collect_descendants ~visited ~insn_isg superset callsites;*)
  let superset = Grammar.tag_callsites visited ~callsites superset in
  Markup.clear_bad superset;
  collect_descendants ~visited ~insn_isg superset gdesc;
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Hash_set.(mem gdesc d) then
        Superset.clear_bad superset d
    );
  Markup.check_convergence superset visited;
  superset  

let extract_trim_fixpoint_freevarssa superset =
  let gdesc = fixpoint_freevarssa superset 6 in
  let insn_risg = Superset.get_graph superset in
  let insn_isg  = Superset_risg.Oper.mirror insn_risg in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let callsites = Superset.get_callsites ~threshold:0 superset in
  (*collect_descendants ~visited ~insn_isg superset callsites;*)
  let superset = Grammar.tag_callsites visited ~callsites superset in
  Markup.clear_bad superset;
  collect_descendants ~visited ~insn_isg superset gdesc;
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Hash_set.(mem gdesc d) then
        Superset.clear_bad superset d
    );
  Markup.check_convergence superset visited;
  superset

let extract_trim_fixpoint_tails superset = 
  let tdesc = fixpoint_tails superset in
  let insn_risg = Superset.get_graph superset in
  let insn_isg  = Superset_risg.Oper.mirror insn_risg in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let callsites = Superset.get_callsites ~threshold:0 superset in
  let superset = Grammar.tag_callsites visited ~callsites superset in
  Markup.clear_bad superset;
  Hash_set.iter tdesc ~f:(fun v -> 
      if not Hash_set.(mem visited v) then
        Superset.mark_descendents_at ~insn_isg ~visited ~datas superset v
    );
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Hash_set.(mem tdesc d) then
        Superset.clear_bad superset d
    );
  Markup.check_convergence superset visited;
  superset

type fkind = 
  | AppSuperset
  (*| AppFactors
    | AppFactorGraph*)

let discard_edges superset =
  let g = Superset.get_graph superset in
  let insn_risg = Superset.get_graph superset in
  let insn_map  = Superset.get_map superset in
  Superset_risg.G.iter_edges
    (fun child parent -> 
       if not Superset.(is_fall_through superset parent child) then
         match Map.find insn_map parent with
         | None -> ()
         | Some (mem, insn) -> 
           match insn with 
           | Some(insn) -> 
             let insn = Insn.of_basic insn in
             if Insn.(is Insn.call insn) then
               Superset_risg.G.remove_edge insn_risg child parent
           | None -> ()
    ) g;
  (*let edges = Superset.get_non_fall_through_edges superset in*)
  superset

let featuremap : ((unit Superset.t -> unit Superset.t) * fkind) String.Map.t = String.Map.empty
let featuremap = String.Map.set
    featuremap "Callsites3" (extract_trim_callsites,AppSuperset)
let featuremap = String.Map.set
    featuremap "DiscardEdges" (discard_edges,AppSuperset)
let featuremap = String.Map.set
    featuremap "LoopsWithBreak" (extract_trim_loops_with_break,AppSuperset)
let featuremap = String.Map.set
    featuremap "ImgEntry" (extract_trim_entry,AppSuperset)
let featuremap = String.Map.set
    featuremap "BranchViolations" (extract_trim_branch_violations,AppSuperset)
let featuremap = String.Map.set
    featuremap "LayerViolations" (extract_trim_layer_violations,AppSuperset)
(*let featuremap = String.Map.set
    featuremap "SCC" (extract_tag_loops,AppFactors)*)
let featuremap = String.Map.set
    featuremap "NoExit" (extract_trim_noexit,AppSuperset)
let featuremap = String.Map.set
    featuremap "TrimClamped" (extract_trim_clamped,AppSuperset)
let featuremap = String.Map.set
    featuremap "TrimLimitedClamped"
    (extract_trim_limited_clamped,AppSuperset)
let featuremap = String.Map.set
    featuremap "TrimFixpointGrammar"
    (extract_trim_fixpoint_grammar,AppSuperset)
let featuremap = String.Map.set
    featuremap "TrimFixpointSSA"
    (extract_trim_fixpoint_ssa,AppSuperset)
let featuremap = String.Map.set
    featuremap "TrimFixpointFreevarSSA"
    (extract_trim_fixpoint_freevarssa,AppSuperset)
let featuremap = String.Map.set
    featuremap "TrimFixpointTails"
    (extract_trim_fixpoint_tails,AppSuperset)

let apply_featureset featureset superset = 
  let superset = List.fold ~init:(superset) featureset ~f:(fun (superset) feature -> 
      match Map.(find featuremap feature) with
      | None -> superset
      | Some (f, AppSuperset) -> 
        print_endline feature;
        let superset = f superset in
        let superset = Trim.Default.trim superset in
        superset
    ) in
  superset

let fdists = String.Map.empty
let fdists = String.Map.set fdists "FixpointGrammar" 5
let fdists = String.Map.set fdists "FixpointFreevarSSA" 3

let make_featurepmap featureset superset = 
  List.fold ~f:(fun (feature_pmap) feature -> 
      let p = Map.find fdists feature in
      let p = Option.value p ~default:2 in
      match Map.(find exfiltset feature) with
      | None -> feature_pmap
      | Some (extract,filter) -> 
        print_endline feature;
        let fset = extract superset in
        Set.fold fset ~init:feature_pmap 
          ~f:(fun feature_pmap x -> 
              Map.update feature_pmap x ~f:(function 
                  | Some l ->  (p, x, feature) :: l
                  | None -> [(p, x, feature)]
                )
            )
    ) ~init:Addr.Map.empty featureset

let total_of_features l =
  List.fold ~init:0 ~f:(fun x (y,_,_) -> x + y) l

let apply_featurepmap featureset ?(threshold=50) superset =
  let feature_pmap = make_featurepmap featureset superset in
  let feature_pmap = fixpoint_map superset feature_pmap in
  let feature_pmap = 
    Map.map feature_pmap ~f:(total_of_features) in
  let feature_pmap = 
    Map.filter feature_pmap (fun total -> total > threshold) in
  let visited = Addr.Hash_set.create () in
  let insn_risg = Superset.get_graph superset in
  let insn_isg = Superset_risg.Oper.mirror insn_risg in
  let callsites = Superset.get_callsites ~threshold:0 superset in
  let superset = Grammar.tag_callsites visited ~callsites superset in
  Markup.clear_bad superset;
  List.iter Map.(keys feature_pmap) ~f:(fun addr -> 
      Superset.mark_descendents_at superset ~insn_isg ~visited addr
    );
  Markup.check_convergence superset visited;
  superset
(*Trim.trim superset*)

(*let collect_metrics superset tps pmap = 
  let avg hs =
    let avg = 
      Hash_set.fold hs ~init:(0.0) ~f:(fun (avg) tp -> 
          let tp = Option.value Map.(find pmap tp) ~default:0.0 in
          avg +. tp
        ) in
    avg /. (float_of_int Hash_set.(length hs)) in
  let avg_tp = avg tps in
  let min_tp, max_tp = 
    Map.fold pmap ~init:(None, None) 
      ~f:(fun ~key ~data (min_tp, max_tp) ->
          let p = data in
          if Hash_set.mem tps key then
            let min_tp = min p Option.(value min_tp ~default:p) in
            let max_tp = max p Option.(value max_tp ~default:p) in
            (Some min_tp, Some max_tp)
          else (min_tp, max_tp)
        ) in
  let min_tp = Option.value min_tp ~default:0.0 in
  let max_tp = Option.value max_tp ~default:0.0 in
  let true_positives  = Hash_set.(length tps) in
  let ro = Metrics.reduced_occlusion superset tps in
  let fps = Metrics.false_positives superset ro in
  let removed = Markup.collect_bad superset in
  let fps = Hash_set.filter fps ~f:(fun x -> not Hash_set.(mem removed x)) in
  let tps = Hash_set.filter tps ~f:(fun x -> not Hash_set.(mem removed x)) in
  let avg_fp = avg fps in
  let min_fp, max_fp = 
    Map.fold pmap ~init:(None, None) 
      ~f:(fun ~key ~data (min_fp, max_fp) ->
          let p = data in
          if Hash_set.mem fps key then
            let min_fp = min p Option.(value min_fp ~default:p) in
            let max_fp = max p Option.(value max_fp ~default:p) in
            (Some min_fp, Some max_fp)
          else (min_fp, max_fp)
        ) in
  let min_fp = Option.value min_fp ~default:0.0 in
  let max_fp = Option.value max_fp ~default:0.0 in
  let false_positives = Hash_set.(length fps) in
  let tp_std_dev = stddev_of tps avg_tp pmap in
  let fp_std_dev = stddev_of fps avg_fp pmap in
  let insn_risg = Superset.get_graph superset in
  let detected_insns = 
    Superset_risg.G.fold_vertex 
      (fun vert detected_insns -> Set.add detected_insns vert) 
      insn_risg Addr.Set.empty in
  let tps = Hash_set.fold ~init:Addr.Set.empty tps ~f:(Set.add) in
  let false_negatives = Set.length Set.(diff tps detected_insns) in
  ()
*)
