open Core
open Bap.Std

module Dis = Disasm_expert

module type Heurism = sig
  type t
  val name : string
  val impl : Superset.t -> t
end

module HeurismSet(H : Heurism) = struct
  open Bap_knowledge
  open Bap_core_theory

  module Cache = struct
    open H
    let package = "superset-heuristics"
    let addrs_t =
      Knowledge.Domain.optional
        ~inspect:Addr.Hash_set.sexp_of_t
        ~equal:Addr.Hash_set.equal "addr.set"

    let addrs_persistent =
      Knowledge.Persistent.of_binable
        (module struct type t = Addr.Hash_set.t option [@@deriving bin_io] end)
      
    let attr ty persistent desc =
      let open Theory.Program in
      Knowledge.Class.property ~package cls name ty
        ~persistent ~public:true ~desc
    let locations =
      attr addrs_t addrs_persistent 
        ("addresses of all sites of " ^ name ^ " heuristic")
  end
end

let defaults = [
  "ImgEntry";
  (*"NoExit";*)
  (*"LoopsWithBreak";*)
  "BranchViolations";
  (*"LayerViolations";*)
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
let defaults = List.rev defaults

let transform = Hash_set.fold ~init:Addr.Set.empty ~f:Set.add

(** A callsite is a location which is shared as the target of a call
    by several other locations in the binary. *)
let get_callsites ?(threshold=6) superset =
  let callsites = Addr.Hash_set.create () in
  Superset.ISG.iter_vertex superset
    (fun v -> 
       let callers = Superset.ISG.ancestors superset v in
       let num_callers = 
         List.fold callers ~init:0 ~f:(fun total caller -> 
             if not (Superset.is_fall_through superset caller v) then
               total + 1
             else total) in
       if num_callers > threshold then (
         Hash_set.add callsites v;
       )
    ) ;
  callsites

(** Adds to the set visited the set of reachable descendents of a
    callsite of a given sufficient threshold number of external
    callers *)
let tag_callsites visited ?callsites superset =
  let callsites = Option.value callsites 
      ~default:(get_callsites superset) in
  Hash_set.iter callsites ~f:(fun callsite ->
      Traverse.with_descendents_at ~visited
        (* TODO ~pre should mark insn bodies as data *)
        ?post:None ?pre:None superset callsite;
    );
  superset

let find_free_insns superset = 
  let mem = Superset.Core.mem superset in
  let to_clamp =
    Superset.Core.fold superset ~init:([])
      ~f:(fun ~key ~data to_clamp ->
          let (addr,(memory,_)) = key, data in
          let len = Memory.length memory in
          let conflicts = Superset.Occlusion.range_seq_of_conflicts
              ~mem addr len in
          let no_conflicts = Seq.is_empty conflicts in
          if no_conflicts then
            addr :: to_clamp
          else (
            to_clamp
          )
        ) in
  let to_clamp = Addr.Set.of_list to_clamp in
  Set.diff to_clamp @@
    Superset.Occlusion.find_all_conflicts superset

let restricted_clamp superset = 
  let entries = Superset.entries_of_isg superset in
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
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
      in Traverse.with_ancestors_at ~post:(fun _ -> ()) ~pre superset entry;
    );
  !to_clamp

let extended_clamp superset = 
  let to_clamp = find_free_insns superset in
  Set.fold to_clamp ~init:Addr.Set.empty ~f:(fun to_clamp clamp -> 
      let _, to_clamp =
        Superset.ISG.dfs_fold superset
          ~pre:(fun (struck,to_clamp) addr ->
            if struck then (struck,to_clamp) else
              let conflicts =
                Superset.Occlusion.conflicts_within_insn_at
                  superset addr in
              let no_conflicts = Set.length conflicts = 0 in
              (*let conflicts = Superset.Occlusion.parent_conflict_at
                     insn_risg insn_map addr in
                   let no_conflicts = Set.length conflicts = 0
                                    && no_conflicts in*)
              if no_conflicts then (struck, Set.(add to_clamp addr))
              else (true, to_clamp)
          ) ~post:(fun x _ -> x) (false, to_clamp) clamp 
      in to_clamp
    )

let extract_loop_addrs superset = 
  let loop_addrs = Superset.ISG.raw_loops superset in
  List.fold_left ~init:Addr.Map.empty loop_addrs
    ~f:(fun addrs loop ->
        if List.length loop >= 2 then
          Option.value ~default:addrs 
            Option.(map List.(hd loop) ~f:(fun addr -> 
                Map.set addrs ~key:addr ~data:loop))
        else addrs
      )

let extract_filtered_loop_addrs superset =
  let loop_addrs = extract_loop_addrs superset in
  Map.filteri loop_addrs ~f:(fun ~key ~data ->
      List.length data > 20)

let extract_constants superset =
 let imgmem =
    Memmap.to_sequence @@ Superset.Inspection.get_memmap superset in
  let addrs =
    Seq.bind imgmem
      ~f:(fun (segment,_) ->
          let words_of_mem mem =
            let rec yield_next addr =
              let width = Addr.bitwidth addr in
              let s = Size.of_int_exn width in
              let open Seq.Generator in
              match Memory.view ~word_size:s ~from:addr mem with
              | Ok next ->
                yield next >>= fun () -> yield_next (Addr.succ addr)
              | _ -> return () in
            Sequence.Generator.run (yield_next Memory.(min_addr mem))
          in words_of_mem segment
        ) in
  Seq.fold ~init:Addr.Map.empty  addrs
    ~f:(fun constants m ->
        let constant = Memory.(m ^ (min_addr m)) in
        match constant with
        | Ok constant -> 
          if Superset.Inspection.contains_addr superset constant
          && Superset.Core.(mem superset constant) then
            Map.set constants ~key:Memory.(min_addr m) ~data:constant
          else constants
        | _ -> constants
      )
  
let extract_cross_section_jmps superset = 
  let segments = Superset.Inspection.get_memmap superset in
  let cross_section_edges = Superset.ISG.fold_edges superset
      (fun src dst csedges ->
         let collect_minaddrs addr =
           let seg = Memmap.lookup segments addr in
           Seq.fold seg ~init:Addr.Set.empty ~f:(fun s1 (mem,_) ->
               Set.add s1 @@ Memory.min_addr mem
             ) in
         let s1 = collect_minaddrs src in
         let s2 = collect_minaddrs dst in
         if not (Set.(length @@ inter s1 s2) >= 1) then
           let ft1 = Superset.is_fall_through superset src dst in
           let ft2 = Superset.is_fall_through superset dst src in
           if (ft1 || ft2) then (
             (*Superset_risg.G.remove_edge insn_risg src dst;*)
             Map.set csedges ~key:src ~data:dst
           ) else csedges
         else csedges
      ) Addr.Map.empty in
  cross_section_edges


let extract_trim_clamped superset = 
  let to_clamp = find_free_insns superset in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  Set.iter to_clamp ~f:(fun c -> 
      if not Hash_set.(mem visited c) then
        if Superset.Core.mem superset c then (
          Traverse.mark_descendent_bodies_at
            ~visited ~datas superset c
        )
    );
  Superset.Core.clear_each superset visited;
  Set.iter to_clamp ~f:(Superset.Core.clear_bad superset);
  superset

let extract_trim_limited_clamped superset =
  let protection = Addr.Hash_set.create () in
  let callsites = get_callsites ~threshold:0 superset in
  let superset = tag_callsites protection ~callsites superset in
  Superset.Core.clear_all_bad superset;
  let superset = extract_trim_clamped superset in
  Superset.Core.clear_each superset protection; superset

let fixpoint_descendants superset extractf depth = 
  let rec fix_descendants cur_features d =
    if d >= depth then
      cur_features
    else
      let visited = Addr.Hash_set.create () in
      let subset_features = Addr.Hash_set.create () in
      Hash_set.iter cur_features ~f:(fun cur ->
          if not Hash_set.(mem visited cur) then
            Traverse.with_descendents_at superset
              ~pre:(fun v ->
                  if Hash_set.(mem cur_features v)
                  && not Addr.(cur = v) then
                    Hash_set.add subset_features v
                ) ~visited cur
          else Hash_set.add subset_features cur
        );
      fix_descendants subset_features (d+1)
  in
  let cur_features = extractf superset in
  fix_descendants cur_features 0

let fixpoint_map superset feature_pmap = 
  let visited = Addr.Hash_set.create () in
  let entries = Superset.frond_of_isg superset in
  Hash_set.fold ~init:feature_pmap entries ~f:(fun feature_pmap cur -> 
      if not Hash_set.(mem visited cur) then
        let prev = ref [] in
        let feature_pmap = ref feature_pmap in
        Traverse.with_descendents_at ~pre:(fun v ->
            match Map.find !feature_pmap v with
            | None -> ()
            | Some(p) ->
              prev :=  List.append p  !prev;
              feature_pmap := Map.set !feature_pmap ~key:v ~data:!prev;
          ) ~visited superset cur;
        !feature_pmap
      else feature_pmap
    )

let fixpoint_grammar superset depth =
  let extractf superset = 
    Superset.get_branches superset in
  fixpoint_descendants superset extractf depth

(* TODO all features is not all features *)
let allfeatures = 
  "RestrictedClamped"      ::
  "ExtendedClamped"        ::
  "ClassicGrammar"         ::
  "LinearGrammar"          ::
  "UnfilteredGrammar"      ::
  "FalseBranchMap"         ::
  "FilteredFalseBranchMap" ::
  "UnfilteredSCC"          ::
  "FreeVarSSA"             ::
  "FixpointGrammar"        :: 
  "FixpointSSA"            ::
  "FixpointFreevarSSA"     :: 
  "FixpointTails"          :: 
  defaults

let get_branches superset = 
  let branches = Superset.get_branches superset in
  transform branches

let linear_grammar superset =
  let entries = Superset.entries_of_isg superset in
  transform Grammar.(linear_branch_sweep superset entries)

let classic_grammar superset =
  transform Grammar.(identify_branches superset)

let extract_loops_to_set superset =
  let loops = Superset.ISG.raw_loops superset in
  let loops = List.filter loops ~f:(fun l -> List.length l >= 2) in
  Grammar.addrs_of_loops loops

let extract_filter_loops superset =
  Grammar.addrs_of_filtered_loops superset

let extract_loops_with_break superset =
  let loop_addrs = extract_loop_addrs superset in
  Map.fold ~init:Addr.Set.empty loop_addrs ~f:(fun ~key ~data loops -> 
      let loop = List.fold ~init:Addr.Set.empty data ~f:Set.add in
      let has_break = List.exists  data
                        ~f:(fun addr ->
            let targets = Superset.ISG.descendants superset addr in
            List.exists targets
                ~f:(fun x -> not Set.(mem loop x))
            ) in
      if has_break then Set.union loops loop else loops
    )

let extract_exitless superset = 
  let returned = Addr.Hash_set.create () in
  let entries = Superset.entries_of_isg superset in
  Hash_set.iter entries ~f:(fun entry -> 
      Traverse.with_ancestors_at superset
        ?post:None ~pre:(Hash_set.add returned) entry
    );
  Superset.Core.fold superset ~f:(fun ~key ~data exitless ->
      let v = key in
      if not (Hash_set.mem returned v) 
      then Set.add exitless v else exitless
    ) ~init:Addr.Set.empty

let extract_constants_to_set superset = 
  let constants = extract_constants superset in
  Map.fold constants ~init:Addr.Set.empty ~f:(fun ~key ~data consts -> 
      Set.add consts data
    )

let collect_descendants superset ?visited ?datas targets = 
  let visited = Option.value visited ~default:(Addr.Hash_set.create ()) in
  let datas = Option.value datas ~default:(Addr.Hash_set.create ()) in
  Hash_set.iter targets ~f:(fun v -> 
      if not Hash_set.(mem visited v) then
        Traverse.mark_descendent_bodies_at ~visited ~datas superset v      
    )

let extract_img_entry superset =
  let e = Addr.Set.empty in
  match Superset.Inspection.get_main_entry superset with
  | Some mentry -> Set.add e mentry 
  | None -> e

let extract_trim_callsites superset =
  let visited = Addr.Hash_set.create () in
  let callsites = get_callsites ~threshold:2 superset in
  let protection = get_callsites ~threshold:0 superset in
  collect_descendants superset ~visited protection;
  Superset.Core.clear_all_bad superset;
  let superset = tag_callsites visited ~callsites superset in
  Superset.Core.clear_each superset visited;
  superset

let extract_trim_entry superset =
  let imgentry = extract_img_entry superset in
  Set.iter imgentry ~f:Traverse.(mark_descendent_bodies_at superset);
  superset

let extract_trim_noexit superset =
  let exitless = extract_exitless superset in
  Set.iter exitless ~f:Superset.Core.(mark_bad superset);
  superset

type extractor = (Superset.t -> Addr.Set.t)
type ('b) mapextractor = (Superset.t -> 'b Addr.Map.t)
type setfilter = (Superset.t -> Addr.Set.t -> Addr.Set.t)
type ('b) mapfilter = (Superset.t -> 'b Addr.Map.t -> 'b Addr.Map.t)
type setexfilt = extractor * setfilter
type ('a, 'b) mapexfilt = ('b) mapextractor * ('b) mapfilter
let unfiltered _ = Fn.id
  
let _exfiltset = [
  ("FixpointGrammar",
   ((fun x -> transform (fixpoint_grammar x 0)), unfiltered));
  ("Liveness", (Liveness.compute_liveness,unfiltered));
  ("LinearGrammar", (linear_grammar, unfiltered));
  ("UnfilteredGrammar", (get_branches, unfiltered));
  ("ClassicGrammar", (classic_grammar, unfiltered));
  ("Callsites3",
   ((fun x -> transform (get_callsites
                           ~threshold:6 x)), unfiltered));
  ("Clamped",
   ((fun s -> find_free_insns s), unfiltered));
  ("RestrictedClamped", (restricted_clamp, unfiltered));
  ("ExtendedClamped", (extended_clamp, unfiltered));
  ("UnfilteredSCC", (extract_loops_to_set,unfiltered));
  ("LoopsWithBreak", (extract_loops_with_break,unfiltered));
  ("SCC", (extract_filter_loops,unfiltered));
  ("NoExit", (extract_exitless, unfiltered));
  ("Constant", (extract_constants_to_set,unfiltered));
  ("ImgEntry", (extract_img_entry, unfiltered));
]
let exfiltset :(setexfilt) String.Map.t
  = List.fold ~init:String.Map.empty _exfiltset
    ~f:(fun exfiltset (name, f) ->
        String.Map.set exfiltset ~key:name ~data:f
      )

let featureflist =
  [("Callsites3", extract_trim_callsites);
   ("ImgEntry",extract_trim_entry);
   (*("SCC", extract_tag_loops)*)
   ("NoExit", extract_trim_noexit);
   ("TrimLimitedClamped" ,extract_trim_limited_clamped);
  ]
let featuremap = List.fold featureflist ~init:String.Map.empty
    ~f:(fun featuremap (name, f) ->
        Map.set featuremap ~key:name ~data:f
      )

let with_featureset ~f ~init featureset superset =
  let superset = List.fold ~init featureset ~f:(fun accu fname ->
      match Map.(find featuremap fname) with
      | None -> accu
      | Some (feature) ->
        f fname feature accu
    ) in
  superset

let fdists = String.Map.empty
let fdists = String.Map.set fdists ~key:"FixpointGrammar" ~data:1
let fdists = String.Map.set fdists ~key:"Liveness"        ~data:1

let make_featurepmap featureset superset = 
  List.fold ~f:(fun (feature_pmap) feature -> 
      let p = Map.find fdists feature in
      let p = Option.value p ~default:2 in
      match Map.(find exfiltset feature) with
      | None -> feature_pmap
      | Some (extract,filter) -> 
        let fset = extract superset in
        Set.fold fset ~init:feature_pmap 
          ~f:(fun feature_pmap x -> 
              Map.update feature_pmap x ~f:(function 
                  | Some l ->  (p, x, feature) :: l
                  | None -> [(p, x, feature)]
                )
            )
    ) ~init:Addr.Map.empty featureset

let with_featurepmap featureset superset ~f =
  let feature_pmap = make_featurepmap featureset superset in
  let feature_pmap = fixpoint_map superset feature_pmap in
  f feature_pmap featureset superset

