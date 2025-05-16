open Core
open Bap.Std

let defaults = [
  "ImgEntry";
  "InterpretationDepthOne";
  "Callsites3";
  "FixpointGrammar";
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
        ?post:None ?pre:None superset callsite;
    );
  superset

let find_free_insns superset = 
  let mem = Superset.Core.mem superset in
  let conflict_free_addrs =
    Superset.Core.fold superset ~init:([])
      ~f:(fun ~key ~data free_insns ->
          let (addr,(memory,_)) = key, data in
          let len = Memory.length memory in
          let conflicts = Superset.Occlusion.range_seq_of_conflicts
              ~mem addr len in
          let no_conflicts = Seq.is_empty conflicts in
          if no_conflicts then
            addr :: free_insns
          else (
            free_insns
          )
        ) in
  let conflict_free_addrs = Addr.Set.of_list conflict_free_addrs in
  Set.diff conflict_free_addrs @@
    Superset.Occlusion.find_all_conflicts superset
  
let extract_trim_interpretation_depth superset = 
  let free_insns = find_free_insns superset in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  Set.iter free_insns ~f:(fun c -> 
      if not Hash_set.(mem visited c) then
        if Superset.Core.mem superset c then (
          Traverse.mark_descendent_bodies_at
            ~visited ~datas superset c
        )
    );
  Superset.Core.clear_each superset visited;
  Set.iter free_insns ~f:(Superset.Core.clear_bad superset);
  superset

(** Interpretation depth is a heuristics that recognizes that true *)
(** positive compiler intended instructions are the only kind of *)
(** instruction that can occur where the body of the instruction is *)
(** completely free of occlusion, aside from random false positives *)
(** that happen to be free. These false positives should not affect *)
(** the convergence on the true positive set. It is called *)
(** interpretation depth one because, without noise in the body, *)
(** there is a single instruction that can be interpreted. *)
let extract_trim_protected_interpretation_depth superset =
  let protection = Addr.Hash_set.create () in
  let callsites = get_callsites ~threshold:0 superset in
  let superset = tag_callsites protection ~callsites superset in
  Superset.Core.clear_all_bad superset;
  let superset = extract_trim_interpretation_depth superset in
  Superset.Core.clear_each superset protection;
  superset

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
  ("Callsites3",
   ((fun x -> transform (get_callsites
                           ~threshold:6 x)), unfiltered));
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
   ("InterpretationDepthOne" ,
    extract_trim_protected_interpretation_depth);
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

