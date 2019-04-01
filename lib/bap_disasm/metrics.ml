open Bap_types.Std
open Bap_image_std
open Core_kernel.Std


type format_as   = | Latex
                   | Standard
[@@deriving sexp]

type metrics = {
  name                : string;
  detected_insn_count : int;
  false_negatives     : int;
  false_positives     : int;
  detected_entries    : int;
  actual_entries      : int;
  trimmed             : int list;
}

module InvariantTrackingApplicator = struct
end

module type MetricsGathererInstance = sig
  type acc = (Addr.Hash_set.t * Superset_risg.t )
  val accu : acc
end

module MetricsGatheringReducer(M : MetricsGathererInstance) : Trim.Reducer = struct
  type acc = M.acc
  let accu = M.accu
  let check_pre _ accu _ = accu
  let check_post _ accu _ = accu
  let check_elim _ _ _ = true
  let mark superset (s,g) addr =
    Hash_set.add s addr;
    let src_graph = Superset.get_graph superset in
    List.iter Superset_risg.G.(succ src_graph addr) ~f:(fun succ -> 
        Superset_risg.G.add_edge g addr succ
      );
    List.iter Superset_risg.G.(pred src_graph addr) ~f:(fun pred -> 
        Superset_risg.G.add_edge g pred addr
      )
end

module type PerMarkTracker = sig
  type acc = (Addr.Set.t Addr.Map.t) ref
  val accu : acc
end
                                                                           
module PerMarkTrackingReducer(M : PerMarkTracker) : Trim.Reducer =
  struct
    type acc = M.acc
    let accu = M.accu
    let cur_root = ref None
    let check_pre superset accu addr =
      (match !cur_root with
      | Some x ->
         accu :=
           Map.update !accu x ~f:(fun s ->
               match s with
               | None -> Addr.Set.empty
               | Some s -> Set.add s addr
             );
      | None -> cur_root:=Some(addr));
      accu
    let check_post superset (accu : acc) addr =
      (match !cur_root with
      | None -> ()
      | Some x -> if x = addr then cur_root := None);
      accu
    let check_elim _ _ _ = true
    let mark superset accu addr =
      let cur_root = Option.value_exn !cur_root in
      accu :=
        Map.update !accu cur_root ~f:(fun s ->
            match s with
            | None -> Addr.Set.empty
            | Some s -> Set.add s addr
          );
  end

let print_dot superset colorings =
  (*if not (colorings = String.Map.empty) then*)
  let img = Superset.get_img superset in
  let fout = Out_channel.create @@
    Option.value_exn Image.(filename img) ^ ".dot" in
  let superset_risg = Superset.get_graph superset in
  let superset_isg = Superset_risg.Oper.mirror superset_risg in
  let insn_map = Superset.get_map superset in
  let module Layout =
    Cfg_dot_layout.Make(struct
      let instance = (superset_isg, colorings, insn_map)
    end) in
  Layout.Dot.output_graph fout (superset_isg, colorings, insn_map)


let format_standard metrics =
  match metrics with 
  | Some metrics -> 
    sprintf "%s%d\n%s%d\n%s%d\n%s%d\n%s%d" 
      "Total instructions recovered: " metrics.detected_insn_count
      "False negatives: " metrics.false_negatives
      "False positives: " metrics.false_positives
      "Detected function entrances: " metrics.detected_entries
      "Actual function entrances: " metrics.actual_entries
  | None -> "No metrics gathered!"

let format_latex metrics = 
  match metrics with
  | Some metrics ->
    (match metrics.trimmed with
     | (phase1 :: phase2 :: _) ->
       sprintf "%s & %d & %d & %d & %d \\\\\n"
         metrics.name
         metrics.false_negatives
         phase1
         phase2
         metrics.detected_insn_count;
     | _ -> "Missing trim phases")
  | None -> "No metrics gathered!"

let true_positives_of_ground_truth ?insn_isg superset ground_truth = 
  let insn_isg = 
    match insn_isg with 
    | Some insn_isg -> insn_isg 
    | None ->
      let insn_risg = Superset.get_graph superset in
      Superset_risg.Oper.mirror insn_risg in
  let true_positives = Addr.Hash_set.create () in
  Set.iter ground_truth ~f:(fun addr -> 
      if Superset_risg.G.mem_vertex insn_isg addr then
        Superset_risg.Dfs.prefix_component 
          (Hash_set.add true_positives) insn_isg addr;
    );
  true_positives

(* implement jmp_of_fp as a map from target to source in *)
(* True positive set is going to come up short because if it isn't in *)
(* the isg, it isn't going to be explored *)
let true_positives ?insn_isg superset f = 
  let function_starts =
    Insn_disasm_benchmark.ground_truth_of_unstripped_bin f |> ok_exn
  in
  let ground_truth =
    Addr.Set.of_list @@ Seq.to_list function_starts in
  true_positives_of_ground_truth ?insn_isg superset ground_truth

let reduced_occlusion superset tp =
  let fps = Addr.Hash_set.create () in
  Hash_set.iter tp ~f:(fun addr ->
      let len = Superset.len_at superset addr in
      Seq.iter (Superset_risg.seq_of_addr_range addr len) 
        ~f:(fun x -> Hash_set.add fps x);
      Hash_set.remove fps addr;
    );
  fps

let false_positives superset ro = 
  let insn_risg = Superset.get_graph superset in
  let fps = Addr.Hash_set.create () in
  Hash_set.iter ro ~f:(fun v ->
      if Superset_risg.G.mem_vertex insn_risg v then
        Hash_set.add fps v
    );
  fps

let fn_insn_cnt superset tps =
  let insn_risg = Superset.get_graph superset in
  Hash_set.fold ~init:0 tps ~f:(fun count v -> 
      if Superset_risg.G.mem_vertex insn_risg v then count 
      else count+1)

let check_tp_set true_positives s =
  let n = Hash_set.length s in
  let tp_of_s = 
    Hash_set.fold ~init:0 true_positives
      ~f:(fun tp_of_s x -> 
          if Hash_set.mem s x
          then tp_of_s + 1 else tp_of_s) in
  let fp_of_s = n - tp_of_s in
  fp_of_s, tp_of_s

let check_fn_entries superset ground_truth =
  let insn_risg = Superset.get_graph superset in
  let detected_insns = 
    Superset_risg.G.fold_vertex 
      (fun vert detected_insns -> Set.add detected_insns vert) 
      insn_risg Addr.Set.empty in
  Set.diff ground_truth detected_insns

(* adjust this to collect metrics into the metrics field, and then *)
(* split the printing out into a separate function *)
let gather_metrics ~bin superset =
  let insn_map = Superset.get_map superset in
  let insn_risg = Superset.get_graph superset in
  let function_starts =
    Insn_disasm_benchmark.ground_truth_of_unstripped_bin bin |> ok_exn in
  let ground_truth =
    Addr.Set.of_list @@ Seq.to_list function_starts in
  let insn_isg = Superset_risg.Oper.mirror insn_risg in
  let ground_truth = 
    Set.(filter ground_truth ~f:(fun e ->
        let img = Superset.get_img superset in
        Superset.with_img ~accu:false img 
          ~f:(fun ~accu mem ->
              accu || Memory.(contains mem e))
      )) in
  let true_positives = true_positives_of_ground_truth ~insn_isg superset ground_truth in
  let datas = Addr.Hash_set.create () in
  let detected_insns = Addr.Hash_set.create () in
  let dfs_find_conflicts addr =
    Superset.with_descendents_at
      ~insn_isg ~visited:detected_insns superset addr
      ~f:(fun v -> Superset.with_data_of_insn superset v
             ~f:(fun x -> Hash_set.add datas x)) in
  let reduced_occlusion () = Hash_set.fold ~init:0 datas
      ~f:(fun ro d ->
          if Superset_risg.G.(mem_vertex insn_risg d)
          then ro+1 else ro) in
  let num_bytes =
    Superset.with_img ~accu:0 Superset.(get_img superset)
      ~f:(fun ~accu mem  -> accu + Memory.(length mem)) in
  let entries = Superset_risg.entries_of_isg insn_risg in
  let branches = Grammar.linear_branch_sweep superset entries in
  let fp_branches, tp_branches = check_tp_set true_positives branches in
  printf "Num f.p. branches: %d, num tp branches: %d\n" fp_branches tp_branches;
  printf "superset_isg_of_mem length %d\n" num_bytes;
  let total_clean,_ =
    Set.fold ground_truth ~init:(0,0) ~f:(fun (n,prev) x ->
        dfs_find_conflicts x;
        if Hash_set.length datas > prev then
          ((n),Hash_set.(length datas))
        else ((n+1),prev)
      ) in
  printf "Number of functions precisely trimmed: %d of %d\n"
    total_clean Set.(length ground_truth);
  printf "Number of possible reduced false positives: %d\n" 
    Hash_set.(length datas);
  printf "Reduced occlusion: %d\n" (reduced_occlusion ());
  printf "True positives: %d\n" Hash_set.(length true_positives);
  let fn_entries = check_fn_entries superset ground_truth in
  if not (Set.length fn_entries = 0) then
    printf "Missed function entrances %s\n" 
      (List.to_string ~f:Addr.to_string @@ Set.to_list fn_entries);
  printf "Occlusion: %d\n" 
    (Set.length @@ Superset_risg.find_all_conflicts insn_map);
  printf "Instruction fns: %d\n" (fn_insn_cnt superset true_positives);
  printf "superset_map length %d graph size: %d num edges %d\n" 
    Map.(length insn_map) 
    (Superset_risg.G.nb_vertex insn_risg)
    (Superset_risg.G.nb_edges insn_risg);
  let false_negatives = Set.(length fn_entries) in
  let detected_entries = Set.(length ground_truth) - false_negatives in
  let false_positives = Hash_set.fold detected_insns ~init:0
      ~f:(fun c v -> if not Set.(mem ground_truth v) then c+1 else c) in
  let detected_insn_count = Superset_risg.G.nb_vertex insn_risg in
  Some ({
      name                = bin;
      detected_insn_count = detected_insn_count;
      false_positives     = false_positives;
      false_negatives     = false_negatives;
      detected_entries    = detected_entries;
      actual_entries      = (Set.length ground_truth);
      trimmed             = [];
    })

(*module Opts = struct 
  open Cmdliner

  let list_content_doc = sprintf
      "Metrics may be collected against a symbol file"
  let content_type = 
    Arg.(value &
         opt (some string) (None)
         & info ["metrics_data"] ~doc:list_content_doc)

  let list_formats_types = [
    "standard", Standard;
    "latex", Latex;
  ]
  let list_formats_doc = sprintf
      "Available output metrics formats: %s" @@ 
    Arg.doc_alts_enum list_formats_types
  let metrics_format = 
    Arg.(value & opt (enum list_formats_types) Standard
         & info ["metrics_format"] ~doc:list_formats_doc)

end*)
