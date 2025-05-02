open Bap.Std
open Core
open Or_error

module Linear = Disasm_expert.Linear

let read arch ic : (string * addr * addr) list =
  let sym_of_sexp x = [%of_sexp:string * int64 * int64] x in
  let addr_of_int64 x =
    let width = Arch.addr_size arch |> Size.in_bits in
    Addr.of_int64 ~width x in
  List.(Sexp.input_sexps ic >>| sym_of_sexp >>| (fun (s, es, ef) ->
      s, addr_of_int64 es, addr_of_int64 ef))

let read_addrs ic : addr list =
  List.t_of_sexp Addr.t_of_sexp @@ Sexp.input_sexp ic

let ground_truth_of_unstripped_bin bin : addr seq Or_error.t =
  let name = Filename.basename bin in
  let tmp = Filename.temp_dir_name ^ "/bw_" ^ name ^ ".symbol" in
  let cmd = sprintf "bap-byteweight dump -i symbols %S > %S" 
      bin tmp in
  if Stdlib.Sys.command cmd = 0
  then return (Seq.of_list @@ In_channel.with_file tmp ~f:read_addrs)
  else errorf
      "failed to fetch symbols from unstripped binary, command `%s'
  failed" cmd

let true_positives_of_ground_truth superset ground_truth = 
  let true_positives = Addr.Hash_set.create () in
  Set.iter ground_truth ~f:(fun addr -> 
      if Superset.ISG.mem_vertex superset addr then
        Traverse.with_descendents_at
          ~visited:true_positives
          superset addr;
    );
  true_positives
  
let true_positives superset f = 
  let function_starts = ground_truth_of_unstripped_bin f |> ok_exn
  in
  let ground_truth =
    Addr.Set.of_list @@ Seq.to_list function_starts in
  true_positives_of_ground_truth superset ground_truth  

module Cache = struct
  open Bap_knowledge
  open Bap_core_theory

  let package = "superset-disasm-metrics"
  let sym_label =
    KB.Symbol.intern "superset_analysis" Theory.Program.cls

  let bool_t = Knowledge.Domain.optional
                 ~inspect:sexp_of_bool ~equal:Bool.equal "bool"
  let bool_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = bool option [@@deriving bin_io] end)

  let int_t = Knowledge.Domain.optional
                ~inspect:sexp_of_int ~equal:Int.equal "int"

  let int_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = int option [@@deriving bin_io] end)

  let addrs_t =
    Knowledge.Domain.optional
      ~inspect:Addr.Set.sexp_of_t ~equal:Addr.Set.equal "addr.set"

  let addrs_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = Addr.Set.t option [@@deriving bin_io] end)
    
  let attr ty persistent name desc =
    let open Theory.Program in
    Knowledge.Class.property ~package cls name ty
      ~persistent ~public:true ~desc
    
  let string_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = string [@@deriving bin_io] end)

  let ground_truth_source =
    let open Knowledge.Domain in
    attr string string_persistent "ground_truth_source"
      "Binary containing debug information"

  let function_entrances =
    attr addrs_t addrs_persistent "function_entrances"
      "List of compiler intended function entrances"

  let ground_truth =
    attr addrs_t addrs_persistent "ground_truth"
      "Statically reachable descendents of function entrances"

  let size =
    attr int_t int_persistent "code_size"
      "Total number of byte that the code memory segments occupy"

  let time =
    attr int_t int_persistent "processing_time"
      "Time required to process this binary"
    
  let occlusive_space =
    attr int_t int_persistent "occlusive_space"
      "Number of addresses are in the bodies (addrs) of ground truth"

  let reduced_occlusion =
    attr int_t int_persistent "reduced_occlusion"
      "Of the occlusive space, how many are occupied"

  let false_negatives =
    attr int_t int_persistent "false_negatives"
      "Number of compiler intended instructions missing"

  let false_positives =
    attr int_t int_persistent "false_positives"
      "Number of compiler intended instructions missing"

  let true_positives =
    attr int_t int_persistent "true_positives"
      "Number of retained compiler intended instructions"
    
  let clean_functions =
    attr addrs_t addrs_persistent "clean_functions"
      "Functions with completely empty occlusive space"

  let occluded_entrances =
    attr addrs_t addrs_persistent "occluded_entrances"
      "Functions with at least one address that is obstructed"
end

let set_ground_truth superset =
  let open Bap_knowledge in
  let open Bap_core_theory in
  let open KB.Syntax in
  KB.promise Cache.function_entrances (fun label ->
      KB.collect Cache.ground_truth_source label >>= fun bin ->
      (* list of compiler intended entrances *)
      let function_addrs = ground_truth_of_unstripped_bin bin
                           |> ok_exn in
      let function_addrs =
        Addr.Set.of_list @@ Seq.to_list function_addrs in
      KB.return (Some function_addrs)
    );
  
  KB.promise Cache.ground_truth (fun label ->
      (* List of compiler intended addresses *)
      KB.collect Cache.function_entrances label >>=
        fun function_addrs ->
        match function_addrs with
        | None -> KB.return None
        | Some function_addrs ->
           let visited = Addr.Hash_set.create () in
           Set.iter function_addrs ~f:(fun x ->
               Traverse.with_descendents_at ~visited superset x
             );
           let ground_truth = Hash_set.fold visited ~init:
                                Addr.Set.empty ~f:Set.add in
           KB.return (Some ground_truth)
    )

let compute_metrics superset =
  let open Bap_knowledge in
  let open Bap_core_theory in
  let open KB.Syntax in
  KB.promise Cache.size (fun label ->
      KB.return (Some (Superset.Inspection.total_bytes superset))
    );

  KB.promise Cache.occlusive_space (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | None -> KB.return None
      | Some ground_truth ->
         KB.return @@
           Some (Set.fold ground_truth ~init:0
                   ~f:(fun occ addr ->
                     occ + (Superset.Inspection.len_at superset addr)
             ))
    );

  (* per feature metrics *)
  KB.promise Cache.reduced_occlusion (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | None -> KB.return None
      | Some ground_truth ->
         KB.return @@
           Some (Set.fold ground_truth ~init:0
                   ~f:(fun ro addr ->
                     let conflicts = 
                       Superset.Occlusion.conflict_seq_at
                         superset addr in
                     ro + (Seq.length conflicts)
             ))

    );

  KB.promise Cache.clean_functions (fun label ->
      KB.collect Cache.function_entrances label >>=
        fun function_entrances ->
        match function_entrances with
        | None -> KB.return None
        | Some (function_entrances) ->
           let ro_at x =
             let ro = ref false in
             let pre x =
               let c = Superset.Occlusion.conflict_seq_at superset x in
               ro := Seq.exists c ~f:(fun _ -> true); in
             Traverse.with_descendents_at superset ~pre x; !ro in
           let init = Addr.Set.empty in
           KB.return @@
             (Some
                (Set.fold function_entrances ~init ~f:(fun clean x ->
                  if ro_at x then clean else Set.add clean x
                ))
             )
    );

  KB.promise Cache.occluded_entrances (fun label ->
      KB.collect Cache.function_entrances label >>= function
      | None -> KB.return None
      | Some funcs -> 
         let occluded_starts =
           Set.fold funcs ~init:Addr.Set.empty ~f:(fun occluded start ->
               let behind = Addr.(start -- 20) in
               let range = Superset.Core.seq_of_addr_range behind 21 in
               let range = Seq.filter range ~f:(fun v -> not Addr.(v = start)) in
               let addr =
                 Seq.find range ~f:(fun current ->
                     Seq.exists
                       (Superset.Occlusion.conflict_seq_at superset current)
                       ~f:(fun conflict -> Addr.((not (conflict = current))
                                                 && conflict = start)
                   )) in
               Option.value_map addr ~default:occluded ~f:Set.(add occluded)
             ) in
         KB.return (Some occluded_starts)
    );

  KB.promise Cache.true_positives (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | None -> KB.return None
      | Some ground_truth ->
         KB.return
           (Some (Set.fold ground_truth ~init:0 ~f:(fun tp_cnt x ->
                      if Superset.Core.mem superset x then
                        tp_cnt + 1
                      else tp_cnt
              ))
           )
    );
  
  KB.promise Cache.false_negatives (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | Some ground_truth ->
         let fn_cnt =
           Set.fold ground_truth ~init:0 ~f:(fun cnt x ->
               if not (Superset.Core.mem superset x) then
                 cnt + 1
               else cnt
             ) in
         KB.return @@ Some fn_cnt
      | None -> KB.return None          
    );
  
  KB.promise Cache.false_positives (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | Some ground_truth ->
         let false_positives =
           Superset.Core.fold superset ~init:0
             ~f:(fun ~key ~data c ->
               if not Set.(mem ground_truth key) then c+1
               else c) in
         KB.return (Some false_positives)
      | None -> KB.return None
    )

type t = {
    size      : int option;
    time      : int option;
    occ       : int option;
    occ_space : int option;
    fe        : int option;
    clean     : int option;
    fns       : int option;
    fps       : int option;
    tps       : int option;
  } [@@deriving sexp]

let get_summary () = {
    size = Toplevel.eval Cache.size Cache.sym_label;
    time = Toplevel.eval Cache.time Cache.sym_label;
    fe = Option.map ~f:Set.length @@
           Toplevel.eval Cache.function_entrances Cache.sym_label;
    occ_space = Toplevel.eval Cache.occlusive_space Cache.sym_label;
    occ = Toplevel.eval Cache.reduced_occlusion Cache.sym_label;
    fns = Toplevel.eval Cache.false_negatives Cache.sym_label;
    fps = Toplevel.eval Cache.false_positives Cache.sym_label;
    tps = Toplevel.eval Cache.true_positives Cache.sym_label;
    clean = Option.map ~f:Set.length @@
      Toplevel.eval Cache.clean_functions Cache.sym_label;
  }
