open Core
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std

let transform_summaries summaries = 
  let open Metrics in
  let summaries =
    List.filter_map summaries ~f:(fun s ->
        match s.size, s.occ, s.occ_space, s.fe, s.clean, s.fns, s.fps,
              s.tps, s.time with
        | None, _, _, _, _, _, _, _, _ -> None
        | _, None, _, _, _, _, _, _, _ -> None
        | _, _, None, _, _, _, _, _, _ -> None
        | _, _, _, None, _, _, _, _, _ -> None
        | _, _, _, _, None, _, _, _, _ -> None
        | _, _, _, _, _, None, _, _, _ -> None
        | _, _, _, _, _, _, None, _, _ -> None
        | _, _, _, _, _, _, _, None, _ -> None
        | _, _, _, _, _, _, _, _, None -> None
        | Some size, Some occ, Some occ_space, Some fe, Some clean,
          Some fns, Some fps, Some tps, Some time ->
           Some (size, occ, occ_space, fe, clean, fns, fps, tps, time)
      ) in
    List.fold summaries ~init:([],[],[],[],[],[],[],[],[])
      ~f:(fun (sizes,occ,occ_space,fe,clean,fns,fps,tps,time) s ->
        let _size,_occ,_occ_space,_fe,_clean,_fns,_fps,_tps,_time = s in
        _size :: sizes, _occ :: occ, _occ_space :: occ_space,
        _fe :: fe,_clean :: clean,_fns :: fns,_fps :: fps,_tps :: tps,
        _time :: time
      )

  (* Plots:
   binary size to occlusive rate (occlusion by occ space)
   occlusive space to occlusion
   scatter plot occlusive count and number of occ functions
   size and processing time
   least value required for safe convergence
   number of binaries and occ rate
 *)
let make_plots summaries =
  let open Metrics in
  let sizes,occ,occ_space,fe,clean,fns,fps,tps,time =
    transform_summaries summaries in
  let make_plot xlabel ylabel fname x y = 
    let x  = List.map x ~f:float_of_int in
    let y = List.map y ~f:float_of_int in
    match List.zip x y with
    | Ok data -> 
       let title = (xlabel ^ " and " ^ ylabel) in
       let labels = Gnuplot.Labels.create ~x:xlabel ~y:ylabel () in
       let color = Gnuplot.Color.(`Blue) in
       let plot = Gnuplot.Series.points_xy ~title ~color data in
       let gp = Gnuplot.create () in
       let output = (Gnuplot.Output.create (`Png (title ^ ".png"))) in
       Gnuplot.set ~use_grid:true gp;
       Gnuplot.plot ~output ~labels gp plot;
       Gnuplot.close gp;
       ()
    | _ -> () in
  make_plot "Size" "Occlusion" "size_and_occlusion.png" sizes occ;
  make_plot "Actual Occlusion" "Possible Occlusion"
    "occlusion_and_occspace.png" occ occ_space;
  let () =
    match List.map2 fe clean ~f:(fun x y -> x - y) with
    | List.Or_unequal_lengths.Ok occfuncs ->
       make_plot "Total Occlusion" "# Unclean functions"
         "occcnt_occfuncs.png" occ occfuncs
    | _ -> () in
  make_plot "Size" "Time" "size_time.png" sizes time;
  ()

let knowledge_reader = Data.Read.create
    ~of_bigstring:Knowledge.of_bigstring ()

let knowledge_writer = Data.Write.create
    ~to_bigstring:Knowledge.to_bigstring ()

let knowledge_cache () =
  Data.Cache.Service.request
    knowledge_reader
    knowledge_writer

let load_cache_with_digest cache digest =
  match Data.Cache.load cache digest with
  | None -> false
  | Some state ->
     Toplevel.set state;
     true

let import_knowledge_from_cache digest =
  let cache = knowledge_cache () in
  load_cache_with_digest cache digest

let make_digest inputs =
  let inputs = String.concat inputs in
  fun ~namespace ->
    let d = Data.Cache.Digest.create ~namespace in
    Data.Cache.Digest.add d "%s" inputs

let compute_digest target disasm =
  make_digest [
      Caml.Digest.file target;
      disasm;
    ] ~namespace:"knowledge"

let summaries_of_files tgt fs =
  List.fold fs ~init:[] ~f:(fun ls lf ->
      let digest = compute_digest lf tgt  in
      if import_knowledge_from_cache digest then
        Metrics.get_summary () :: ls
      else (
        print_endline @@ sprintf "%s not present in cache" lf;
        ls
      )
    )

let plot_summaries summaries =
  make_plots summaries;
  let sizes,occ,occ_space,fe,clean,fns,fps,tps,time =
    transform_summaries summaries in
  let tot_fns = List.fold fns ~init:0 ~f:(+) in
  let tot_occ = List.fold occ ~init:0 ~f:(+) in
  let tot_occ_space = List.fold occ_space ~init:0 ~f:(+) in
  let avg_occ =
    (float_of_int tot_occ) /. (float_of_int tot_occ_space) in
  printf "fns: %d, total occ space %d, avg occ: %f\n"
    tot_fns tot_occ_space avg_occ

