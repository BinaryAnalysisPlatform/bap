open Core
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std
   
let () = match Bap_main.init () with
  | Ok () -> ()
  | Error err -> 
     let open Bap_main in
     Bap_main.Extension.Error.pp Format.std_formatter err;
     exit 1

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

let () =
  let summaries =
    Metadata.with_digests Metadata.cache_corpus_metrics in
  make_plots summaries;
  let sizes,occ,occ_space,fe,clean,fns,fps,tps,time =
    transform_summaries summaries in
  let tot_fns = List.fold fns ~init:0 ~f:(+) in
  let tot_occ = List.fold occ ~init:0 ~f:(+) in
  let tot_occ_space = List.fold fns ~init:0 ~f:(+) in
  let avg_occ =
    (float_of_int tot_occ) /. (float_of_int tot_occ_space) in
  printf "fns: %d, avg occ: %f" tot_fns avg_occ

