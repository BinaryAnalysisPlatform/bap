open Core_kernel.Std
open Bap.Std
open Format

type t = {
  precision : float;
  recall : float;
  fp     : int;
  tp     : int;
  fn     : int;
  f_05     : float;
  tool   : string
}

let compare (tool_fs : addr list) (gt_fs : addr list) (tool : string) : t =
  (* TODO: I don't like this way to initilize because I could forget the meaning
   * for each 0 *)
  let tp, fp = List.fold tool_fs ~init:(0, 0) ~f:(fun (tp, fp) addr ->
    if List.mem gt_fs addr then (tp + 1, fp)
    else (tp, fp + 1)) in
  let fn = List.length gt_fs - tp in
  let tp_float = float tp in
  let fn_float = float fn in
  let fp_float = float fp in
  let precision = tp_float /. (tp_float +. fp_float) in
  let recall = tp_float /. (tp_float +. fn_float) in
  let f_05 = 1.5 *. precision *. recall /. (0.5 *. precision +. recall) in
  {precision;recall;f_05;fp;tp;fn;tool}

let pp (measures : t list) : unit =
  printf "Tool\tPrecision\tRecall\tF_0.5\tTP\tFP\tFN\n";
  List.iter measures ~f:(fun m ->
    printf "%s\t%.2f\t\t%.2f\t%.2f\t%d\t%d\t%d\n" m.tool m.precision m.recall
    m.f_05 m.tp m.fp m.fn);
