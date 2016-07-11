open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Bap_future.Std
open Format
open Frontend

type metrics = [
  | `with_F
  | `with_FN
  | `with_FP
  | `with_TP
  | `with_prec
  | `with_recl
]

type evaluation = {
  true_positive: int;
  false_negative: int;
  false_positive: int;
  prec: float;
  recl: float;
  f_05: float}

let bin : string Config.param =
  let doc = "The testing stripped binary." in
  Config.(pos non_dir_file ~docv:"binary" ~doc 0)

let tool : string Config.param =
  let doc = sprintf "The tool of the function start result \
                     that we are going to evaluate." in
  Config.(pos string ~docv:"tool" ~doc 1)

let truth : string Config.param =
  let doc =
    "The ground truth. The ground truth can be an unstripped
  binary, or a .scm file with symbol information. In .scm file, each symbol should
  be in format of:
  (<symbol name> <symbol start address> <symbol end address>), e.g.
  (malloc@@GLIBC_2.4 0x11034 0x11038)" in
  Config.(pos non_dir_file ~docv:"ground-truth" ~doc 2)

let print_metrics : metrics list Config.param =
  let opts = [
    "precision", `with_prec;
    "recall", `with_recl;
    "F_measure", `with_F;
    "TP", `with_TP;
    "FP", `with_FP;
    "FN", `with_FN;
  ] in
  let doc = "Print metrics. User can choose to print -cprecision, -crecall,
  -cF_measure, -cTP, -cFP and -cFN." in
  Config.(param_all (enum opts) ~default:(List.map ~f:snd opts)
            "with-metrics" ~synonyms:["c"] ~doc)

let output_metric_value formatter r metrics : unit =
  let output_ratio = fprintf formatter "\t%.2g" in
  let output_int = fprintf formatter "\t%d" in
  List.iter metrics ~f:(function
      | `with_prec -> output_ratio r.prec
      | `with_recl -> output_ratio r.recl
      | `with_F -> output_ratio r.f_05
      | `with_TP -> output_int r.true_positive
      | `with_FN -> output_int r.false_negative
      | `with_FP -> output_int r.false_positive);
  fprintf formatter "\n"

let string_of_metric = function
  | `with_prec -> "Prcs"
  | `with_recl -> "Rcll"
  | `with_F -> "F_05"
  | `with_TP -> "TP"
  | `with_FN -> "FN"
  | `with_FP -> "FP"

let print formatter tool result print_metrics : unit =
  List.iter print_metrics ~f:(fun m -> fprintf formatter "\t%s"
                               @@ string_of_metric m);
  fprintf formatter "\n";
  output_metric_value formatter result print_metrics

let compare_against bin tool_name truth_name print_metrics : unit =
  let module EF = Monad.T.Or_error.Make(Future) in
  let open EF in
  (Func_start.of_tool tool_name ~testbin:bin >>| fun tool ->
   Func_start.of_truth truth_name ~testbin:bin >>| fun truth ->
   let result =
     let to_set seq = Seq.fold seq ~init:Addr.Set.empty
         ~f:Addr.Set.add in
     let tool = to_set tool in
     let truth = to_set truth in
     let false_positive = Set.(length (diff tool truth)) in
     let false_negative = Set.(length (diff truth tool)) in
     let true_positive = Set.length truth - false_negative in
     let ratio x = Float.(of_int true_positive / (of_int true_positive + of_int x)) in
     let prec = ratio false_positive in
     let recl = ratio false_negative in
     let f_05 = 1.5 *. prec *. recl /. (0.5 *. prec +. recl) in
     {false_positive;false_negative;true_positive;prec;recl;f_05} in
   print std_formatter tool_name result print_metrics)
  |> (fun x -> Future.upon x (function
      | Ok _ -> ()
      | Error err ->
        printf "Function start is not recognized properly due to \
                the following error:\n %a" Error.pp err))

let man = [
  `S "DESCRIPTION";
  `P "Compares function start identification algorithms to
        the ground truth. The latter should be provided by a user.";
] @ Bap_cmdline_terms.common_loader_options

let () =
  Config.(descr "function start identification benchmark game");
  Config.(manpage default_command man);
  Config.(when_ready default_command (fun {Config.get=(!)} ->
      compare_against !bin !tool !truth !print_metrics));
  Frontend.start ();
