open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Bap_future.Std
open Format
open Cmdliner

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

let bin : string Term.t =
  let doc = "The testing stripped binary." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"binary" ~doc)

let tool () : string Term.t =
  match Rooter.Factory.list () |> List.map ~f:(fun x -> x,x) with
  | [x,_] -> Term.const x
  | [] -> assert false
  | names ->
    let doc = sprintf "The tool of the function start result \
                       that we are going to evaluate. Possible \
                       values: %s." @@ Arg.doc_alts_enum names in
    Arg.(required & pos 1 (some (enum names)) None & info [] ~docv:"tool" ~doc)

let truth : string Term.t =
  let doc =
    "The ground truth. The ground truth can be an unstripped
  binary, or a .scm file with symbol information. In .scm file, each symbol should
  be in format of:
  (<symbol name> <symbol start address> <symbol end address>), e.g.
  (malloc@@GLIBC_2.4 0x11034 0x11038)" in
  Arg.(required & pos 2 (some non_dir_file) None
       & info [] ~docv:"ground-truth" ~doc)

let print_metrics : metrics list Term.t =
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
  Arg.(value & opt_all (enum opts) (List.map ~f:snd opts) &
       info ["with-metrics"; "c"] ~doc)

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


let compare_against_t () = Term.(pure compare_against $bin $tool () $truth $print_metrics)

let info =
  let doc = "function start identification benchmark game" in
  let man = [
    `S "DESCRIPTION";
    `P "Compares function start identification algorithms to
        the ground truth. The latter should be provided by a user.";
  ] @ Bap_cmdline_terms.common_loader_options in
  Term.info "bap-fsi-benchmark" ~doc ~man

let () =
  let argv = Bap_plugin_loader.run Sys.argv in
  match Term.eval ~argv (compare_against_t (), info) with
  | `Error _ -> exit 1
  | _ -> exit 0
