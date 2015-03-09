open Core_kernel.Std
open Cmdliner
open Bap.Std
open Bap_plugins.Std

exception No_unstripped_file

let print_source = function
  | `bw -> print_endline "bw"
  | _ -> print_endline "others"

let source = [
  "BW", `bw;
  "SymTbl", `symtbl;
  "User", `user;
  "Ida", `ida;
]

let source_gt = [
  "User", `user;
  "SymTbl", `symtbl
]

let tool : _ Term.t =
  let doc = "The source of the function start result that we are going to
  evaluate. " ^ (Arg.doc_alts_enum source) in
  Arg.(value & pos 1 (enum source) `bw & info [] ~docv:"tool" ~doc)

let gt : _ Term.t =
  let doc = "The ground truth source. One can direct to a user file or use the symbol
  table from unstripped binary (-u required). " ^ (Arg.doc_alts_enum source_gt) in
  Arg.(value & pos 2 (enum source_gt) `symtbl & info [] ~docv:"gt" ~doc)

let symsfile : string option Term.t =
  let doc = "The symbol table of binaries. This requires the binaries to use this file as symbols source." in
  Arg.(value & opt (some non_dir_file) None & info ["syms"; "s"]
         ~docv:"syms" ~doc)

let unstrip_bin : string option Term.t =
  let doc = "The unstripped binary." in
  Arg.(value & opt (some non_dir_file) None & info ["unstrip"; "u"]
         ~docv:"unstripped_bin" ~doc)

let use_ida : string option Term.t =
  let doc = "Use Ida to extract symbols from file. \
             You can optionally provide path to IDA executable, \
             or executable name." in
  Arg.(value & opt (some string) None & info
         ["use-ida"] ~doc)

let print_metrics : _ list Term.t =
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

let bin : string Term.t =
  let doc = "The testing stripped binary." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"binary" ~doc)

let func_start bin symsfile unstrip_bin use_ida : _ -> Addr.Set.t * string =
  function
  | `bw -> Func_start.byteweight bin, "BW"
  | `user -> (match symsfile with
      | None -> raise Not_found
      | Some f -> Func_start.user f, "User")
  | `symtbl -> (match unstrip_bin with
      | None -> raise No_unstripped_file
      | Some b -> Func_start.symbols b, "Symbol")
  | `ida -> Func_start.ida ?use_ida bin, "IDA"

let compare bin print_metrics tool gt symsfile unstrip_bin use_ida : unit = try
    let fs_tool, tool_name = func_start bin symsfile unstrip_bin use_ida tool in
    let fs_gt, _ = func_start bin symsfile unstrip_bin use_ida gt in
    let fp =
      let set = Set.diff fs_tool fs_gt in
      Set.length set in
    let fn =
      let set = Set.diff fs_gt fs_tool in
      Set.length set in
    let tp = Set.length fs_gt - fn in
    let prec, recl =
      let f_fp = float fp in
      let f_fn = float fn in
      let f_tp = float tp in
      f_tp /. (f_tp +. f_fp), f_tp /. (f_tp +. f_fn) in
    let f_05 = 1.5 *. prec *. recl /. (0.5 *. prec +. recl) in

    (* print out the metrics *)
    let headers, items =
      let rev_hd, rev_it = List.fold print_metrics ~init:(["Tool"], [tool_name])
          ~f:(fun (headers, items) -> function
              | `with_prec -> "Prcs"::headers, (Printf.sprintf "%.2g" prec)::items
              | `with_recl -> "Rcll"::headers, (Printf.sprintf "%.2g" recl)::items
              | `with_F -> "F_05"::headers, (Printf.sprintf "%.2g" f_05)::items
              | `with_TP -> "TP"::headers, (Printf.sprintf "%d" tp)::items
              | `with_FN -> "FN"::headers, (Printf.sprintf "%d" fn)::items
              | `with_FP -> "FP"::headers, (Printf.sprintf "%d" fp)::items ) in
      List.rev rev_hd, List.rev rev_it in
    Printf.printf "%s\n%s\n" (String.concat ~sep:"\t" headers)
      (String.concat ~sep:"\t" items)
  with
  | Func_start.Bad_user_input tool ->
    Printf.printf "bap-byteweight dump %s command does not \
                   work properly.\n" tool
  | Not_found -> Printf.printf "No Symbol File found.\n"
  | No_unstripped_file ->
    Printf.printf "Cannot get symbole table: No unstripped file found. Did you
    provide unstripped binary by -u?\n"

let compare_t = Term.(pure compare $bin $print_metrics $tool $gt $symsfile
                      $unstrip_bin $use_ida)

let info =
  let doc = "to compare the functions start identification result to the ground
  truth" in
  let man = [] in
  Term.info "bap-measure-byteweight" ~doc ~man

let () =
  Printexc.record_backtrace true;
  Plugins.load ();
  match Term.eval ~catch:false (compare_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
