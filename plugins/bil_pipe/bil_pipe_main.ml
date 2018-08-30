open Core_kernel
open Bap.Std
open Bil_const_propagation
open Bil_renum_virtual
open Bil_memo
include Self ()

let parse = function
  | "enable" -> `Ok true
  | "disable" -> `Ok false
  | s ->
    `Error (sprintf "unknown value %s, possible are: disable | enable" s)

let print fmt s =
  Format.fprintf fmt "%s" @@
  match s with
  | true  -> "enable"
  | false -> "disable"

let switcher ?(default=true) =
  Config.(param (converter parse print default) ~default)

let simpl = Bil.fold_consts
let norml = Stmt.normalize ~keep_ites:false ~normalize_exp:false
let propg = propagate_consts

let process pipe =
  let pipe = List.filter_map pipe
      ~f:(fun (test, f) -> Option.some_if test f) in
  fun bil ->
    let rec run bil = function
      | [] -> bil
      | f :: fs ->
        run (f bil) fs in
    run bil pipe

let run if_norml if_simpl if_propg =
  register_bass "internal" @@
  process [
    if_simpl,simpl;
    if_norml,norml;
    if_propg,propg;
  ]

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Applies analysises to a instruction bil code" ;
      `Pre "
The following bil analysises are in default pipeline:
Constant Folding and Simplification
Bil Normalization
Constant Propagation
Memoization
";
      `S "SEE ALSO";
      `P "$(b,bap.mli)"
    ] in
  let simpl = switcher "simpl" ~doc:"Applies expressions simplification." in
  let norm = switcher "norm" ~doc:"Produces a normalized BIL program" in
  let prop = switcher "prop" ~doc:"Const propagation" in
  Config.when_ready (fun {Config.get=(!)} -> run !norm !simpl !prop)
