open Core_kernel
open Bap.Std
open Bil_const_propagation
include Self ()


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
";
      `S "SEE ALSO";
      `P "$(b,bap.mli)"
    ] in
  let simpl =
    let doc = "Applies expressions simplification." in
    Config.(param int ~default:1 "simplification" ~doc) in
  let norm =
    let doc = "Produces a normalized BIL program" in
    Config.(param int ~default:1 "normalization" ~doc) in
  Config.when_ready (fun {Config.get=(!)} -> run !norm !simpl)
