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

let mk f = fun _ _ bil -> f bil
let simpl = mk Bil.fold_consts
let norml = mk Stmt.normalize
let propg = mk propagate_consts
let renum = mk renum

let process pipe =
  let pipe = List.filter_map pipe
      ~f:(fun (test, f) -> Option.some_if test f) in
  fun addr code bil ->
    let rec run bil = function
      | [] -> bil
      | f :: fs ->
        run (f addr code bil) fs in
    Ok (run bil pipe)

let run if_norml if_simpl if_propg if_memo =
  if if_memo then register_memo find;
  register_bass "internal" @@
  process [
    if_simpl,simpl;
    if_norml,norml;
    if_propg,propg;
    if_memo, renum;
    if_memo, save;
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
  let memo = switcher "memo" ~doc:"Bil memoization" in
  Config.when_ready (fun {Config.get=(!)} -> run !norm !simpl !prop !memo)
