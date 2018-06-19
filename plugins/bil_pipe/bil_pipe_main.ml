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

let simpl _ _ bil = Bil.fold_consts bil
let norml _ _ bil = Stmt.normalize bil
let propg _ _ bil = propagate_consts bil
let renum _ _ bil = renum bil

let add pipe (test, f) =
  if test then f :: pipe
  else pipe

let seal pipe =
  fun addr code bil ->
  let rec run bil = function
    | [] -> bil
    | f :: fs -> run (f addr code bil) fs in
  let bil = run bil (List.rev pipe) in
  Ok bil

let run if_norml if_simpl if_propg if_memo =
  let fs = [
    if_simpl,simpl;
    if_norml,norml;
    if_propg,propg;
    if_memo, renum;
    if_memo, save;
  ] in
  let pipe = List.fold fs ~init:[] ~f:add |> seal in
  if if_memo then register_memo find;
  register_bass "internal" pipe


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
