open Core_kernel
open Bap.Std
open Bil_const_propagation
open Bil_renum_virtual
open Bil_memo
include Self ()

type switch = Enable | Disable

let parse = function
  | "enable" -> `Ok Enable
  | "disable" -> `Ok Disable
  | s ->
    `Error (sprintf "unknown value %s, possible are: disable | enable" s)

let print fmt s =
  Format.fprintf fmt "%s" @@
  match s with
  | Enable  -> "enable"
  | Disable -> "disable"

let switcher ?(default=Enable) =
  Config.(param (converter parse print default) ~default)

let norml _ _ bil = Ok (Stmt.normalize bil)
let simpl _ _ bil = Ok (Bil.fold_consts bil)
let propg _ _ bil = Ok (propagate_copy bil)
let renum _ _ bil = Ok (renum bil)

let run if_norml if_simpl if_propg =
  let pipe = [
     Enable,   find,  memo;
     if_norml, norml, bass;
     if_simpl, simpl, bass;
     if_propg, propg, bass;
     Enable,   renum, bass;
     Enable,   save,  bass;
  ] in
  List.iter pipe ~f:(fun (e,f,cat) -> match e with
      | Disable -> ()
      | Enable -> register_bass cat f)

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Applies analysises to a instruction bil code" ;
      `Pre "
The following bil analysises are in default pipeline:
Bil Normalization
Constant Folding
Constant Propagation
";
      `S "SEE ALSO";
      `P "$(b,bap.mli)"
    ] in
  let norm =
    switcher "norm" ~doc:"Produces a normalized BIL program" in
  let simpl =
    switcher "simpl" ~doc:"Applies expressions simplification." in
  let prop =
    switcher "prop" ~doc:"Const propagation" in
  Config.when_ready (fun {Config.get=(!)} -> run !norm !simpl !prop)
