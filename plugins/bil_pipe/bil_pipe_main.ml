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

let stub  _ _ bil = Ok bil
let simpl _ _ bil = Ok (Bil.fold_consts bil)
let norml _ _ bil = Ok (Stmt.normalize bil)
let propg _ _ bil = Ok (propagate_consts bil)
let renum _ _ bil = Ok (renum bil)
let simpl _ _ bil = Ok (Stmt.simpl ~ignore:[] bil)
let stub_find  _ _ _ = Error (Error.of_string "bil not found")

let run if_norml if_simpl if_propg if_memo =
  let add cat ~default x  = function
    | Enable -> cat,x
    | Disable -> cat,default in
  let add_memo = add memo ~default:stub_find find in
  let add_bass = add bass ~default:stub in
  let pipe = [
    add_memo if_memo;
    add_bass simpl if_simpl;
    add_bass norml if_norml;
    add_bass propg if_propg;
    add_bass renum if_memo ;
    add_bass save  if_memo ;
  ] in
  List.iter pipe ~f:(fun (cat,f) -> register_bass cat f)

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
