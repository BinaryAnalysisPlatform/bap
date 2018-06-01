open Core_kernel
open Bap.Std
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
  Config.(param (converter parse print Enable) ~default)

let switch f s = Option.some_if (s = Enable) f

let add norml simpl prop =
  let norml = switch (Stmt.normalize ~normalize_exp:false) norml in
  let simpl = switch Bil.fold_consts simpl in
  let prop = switch Bil.propagate_consts prop in
  let apply bil = function
    | None -> bil
    | Some f -> f bil in
  let (>>=) = apply in
  register_bass "internal" @@
  fun bil -> Ok (apply bil norml >>= simpl >>= prop)

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
  Config.when_ready (fun {Config.get=(!)} -> add !norm !simpl !prop)
