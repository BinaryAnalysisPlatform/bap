open Core_kernel
open Bap.Std
include Self ()

type switch = Enable | Disable [@@deriving sexp]

let parse = function
  | "enable" -> `Ok Enable
  | "disable" -> `Ok Disable
  | s ->
    `Error (sprintf "unknown value %s, possible are: disable | enable" s)

let print fmt s =
  Format.fprintf fmt "%s" @@  Sexp.to_string (sexp_of_switch s)

let switcher = Config.converter parse print Enable

let switch f s = Option.some_if (s = Enable) f
let norm = switch (Stmt.normalize ~normalize_exp:false)
let simpl = switch Bil.fold_consts
let subst = switch Bil.reduce_consts

let add norm simpl reduce =
  let norm = switch (Stmt.normalize ~normalize_exp:false) norm in
  let simpl = switch Bil.fold_consts simpl in
  let reduce = if Option.is_none norm then None
    else switch Bil.reduce_consts reduce in
  let apply bil = function
    | None -> bil
    | Some f -> f bil in
  let (>>=) = apply in
  let f bil =
    Ok (apply bil norm >>= simpl >>= reduce) in
  register_bass "internal" f

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Applies analysises to a instruction bil code" ;
      `Pre "
The following bil analysises are in default pipeline:
Bil Normalization
Constant Folding
Constant Substitution
";
      `S "SEE ALSO";
      `P "$(b,bap.mli)"
    ] in
  let norm =
    let doc = "Produces a normalized BIL program" in
    Config.(param switcher ~default:Enable ~doc "norm") in
  let simpl =
    let doc = "Applies expressions simplification." in
    Config.(param switcher ~default:Enable ~doc "simpl") in
  let subst =
    let doc = "Substitutes constant expressions." in
    Config.(param switcher ~default:Enable ~doc "subst") in
  Config.when_ready (fun {Config.get=(!)} ->
      add !norm !simpl !subst)
