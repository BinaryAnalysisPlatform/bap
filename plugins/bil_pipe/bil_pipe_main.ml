open Core_kernel
open Bap.Std
include Self ()

type switch = Enable | Disable
[@@deriving sexp]

let of_string s = switch_of_sexp @@ Sexp.of_string s

let switch f s = Option.some_if (of_string s = Enable) f

let norm_of_str = switch (Stmt.normalize ~normalize_exp:false)
let simpl_of_str = switch Bil.fold_consts
let subst_of_str = switch Bil.reduce_consts

let add norm simpl reduce =
  let reduce = if Option.is_none norm then None
    else reduce in
  List.iter [norm;simpl;reduce] ~f:(function
      | None -> ()
      | Some f -> register_bass (fun b -> Ok (f b)))

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Applies analysises to a instruction bil code" ;
      `Pre "
Bil Normalization
Constant Folding
Constant Substitution
"
    ] in
  let names = ["enable", "enable"; "disable", "disable";] in
  let norm =
    let doc =
      "Produces a normalized BIL program.
        Possible values are enable | disable" in
    Config.(param (enum names) ~default:"enable" "norm" ~doc) in
  let simpl =
    let doc = "Applies expressions simplification.
                Possible values are enable, disable" in
    Config.(param (enum names) ~default:"enable" ~doc "simpl") in
  let subst =
    let doc = "Applies constant expressions substitution.
                Possible values are enable, disable" in
    Config.(param (enum names) ~default:"enable" ~doc "subst") in
  Config.when_ready (fun {Config.get=(!)} ->
      let norm = norm_of_str !norm in
      let simpl = simpl_of_str !simpl in
      let subst = subst_of_str !subst in
      add norm simpl subst)
