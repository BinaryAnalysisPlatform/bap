open Core_kernel
open Bap.Std

include Self ()

let register_norm name desc normalize_exp : Bil.pass =
  Bil.register_pass name ~desc (Stmt.normalize ~normalize_exp)

let bnf1 =
  register_norm "bnf1" "Transforms BIL to the first normalized form" false
let bnf2 =
  register_norm "bnf2" "Transforms BIL to the second normalized form" true

let normalizations =
  [ "0", [];
    "1", [bnf1];
    "2", [bnf2]; ]

let fold_consts =
  Bil.register_pass "constant-folding" ~desc:"constant-folding" Bil.fold_consts

let consts_propagation =
  Bil.register_pass "constant-propagation" ~desc:"constant-propagation" Bil.propagate_consts

let prune_dead =
  Bil.register_pass "prune-dead-virtuals"
    ~desc:"dead code elimination for virtual variables" Bil.prune_dead_virtuals

let o1 = [fold_consts; consts_propagation; prune_dead]

let optimizations =
  [ "0", [];
    "1", o1; ]

let print_passes () =
  Bil.passes () |> List.map ~f:Bil.Pass.to_string |> List.sort ~compare:String.compare |>
  List.iter ~f:(printf "%s\n");
  exit 0

let pass = Config.enum
    (List.map (Bil.passes ()) ~f:(fun p -> Bil.Pass.to_string p, p))

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Controls the BIL transformation pipeline.";

      `P "The BIL transformation pipeline is used to transform the BIL
        code of each machine instruction and is applied to the output
        of a lifter. Each piece of transformation is called $(i,a pass).
        This plugin enables full control over the pass selection as
        well as provides some predefined passes. See the
        $(b,--bil-list-passes) for a comprehensive list of available
        transformations. The pipeline is formed as a concatenation of
        all normalization passes that correspond to the selected level
        of normalization (setting it to zero will disable normalizations),
        all optimization passes applicable to the selected level of
        optimization (again the zero level corresponds to an empty
        list of optimizations), and all passes selected via the
        $(b,--bil-passes) command line argument. All passes are
        applied until a fixed point is reached (i.e., until the
        outcome of the pipeline is equal to the income) or until a
        loop is detected. Thus to prevent infinite looping each
        registered pass should be a pure function of its inputs (same
        input shall result in the same output).";

      `P "A new pass could be registered via the $(b,Bil.register_pass)
        function. It won't be automatically selected, so it should be
        specified explicitly via the $(b,--bil-passes) command line
        argument.";

      `S "SEE ALSO";
      `P "$(b,bap)(3)";
    ] in
  let norml =
    let doc = "Specifies the BIL normalization level.
      The normalization process doesn't change the semantics of
      a BIL program, but applies some transformations to simplify it.
      There are two BIL normal forms (bnf): bnf1 and bnf2, both
      of which are described in details in $(b,bap)(3).
      Briefly, $(b,bnf1) produce the BIL code  with load expressions
      that applied to a memory only. And $(b,bnf2) also adds some
      more restrictions like absence of let-expressions and makes
      load/store operations sizes equal to one byte.
      So, there are next possible options for normalization level:
      $(b,0) - disables normalization; $(b,1) - produce BIL in bnf1;
      $(b,2) - produce BIL in bnf2" in
    Config.(param (enum normalizations) ~default:[bnf1] ~doc "normalization") in
  let optim =
    let doc = "Specifies the optimization level.\n
      Level $(b,0) disables optimization,  and level $(b,1) performs
      regular program simplifications, i.e., applies constant folding,
      propagation, and elimination of dead temporary (aka virtual) variables." in
    Config.(param (enum optimizations) ~default:o1 ~doc "optimization") in
  let list_passes =
    let doc = "List all available passes and exit" in
    Config.flag ~doc "list-passes" in
  let passes =
    let doc =
      "Selects the list and the order of analyses to be applied during
       the lifing to BIL code." in
    Config.(param (list pass) ~default:[] ~doc "passes") in
  Config.when_ready (fun {Config.get=(!)} ->
      if !list_passes then print_passes ()
      else
        Bil.select_passes (!norml @ !optim @ !passes))
