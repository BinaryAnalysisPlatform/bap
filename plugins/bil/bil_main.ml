open Core_kernel
open Bap.Std

include Self ()

let register' name desc pass =
  Bil.register_pass name ~desc pass;
  List.find (Bil.passes ())
    ~f:(fun p -> String.equal (Bil.pass_name p) name)

let register_norm name desc keep_ites normalize_exp =
  register' name desc (Stmt.normalize ~keep_ites ~normalize_exp)

let bnf1 =
  register_norm "bnf1" "Transforms BIL to the first normalized form"
    false false
let bnf2 =
  register_norm "bnf2" "Transforms BIL to the second normalized form"
    false true
let bnf1' =
  register_norm "bnf1'"
    "Transforms BIL to the first normalized form, but preserves if-then-else expressions" true false
let bnf2' = register_norm "bnf2'"
    "Transforms BIL to the second normalized form, but preserves if-then-else expressions" true true

let normalizations = [ None; bnf1; bnf2; bnf1'; bnf2' ]

let o1 = register' "internal-optimization"
    "Internal implementation of constant folding, constant propagation and elimination of dead virtual variables"
    (fun bil -> Bil.fold_consts bil |> Bil_const_propagation.run |> Bil_live.run)

let optimizations = [ None; o1; ]

let print_passes () =
  Bil.passes () |> List.map ~f:Bil.Pass.to_string |> List.sort ~cmp:String.compare |>
  List.iter ~f:(printf "%s\n");
  exit 0

let get name choices level =
  try
    let level = int_of_string level in
    `Ok (List.nth_exn choices level)
  with _ ->
    `Error
      (sprintf "unknown %s level %s, must be [0 - %d]"
         name level (List.length choices - 1))

let converter name choices =
  let parser = get name choices in
  let printer fmt x =
    Option.iter x ~f:(fun p -> Format.fprintf fmt "%s" (Bil.pass_name p)) in
  Config.converter parser printer None

let norml = converter "normalization" normalizations
let optim = converter "optimization" optimizations

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

      `P "A new pass could registered via the $(b,Bil.register_pass)
        function. It won't be automatically selected, so it should be
        specified explicitly via the $(b,--bil-passes) command line
        argument."
    ] in
  let norml =
    let doc = "Specifies Bil normalization level.
      Normalization process doesn't change a semantics of Bil program,
      but applyies some transformation to make it more strict.
      There are two major Bil normal forms (bnf): bnf1 and bnf2, both
      of which are described in details in $(b,bap.mli), and two minor
      forms: $(b,bnf1') and $(b,bnf2'). Briefly, $(b,bnf1)
      produce a Bil code without if-then-else expressions,
      with load expressions that applied to a memory only. And
      $(b,bnf2) also adds some more restrictions like absence of
      let-expressions and makes load/store operations sizes equal to
      one byte. Minor forms differ from major ones only by
      preserving if-then-else expressions.
      So, there are next possible complementations for
      normalization level:
      $(b,0) - disables normalization; $(b,1) - produce Bil in bnf1;
      $(b,2) - produce Bil in bnf2; $(b,3) - produce Bil in bnf1';
      $(b,4) - produce Bil in bnf2'" in
    Config.(param norml ~default:bnf1 ~doc "normalization") in
  let optim =
    let doc = "Specifies a level of optimization.\n
      Level $(b,0) disables optimization, and level $(b,1) enables
      a pass that gathers under one roof next optimizations: constant folding,
      constant propagation and elimination of virtual dead variables." in
    Config.(param optim ~default:o1 ~doc "optimization") in
  let list_passes =
    let doc = "List all available passes and exits" in
    Config.flag ~doc "list-passes" in
  let passes =
    let doc =
      "Selects the list and the order of analyses to be applied during
       the lifing to BIL code." in
    Config.(param (list pass) ~default:[] ~doc "passes") in
  Config.when_ready (fun {Config.get=(!)} ->
      if !list_passes then
        print_passes ()
      else
        let passes' = List.filter_map ~f:ident [!norml; !optim] in
        let passes = passes' @ !passes in
        Bil.select_passes passes)
