let doc = {|
# DESCRIPTION

Defines the semantics of BAP Instruction Language (BIL) and makes old
BIL lifters available in the system. This plugin denotes the semantics
of the Core Theory in terms of BIL statements and expressions as well
as defines an interpretation of BIL programs in terms of the Core
Theory. The latter enables the old lifter to be seamlessly integrated
with the new knowledge framework.

The BIL code, obtained from the lifters is processed by the BIL
transformation pipeline to enable optimization and/or normalization of
BIL program.s

# THE BIL PIPELINE

The BIL transformation pipeline is used to transform the BIL code of
each machine instruction and is applied to the output of a BIL
lifter. Each piece of transformation is called $(i,a pass).  This
plugin enables full control over the pass selection as well as
provides some predefined passes. See the $(b,--bil-list-passes) for a
comprehensive list of available transformations. The pipeline is
formed as a concatenation of all normalization passes that correspond
to the selected level of normalization (setting it to zero will
disable normalizations), all optimization passes applicable to the
selected level of optimization (again the zero level corresponds to an
empty list of optimizations), and all passes selected via the
$(b,--bil-passes) command line argument. All passes are applied until
a fixed point is reached (i.e., until the outcome of the pipeline is
equal to the income) or until a loop is detected. Thus to prevent
infinite looping each registered pass should be a pure function of its
inputs (same input shall result in the same output).

A new pass could be registered via the $(b,Bil.register_pass)
function. It won't be automatically selected, so it should be
specified explicitly via the $(b,--bil-passes) command line
argument.
|}

open Bap_core_theory
open Core_kernel
open Bap.Std
open Bap_main.Extension

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

let pass = Type.enum
    (List.map (Bil.passes ()) ~f:(fun p -> Bil.Pass.to_string p, p))

let () =
  let norml =
    let doc = "Selects a BIL normalization level.
      The normalization process doesn't change the semantics of
      a BIL program, but applies some transformations to simplify it.
      Consult BAP Annotated Reference (BAR) for the detailed
      description of the BIL normalized forms." in
    Configuration.(parameter Type.(enum normalizations)
                     ~doc "normalization") in
  let optim =
    let doc = "Specifies an optimization level.\n
      Level $(b,0) disables all optimizations,  and level $(b,1) performs
      regular program simplifications, e.g., applies constant folding,
      propagation, and elimination of dead temporary (aka virtual) variables." in
    Configuration.(parameter Type.(enum optimizations =? o1) ~doc "optimization") in
  let list_passes =
    let doc = "List all available passes and exit" in
    Configuration.flag ~doc "list-passes" in
  let passes =
    let doc =
      "Selects the list and the order of analyses to be applied during
       the lifing to BIL code." in
    Configuration.(parameter Type.(list pass) ~doc "passes") in
  let enable_fp_emu = Configuration.flag "enable-fp-emulation"
      ~doc:"Enable the floating point emulation mode.
      When specified, enables reification of the floating point
      operations into Bil expressions that denote those operations
      in terms of bitvector arithmetic. This may lead to very large
      denotations." in

  declare ~provides:["bil"; "core-theory"; "lifter"] @@ fun ctxt ->
  let open Syntax in
  if ctxt-->list_passes then print_passes ()
  else begin
    Bil.select_passes (ctxt-->norml @ ctxt-->optim @ ctxt-->passes);
    Bil_lifter.init ~with_fp:(ctxt-->enable_fp_emu) ();
    Bil_ir.init();
    Theory.declare (module Bil_semantics.Core)
      ~package:"bap.std" ~name:"bil"
      ~desc:"semantics in BIL"
      ~provides:["bil"; "lifter"];
    Theory.declare (module Bil_semantics.Core_with_fp_emulation)
      ~package:"bap.std" ~name:"bil-fp-emu"
      ~extends:["bap.std:bil"]
      ~desc: "semantics in BIL, including FP emulation"
      ~context:["floating-point"]
      ~provides:[
        "bil";
        "floating-point";
        "lifter";
      ];
    Ok ()
  end
