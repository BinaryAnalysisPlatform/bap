
open Core_kernel.Std
open Bap.Std
include Self ()

type norm  = BNF1 | BNF2  [@@deriving sexp]
type simpl = L1 | L2 | L3 [@@deriving sexp]

let of_str f = function
  | "disable" -> None
  | s ->
    printf "s is %s\n" s;
    Some (f (Sexp.of_string s))

let norm_of_str = of_str norm_of_sexp
let simpl_of_str = of_str simpl_of_sexp
let add f = register_bass (fun bil -> Ok (f bil))

let add_norm = function
  | None -> ()
  | Some BNF1 -> add Stmt.normalize
  | Some BNF2 -> add (Stmt.normalize ~normalize_exp:true )

let add_simpl norm simpl =
  let simpl = match norm, simpl with
    | None, Some L3 -> Some L2
    | _, s -> s in
  List.iter ~f:add @@
  match simpl with
  | None -> []
  | Some L1 -> [Bil.fold_consts]
  | Some L2 -> [Bil.fold_consts; Bil.group_like]
  | Some L3 -> [Bil.fold_consts; Bil.group_like; Bil.reduce_consts]

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Applies analysises to a instruction bil code" ;
      `Pre "
Bil Normalization
Produces a normalized BIL program
with the same semantics but in the BIL
normalized form (BNF).
There are two normalized forms, both described
below. The first form (BNF1) is more readable, the second form
(BNF2) is more strict, but sometimes yields a code, that is hard
for a human to comprehend.
BNF1(default):
- No if-then-else expressions.
- Memory load expressions can be only applied to a memory
- No load or store expressions in the following positions:
  1. the right-hand side of the let expression;
  2. address or value subexpressions of the store expression;
  3. storage or address subexpressions of the load expression;
BNF2:
- No let expressions - new variables can be created only with
the Move instruction.
- All memory operations have sizes equal to one byte. Thus the
size and endiannes can be ignored in analysis.";
        `Pre "
Bil simplification
There are three levels of simplifications, each of the next is
applied to the previous one:
constant folding, groupping of like expressions and constant reduction.

On the first level the following code simplifications are applied:
- constant folding, e.g. [1 + 2 -> 3]
- neutral element elimination, e.g.  [x * 1 -> 1]
- zero element propagation,    e.g.  [x * 0 -> 0]
- symbolic equality reduction, e.g.  [x = x -> true]
- double one-/two- complement reduction, e.g. [--1 -> 1]
- binary to unary reduction, e.g. [0 - x -> -x]
- exclusive disjunction reduction, e.g. [42 ^ 42 -> 0].
- DeMorgan's Laws simplifications, e.g. [not (x | 0xFD)  = (not x) & 2 ]

On the second level like expressions are groupped together, e.g.
[x + x + x + y + z + y + z] will be simplified to [3x + 2(y + z)]

Finally, on the third level, all computable variables conservatively
spread through the code.

"
    ] in
  let norm =
    let doc =
       "Produces a normalized BIL program.
        Possible values are bnf1, bnf2, disable" in
    let names = ["bnf1", "BNF1"; "bnf2", "BNF2"; "disable", "disable"; ] in
    Config.(param (enum names) ~default:"BNF1" "norm" ~doc) in
  let simpl =
    let doc = "Applies expressions simplification." in
    let names = ["L1", "L1"; "L2", "L2"; "L3","L3"; "disable", "disable"; ] in
    Config.(param (enum names) ~default:"L3" ~doc "simpl") in
  Config.when_ready (fun {Config.get=(!)} ->
      let norm = norm_of_str !norm in
      add_norm norm;
      add_simpl norm (simpl_of_str !simpl))
