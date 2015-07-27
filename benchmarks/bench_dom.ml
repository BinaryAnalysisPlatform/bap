open Core.Std
open Core_bench.Std
open Bap.Std
open Format

let filename = "arm-binaries/coreutils/coreutils_O1_ls"
let sizes_to_test = 10

module Cfg = Graphlib.Ir
module CFG = Graphlib.To_ocamlgraph(Cfg)
module DOM = Graph.Dominator.Make(CFG)
module SCC = Graph.Components.Make(CFG)

let proj = Project.from_file filename |> ok_exn

let syms = Project.symbols proj

(** [scale_linear ~input:(x1,x2) ~output:(y1,y2)]
    will return a linear function $f(x) = ax + b$ such that:

    {[
      f x1 = y1;
      f x2 = y2;
    ]} *)
let scale_linear ~input:(x1,x2) ~output:(y1,y2) =
  let open Float in
  let x1,x2,y1,y2 = float x1, float x2, float y1, float y2 in
  let a = (y1 - y2) / (x1 - x2) in
  let b = y1 - x1 * (y1 - y2) / (x1-x2) in
  fun x -> to_int (round (a * (float x) + b))


let functions,sizes,index =
  let functions = Term.enum sub_t (Project.program proj) |>
                  Seq.map ~f:Cfg.of_sub |> Seq.to_array in
  let size g = Cfg.number_of_nodes g + Cfg.number_of_edges g in
  Array.sort functions ~cmp:(fun x y -> Int.compare (size x) (size y));
  let max_idx = Array.length functions - 1 in
  let min_size = size functions.(0) in
  let max_size = min 1000 @@ size functions.(max_idx) in
  let size =
    scale_linear ~input:(0,sizes_to_test-1) ~output:(min_size,max_size) in
  let index =
    scale_linear ~input:(min_size,max_size) ~output:(0,max_idx) in
  let sizes = List.init sizes_to_test ~f:size in
  functions,List.rev sizes,index

let ocamlgraph cfg entry =
  DOM.compute_idom cfg entry

let graphlib cfg entry =
  Tree.parent (Graphlib.dominators (module Cfg) cfg entry)

let dom algo cfg =
  algo cfg (Seq.hd_exn @@ Cfg.nodes cfg)

let run f size =
  stage (fun () -> f functions.(index size))


let graphlib_scc cfg =
  Graphlib.strong_components (module Cfg) cfg |>
  Partition.equiv

let ocamlgraph_scc cfg =
  let _,get = SCC.scc cfg in
  fun x y -> get x = get y

let indexed = Bench.Test.create_indexed ~args:sizes


let dom_test =
  Bench.Test.create_group ~name:"dom" [
    indexed ~name:"ocamlgraph" (run (dom ocamlgraph));
    indexed ~name:"graphlib" (run (dom graphlib));
  ]



let scc_test =
  Bench.Test.create_group ~name:"scc" [
    indexed ~name:"graphlib" (run graphlib_scc);
    indexed ~name:"ocamlgraph" (run ocamlgraph_scc);
  ]


let tests = [
  dom_test;
  scc_test;
]
