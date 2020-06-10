open Core_kernel
open Regular.Std
open Graphlib.Std
open Bap_ir


module G = Graphlib.Make(Tid)(Tid)

let of_sub sub =
  Term.enum blk_t sub |> Seq.fold ~init:G.empty ~f:(fun g src ->
      let sid = Term.tid src in
      let g = G.Node.insert sid g in
      Term.enum jmp_t src |> Seq.fold ~init:g ~f:(fun g jmp ->
          match Bap_ir_graph.succ_tid_of_jmp jmp with
          | None -> g
          | Some did ->
            let jid = Term.tid jmp in
            let edge = G.Edge.create sid did jid in
            G.Edge.insert edge g))

let start = Tid.for_name ~package:"bap" "start-pseudo-node"
let exit = Tid.for_name ~package:"bap" "exit-pseudo-node"

let connect_with_exit n =
  if Tid.equal n exit then ident
  else G.Edge.insert (G.Edge.create n exit exit)

let connect_with_start n =
  if Tid.equal n start then ident
  else
    G.Edge.insert @@
    G.Edge.create start n start

let if_unreachable ~from connect g n =
  if G.Node.degree ~dir:from n g = 0
  then connect n
  else ident

let create sub =
  let g = of_sub sub in
  G.nodes g |> Seq.fold ~init:g ~f:(fun g n ->
      g |>
      if_unreachable ~from:`In connect_with_start g n |>
      if_unreachable ~from:`Out connect_with_exit g n) |> fun g ->
  Graphlib.depth_first_search (module G) g
    ~init:g ~start
    ~start_tree:connect_with_start
  |> fun g ->
  Graphlib.depth_first_search (module G) g
    ~rev:true ~init:g ~start:exit
    ~start_tree:connect_with_exit

include G
