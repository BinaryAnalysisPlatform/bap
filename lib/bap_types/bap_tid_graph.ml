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

let start = Tid.for_name "%start-pseudo-node%"
let exit = Tid.for_name "%exit-pseudo-node%"

let connect_with_exit n =
  if Tid.equal n exit then ident
  else G.Edge.insert (G.Edge.create n exit exit)

let connect_with_start n =
  if Tid.equal n start then ident
  else
    G.Edge.insert @@
    G.Edge.create start n start

let create sub =
  let g = of_sub sub in
  G.nodes g |> Seq.fold ~init:g ~f:(fun g n ->
      if G.Node.degree ~dir:`Out n g = 0
      then connect_with_exit n g else
      if G.Node.degree ~dir:`In n g = 0
      then connect_with_start n g
      else g) |> fun g ->
  Graphlib.depth_first_search (module G) g
    ~init:g ~start
    ~start_tree:connect_with_start
  |> fun g ->
  Graphlib.depth_first_search (module G) g
    ~rev:true ~init:g ~start:exit
    ~start_tree:connect_with_exit

include G
