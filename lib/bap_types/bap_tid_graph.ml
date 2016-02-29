open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap_ir
open Bap_ir_graph

module G = Graphlib.Make(Tid)(Tid)

let create sub =
  Term.enum blk_t sub |> Seq.fold ~init:G.empty ~f:(fun g src ->
      let sid = Term.tid src in
      let g = G.Node.insert sid g in
      Term.enum jmp_t src |> Seq.fold ~init:g ~f:(fun g jmp ->
          match succ_tid_of_jmp jmp with
          | None -> g
          | Some did ->
            let jid = Term.tid jmp in
            let edge = G.Edge.create sid did jid in
            G.Edge.insert edge g))

include G
