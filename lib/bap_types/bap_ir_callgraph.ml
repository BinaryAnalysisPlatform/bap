open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap_ir
open Bap_common

module G = Graphlib.Make(Tid)(struct type t = jmp term list end)

let create program =
  Term.enum sub_t program |> Seq.fold ~init:G.empty ~f:(fun g sub ->
      let src = Term.tid sub in
      let init = G.Node.insert src g in
      Term.enum blk_t sub |> Seq.fold ~init ~f:(fun g blk ->
          Term.enum jmp_t blk |>
          Seq.fold ~init:Tid.Map.empty ~f:(fun dsts jmp ->
              match Ir_jmp.kind jmp with
              | Goto _ | Ret _ | Int _ -> dsts
              | Call call -> match Call.target call with
                | Indirect _ -> dsts
                | Direct dst ->
                  Map.add_multi dsts ~key:dst ~data:jmp) |>
          Map.fold ~init:g ~f:(fun ~key:dst ~data:jmp g ->
              let edge = G.Edge.create src dst jmp in
              G.Edge.insert edge g)))



include G

include Printable.Make(struct
    type t = G.t
    let module_name = None
    let version = "1.0.0"

    let pp ppf g =
      Graphlib.Dot.pp_graph
        ~string_of_node:Tid.name
        ~nodes_of_edge:(fun e -> G.Edge.(src e, dst e))
        ~nodes:(G.nodes g)
        ~edges:(G.edges g) ppf

  end)
