open Core_kernel
open Bap_types.Std
open Graphlib.Std
open Bap_ir

module Ssa = Bap_sema_ssa
module G = Bap_tid_graph

let (++) = Set.union and (--) = Set.diff
let blk = G.Node.label

let ssa_free_vars sub =
  let is_undefined v = Var.index v = 0 in
  Term.enum blk_t sub |> Seq.fold ~init:Var.Set.empty ~f:(fun vars blk ->
      vars ++ Set.filter (Ir_blk.free_vars blk) ~f:is_undefined)

let defined_by_blk b =
  Ir_blk.elts b |> Seq.fold ~init:Var.Set.empty ~f:(fun kill -> function
      | `Phi phi -> Set.add kill @@ Ir_phi.lhs phi
      | `Def def -> Set.add kill @@ Ir_def.lhs def
      | `Jmp _ -> kill)

type blk_transfer = {
  defs : Var.Set.t;
  uses : Var.Set.t;
}

let blk_defs blk =
  Term.enum def_t blk |>
  Seq.fold ~init:Var.Set.empty  ~f:(fun defs def ->
      Set.add defs (Ir_def.lhs def))

let block_transitions sub =
  Term.enum blk_t sub |>
  Seq.fold ~init:Tid.Map.empty ~f:(fun fs blk ->
      Map.add_exn fs (Term.tid blk) {
        defs = blk_defs blk;
        uses = Ir_blk.free_vars blk;
      })

let compute_liveness sub =
  let g = G.create sub in
  let init = Solution.create Tid.Map.empty Var.Set.empty in
  let tran = block_transitions sub in
  Graphlib.fixpoint (module G) ~init ~start:G.exit ~rev:true g
    ~merge:Set.union
    ~equal:Var.Set.equal
    ~f:(fun n vars ->
        if Tid.equal n G.exit || Tid.equal n G.start  then vars
        else
          let {defs; uses} = Map.find_exn tran n in
          vars -- defs ++ uses)

let free_vars_of_sub sub  =
  if Ssa.is_transformed sub
  then ssa_free_vars sub
  else Solution.get (compute_liveness sub) G.start
