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

let update blk defs uses trans = Map.update trans blk ~f:(function
    | None -> {defs; uses}
    | Some had -> {
        defs = Set.union had.defs defs;
        uses = Set.union had.uses uses
      })

let block_transitions sub =
  Term.enum blk_t sub |>
  Seq.fold ~init:Tid.Map.empty ~f:(fun fs blk ->
      let init = update
          (Term.tid blk)
          (blk_defs blk)
          (Ir_blk.free_vars blk) fs in
      Term.enum phi_t blk |>
      Seq.fold ~init ~f:(fun init phi ->
          let defs = Var.Set.singleton @@ Ir_phi.lhs phi in
          Ir_phi.values phi |>
          Seq.fold ~init ~f:(fun fs (src,exp) ->
              update src defs (Exp.free_vars exp) fs)))

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
