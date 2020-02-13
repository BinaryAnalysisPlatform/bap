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

let start = Tid.create ()
let exit = Tid.create ()

let connect_with_exit n g =
  if Tid.equal n exit then g
  else G.Edge.insert (G.Edge.create n exit exit) g

let connect_with_start sub = match Term.first blk_t sub with
  | None -> G.Node.insert start
  | Some entry -> G.Edge.insert @@
    G.Edge.create start (Term.tid entry) start

(* post: all nodes are reachable from the exit node *)
let graph_of_sub_with_exit sub =
  let g = connect_with_start sub (G.create sub) in
  G.nodes g |> Seq.fold ~init:g ~f:(fun g n ->
      if G.Node.degree ~dir:`Out n g = 0
      then connect_with_exit n g
      else g) |> fun g ->
  Graphlib.depth_first_search (module G) g
    ~rev:true ~init:g ~start:exit
    ~start_tree:connect_with_exit

let liveness sub =
  let g = graph_of_sub_with_exit sub in
  let init = Solution.create Tid.Map.empty Var.Set.empty in
  let tran = block_transitions sub in
  Graphlib.fixpoint (module G) ~init ~start:exit ~rev:true g
    ~merge:Set.union
    ~equal:Var.Set.equal
    ~f:(fun n vars ->
        if Tid.equal n exit || Tid.equal n start  then vars
        else
          let {defs; uses} = Map.find_exn tran n in
          vars -- defs ++ uses)

let free_vars_of_sub sub  =
  if Ssa.is_transformed sub
  then ssa_free_vars sub
  else Solution.get (liveness sub) start
