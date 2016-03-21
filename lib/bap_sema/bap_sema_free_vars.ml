open Core_kernel.Std
open Bap_types.Std
open Graphlib.Std
open Bap_ir

module Ssa = Bap_sema_ssa
module G = Bap_ir_graph

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

let free_vars_of_dom_tree dom root =
  let rec bfs (vars,kill) root =
    let cs = Tree.children dom root in
    let kill = kill ++ defined_by_blk (blk root) in
    let vars = vars ++ Seq.fold cs ~init:Var.Set.empty ~f:(fun vars c ->
        Ir_blk.free_vars (blk c) -- kill ++ vars) in
    Seq.fold cs ~init:(vars,kill) ~f:bfs in
  fst @@ bfs (Var.Set.empty,Var.Set.empty) root

let dom_free_vars sub =
  match Term.first blk_t sub with
  | None -> Var.Set.empty
  | Some entry ->
    let entry = G.Node.create entry in
    let cfg = G.of_sub sub in
    let dom = Graphlib.dominators (module G) cfg entry in
    free_vars_of_dom_tree dom entry

let free_vars_of_sub sub  =
  if Ssa.is_transformed sub
  then ssa_free_vars sub
  else dom_free_vars sub

let has_sub_exp x = Exp.exists (object
    inherit [unit] Exp.finder
    method! enter_exp exp search =
      if Exp.equal exp x then search.return (Some ())
      else search
  end)

let substitute_exp x y = Exp.map (object
    inherit Exp.mapper
    method! map_exp exp =
      if Exp.equal exp x then y else x
  end)

let substitute blk x y =
  Ir_blk.elts blk |> Seq.fold ~init:(false,blk)
    ~f:(fun (finished,blk) elt ->
        if finished then finished,blk else match elt with
          | `Phi phi ->
            let exps = Ir_phi.values phi |> Seq.map ~f:snd in
            if Seq.exists exps ~f:(has_sub_exp x) then
              let phi = Ir_phi.map_exp phi ~f:(substitute_exp x y) in
              true,Term.update phi_t blk phi
            else false,blk
          | `Def def ->
            if has_sub_exp x (Ir_def.rhs def) then
              let def = Ir_def.map_exp def ~f:(substitute_exp x y) in
              true, Term.update def_t blk def
            else false,blk
          | `Jmp jmp ->
            if Seq.exists (Ir_jmp.exps jmp) ~f:(has_sub_exp x) then
              let jmp = Ir_jmp.map_exp jmp ~f:(substitute_exp x y) in
              true, Term.update jmp_t blk jmp
            else false,blk)

exception Finished of sub term

let finish sub blk =
  Exn.raise_without_backtrace (Finished (Term.update blk_t sub blk))

let dom_bind_arg dom root sub (var,exp) =
  let x = Bil.var var in
  let rec bfs root =
    let subst root =
      let blk = Term.find_exn blk_t sub root in
      let finished,blk = substitute blk x exp in
      if finished then finish sub blk in
    subst root;
    Seq.iter (Tree.children dom root) ~f:subst;
    Seq.iter (Tree.children dom root) ~f:bfs in
  try bfs root; sub with Finished sub -> sub

let dom_bind_args sub entry args =
  let module G = Bap_tid_graph in
  let entry = Term.tid entry in
  let cfg = G.create sub in
  let dom = Graphlib.dominators (module G) cfg entry in
  Seq.fold args ~init:sub ~f:(dom_bind_arg dom entry)

(* we do not provide algorithm that will take advantage of SSA form,
   since the latter will work only for variables, but the DOM tree
   algorithm will work correctly for any kind of expression. *)
let bind_args sub = match Term.first blk_t sub with
  | None -> sub
  | Some entry ->
    Term.enum arg_t sub |>
    Seq.map ~f:(fun arg -> Ir_arg.(lhs arg, rhs arg)) |>
    dom_bind_args sub entry
