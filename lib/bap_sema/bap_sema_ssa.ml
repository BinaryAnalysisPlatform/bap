(** Transform to Semipruned SSA form.

    The algorithm is adopted from the following sources:

    [1]: Muchnick, Advanced Compiler Design and Implementation
         [ISBN-10: 1558603204]
    [2]: Appel, Modern Compiler Implementation in ML
         [ISBN 0-521-60764-7]
    [3]: Cooper, Engineering a Compiler, Second Edition
         [ISBN-10: 012088478X]

    Basically they describe the same algorithm but in different
    flavors and levels of detail.
*)
open Core_kernel.Std
open Bap_types.Std
open Graphlib.Std
open Format
open Bap_ir

module Cfg = Graphlib.Tid.Tid


type state = {
  sub : sub term;
  cfg : Cfg.t;
  dom : tid tree;
  frontier : tid frontier;
  entry : tid;
  vars : Var.Set.t;
}

let ssa_form = Value.Tag.register
    ~uuid:"9abecd51-c6c3-4363-b9c6-51f923d965fe"
    ~name:"ssa_form"
    (module Unit)


(** [iterated_frontier frontier bs] given a [frontier] function, that
    for a each block [b] returns its dominance frontier, compute an
    iterated dominance frontier of a set of block [bs]. Iterated
    dominance frontier is defined inductively as
    [IDF_1(S) = DF(S); IDF_n(S) = DF(S U IDF_{n-1}(S))],
    where [DF(S)] computes a union of dominance frontiers of each
    block in [S].  The function returns a result of [IDF_k], where
    [k] is a fixpoint, i.e., such value that [IDF_k = IDF_{k-1}].  See
    section 8.11 of [1].*)
let iterated_frontier f blks =
  let df = Set.fold ~init:Tid.Set.empty ~f:(fun dfs b ->
      Seq.fold (Frontier.enum f b) ~init:dfs ~f:Set.add) in
  let blks = List.fold blks ~init:Tid.Set.empty ~f:Set.add in
  let rec fixpoint idf =
    let idf' = df (Set.union idf blks) in
    if Set.equal idf idf' then idf' else fixpoint idf' in
  fixpoint Tid.Set.empty

let blk_of_tid = Term.find_exn blk_t
let succs cfg sub tid =
  Cfg.Node.succs tid cfg |> Seq.map ~f:(blk_of_tid sub)

(** [collect_vars] traverses through subroutine [sub] and collects
    variables, that are live across multiple blocks. *)
let collect_vars sub =
  Term.enum blk_t sub |>
  Seq.fold ~init:Var.Set.empty ~f:(fun vars blk ->
      Set.union vars @@ Ir_blk.free_vars blk)

(** returns a list of blocks that contains [def] terms with lhs equal
    to [var] *)
let blocks_that_define_var var sub : tid list =
  Term.enum blk_t sub |>
  Seq.filter ~f:(fun blk -> Ir_blk.defines_var blk var) |>
  Seq.map ~f:Term.tid |> Seq.to_list_rev

(** [substitute vars exp] take a table of stacks of variables and
    for each variable in an expression [exp] perform a substitution
    of the variable to a top value of a stack for this variable, if it
    is not empty *)
let substitute vars = (object
  inherit Bil.mapper as super
  method! map_sym z =
    match Hashtbl.find vars z with
    | None | Some [] -> z
    | Some (d :: _) -> d
end)#map_exp

let blk sub tid =
  match Term.find blk_t sub tid with
  | Some blk -> blk
  | _ -> failwithf "failed to get block %a" Tid.str tid ()

(** [rename t] performs a renaming of variables in a subroutine
    [t.sub]. An algorithm is described in section 19.7 of [[2]] and 9.12
    of [[3]] (but there is a small error in the latter).  The only
    difference is a naming scheme. The naming scheme is the following:
    we start from an original name of a variable, and rename of the
    following definitions of this variable with [renumber]
    function. It has a nice side effect of cleary showing a first use
    of a variable. And works well with our API to resolve input/output
    parameters.*)
let rename t =
  let vars : var list Var.Table.t = Var.Table.create () in
  let nums : int Var.Table.t = Var.Table.create () in
  let top v = match Hashtbl.find vars v with
    | None | Some [] -> v
    | Some (v :: _) -> v in
  let new_name x =
    Hashtbl.change nums x (function
        | None -> Some 1
        | Some x -> Some (x + 1));
    let n = Hashtbl.find_exn nums x in
    let y = Var.with_index x n in
    Hashtbl.add_multi vars ~key:x ~data:y;
    y in
  let rename_phis blk =
    Term.map phi_t blk ~f:(fun phi ->
        Ir_phi.with_lhs phi (new_name (Ir_phi.lhs phi))) in
  let rename_defs blk =
    Term.map def_t blk ~f:(fun def ->
        let rhs = Ir_def.rhs def |> substitute vars in
        let lhs = new_name (Ir_def.lhs def) in
        Ir_def.with_rhs (Ir_def.with_lhs def lhs) rhs) in
  let rename_jmps blk =
    Term.map jmp_t blk ~f:(Ir_jmp.map_exp ~f:(substitute vars)) in
  let update_phis src dst =
    let tid = Term.tid src in
    Term.map phi_t dst ~f:(fun phi ->
        Ir_phi.values phi |> Seq.fold ~init:phi ~f:(fun phi rhs ->
            match rhs with
            | (id,Bil.Var v) when Tid.(tid = id) ->
              Ir_phi.update phi tid (Bil.var (top v))
            | _ -> phi)) in
  let pop_defs blk =
    let pop v = Hashtbl.change vars (Var.base v) (function
        | Some (x::xs) -> Some xs
        | xs -> xs) in
    Term.enum phi_t blk |>
    Seq.iter ~f:(fun phi -> pop (Ir_phi.lhs phi));
    Term.enum def_t blk |>
    Seq.iter ~f:(fun def -> pop (Ir_def.lhs def)) in

  let rec rename_block sub blk' =
    let tid = Term.tid blk' in
    let blk' = blk sub tid in
    let blk = blk' |> rename_phis |> rename_defs |> rename_jmps in
    let sub = Term.update blk_t sub blk in
    let sub =
      succs t.cfg sub tid |> Seq.fold ~init:sub ~f:(fun sub dst ->
          Term.update blk_t sub (update_phis blk dst)) in
    let children = Cfg.nodes t.cfg |>
                   Seq.filter ~f:(Tree.is_child_of ~parent:tid t.dom) |>
                   Seq.map ~f:(blk_of_tid sub) in
    let sub = Seq.fold children ~init:sub ~f:rename_block in
    pop_defs blk';
    sub in
  rename_block t.sub (blk_of_tid t.sub t.entry)

let has_phi_for_var blk x =
  Term.enum phi_t blk |> Seq.exists ~f:(fun phi ->
      Var.(Ir_phi.lhs phi = x))

(** [insert_phi_node ins blk x]   *)
let insert_phi_node ins blk x =
  if has_phi_for_var blk x then blk
  else Seq.map ins ~f:(fun blk -> Term.tid blk, Bil.var x) |>
       Seq.to_list_rev |> Ir_phi.of_list x |>
       Term.append phi_t blk

(** [insert_phi_nodes t] inserts dummy phi nodes of a form [x <-
    phi(x,x,..,x)] for each variable [x] in [t.vars] in each block
    that needs it. The algorithm computes an iterated dominance
    frontier for each variable as per section 8.11 of [1].*)
let insert_phi_nodes t : sub term =
  Set.fold t.vars ~init:t.sub ~f:(fun sub x ->
      let bs = blocks_that_define_var x sub in
      iterated_frontier t.frontier (t.entry :: bs) |>
      Set.fold ~init:sub ~f:(fun sub tid ->
          let blk = blk_of_tid sub tid in
          let ins = Cfg.Node.preds tid t.cfg |>
                    Seq.map ~f:(blk_of_tid sub) in
          Term.update blk_t sub (insert_phi_node ins blk x)))

let is_transformed sub = Term.has_attr sub ssa_form

(** transforms subroutine into a semi-pruned SSA form.  *)
let sub sub =
  match Term.first blk_t sub with
  | Some entry when not (is_transformed sub) ->
    let entry = Term.tid entry in
    let cfg = Graphlib.Ir.create_tid_graph sub in
    let cfg,sub,entry =
      if Cfg.Node.degree ~dir:`In entry cfg = 0
      then cfg,sub,entry
      else
        let blk = Ir_blk.create () in
        let jmp = Ir_jmp.create_goto (Direct entry) in
        let blk = Term.append jmp_t blk jmp in
        let sub = Term.prepend blk_t sub blk in
        let entry' = Cfg.Node.create (Term.tid blk) in
        let edge = Cfg.Edge.create entry' entry (Term.tid jmp) in
        let cfg = Cfg.Edge.insert edge cfg in
        cfg,sub,entry' in
    let vars = collect_vars sub in
    let dom = Graphlib.dominators (module Cfg) cfg entry in
    let frontier = Graphlib.dom_frontier (module Cfg) cfg dom in
    let t = {entry; dom; frontier; cfg; sub; vars} in
    let sub = rename {t with sub = insert_phi_nodes t}  in
    Term.set_attr sub ssa_form ()
  | _ -> sub
