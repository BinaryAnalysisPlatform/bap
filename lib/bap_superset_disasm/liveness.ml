open Bap.Std
open Core
open Graphlib.Std

let stmt_def_freevars =
  object(self)
    inherit [Var.Set.t] Stmt.visitor
    method! enter_move def use accu =
      Set.add accu def
  end

type rev_ssa = {
    defs   : Var.Set.t;
    uses   : Var.Set.t;
  }

let transitions superset =
  Superset.ISG.fold_vertex superset (fun addr fs ->
      match Superset.Core.lift_at superset addr with
      | Some bil ->
         Addr.Map.add_exn fs ~key:addr ~data:{
             defs = stmt_def_freevars#run bil Var.Set.empty;
             uses = Bil.free_vars bil;
           }
      | None -> fs
    ) Addr.Map.empty

let (++) = Set.union and (--) = Set.diff

let compute_liveness superset =
  let start = Addr.of_int ~width:1 0 in
  let _exit = Addr.of_int ~width:1 1 in
  let entries = Superset.entries_of_isg superset in
  let superset = Hash_set.fold ~init:superset entries
    ~f:(fun s e -> Superset.ISG.link s _exit e) in
  let frond = Superset.frond_of_isg superset in
  let superset = Hash_set.fold frond ~init:superset 
                   ~f:(fun s e -> Superset.ISG.link s e start) in
  let init = Solution.create Addr.Map.empty Var.Set.empty in
  let tran = transitions superset in
  let soln = Superset.ISG.fixpoint superset ~init ~start ~rev:false
    ~merge:Var.Set.union ~equal:Var.Set.equal ?step:None ?steps:None
    ~f:(fun n vars ->
        if Addr.equal n _exit || Addr.equal n start then vars
        else
          match Map.find tran n with
          | Some {defs; uses} ->
             vars -- defs ++ uses
          | None -> vars
    ) in
  let superset = Superset.Core.remove superset _exit in
  let _ = Superset.Core.remove superset start in
  let liveness_pairs = Solution.enum soln in
  Seq.fold liveness_pairs ~init:Addr.Set.empty ~f:(fun s (addr,_) ->
      Addr.Set.add s addr)  

