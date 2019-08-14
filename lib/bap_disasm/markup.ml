open Bap_types.Std
open Bap_image_std
open Core_kernel.Std

let mark_threshold_with_pmap ?visited ?datas superset pmap threshold = 
  let visited = Option.value visited 
      ~default:(Addr.Hash_set.create ()) in
  let datas = Option.value datas
      ~default:(Addr.Hash_set.create ()) in
  let insn_risg = Superset.get_graph superset in
  let insn_isg = Superset_risg.Oper.mirror insn_risg in
  Map.iteri pmap ~f:(fun ~key ~data ->
      let addr = key in
      let p = data in
      if p > threshold then (
        if Superset_risg.G.mem_vertex insn_risg addr then
          Superset.mark_descendents_at
            ~datas ~visited ~insn_isg superset addr;
      )
    )

let mark_tps superset visited = 
  let insn_risg = Superset.get_graph superset in
  (*if Superset_risg.G.mem_vertex insn_risg bad then*)
  Hash_set.iter visited 
    ~f:(fun tp -> 
        if Superset_risg.G.mem_vertex insn_risg tp then
          Superset.clear_bad superset tp)

let collect_bad superset =
  let visited = Addr.Hash_set.create () in
  let _ = Superset.with_bad superset ~visited 
      ~pre:(fun _ _ -> ()) ~post:(fun _ _ -> ()) () in
  visited

let clear_bad superset = Superset.clear_all_bad superset

(* TODO need to do more to make sure that those are actually
   retained, since it may be the case that a descendant identifies as
   bad as well.
   1) may be able to tack this onto trim, so that we disable it at
   retain points. would make sense if the superset type was extensible.
*)
let enforce_uncertain superset visited datas pmap =
  Map.iteri !pmap ~f:(fun ~key ~data -> 
      let addr = key in
      let prob = data in      
      let mark_good addr =
        Superset.clear_bad superset addr in
      if prob >= 0.85 then
        mark_good addr
    )

let check_convergence superset visited =
  Hash_set.iter visited ~f:(fun tp -> 
      Superset.clear_bad superset tp
    )

