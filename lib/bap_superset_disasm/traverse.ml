open Core
open Bap.Std
open Superset
   
(** Abstract dfs from a given starting point. *)
let iter_component ?(terminator=(fun _ -> true))
    ?visited ?(pre=fun _ -> ()) ?(post=fun _ -> ())  =
  ISG.dfs ~terminator ?visited ~pre ~post ISG.ancestors

(** This function starts at a given address and traverses toward 
 every statically visible descendant. It is used to maximally 
 propagate a given function application. *)
let with_descendents_at ?visited ?post ?pre superset addr =
  ISG.dfs ?visited ?post ?pre ISG.descendants superset addr

(** This function starts at a given address and traverses toward 
 every statically visible ancestor. It is used to maximally 
 propagate a given function application. *)
let with_ancestors_at ?visited ?post ?pre superset addr =
  ISG.dfs ?visited ?post ?pre ISG.ancestors superset addr

(** From the starting point specified, this reviews all descendants 
    and marks their bodies as bad. *)
let mark_descendent_bodies_at ?visited ?datas superset addr =
  let datas = Option.value datas 
      ~default:(Addr.Hash_set.create ()) in
  let mark_bad = Core.mark_bad superset in
  with_descendents_at ?visited superset addr
    ~pre:(fun v ->
        Occlusion.with_data_of_insn superset v ~f:mark_bad;
        Occlusion.with_data_of_insn superset v ~f:(Hash_set.add datas);
      )

(** A clean wrapper around raw superset that does some management of
    visited nodes for efficiency behind the scenes. *)
let visit ?visited ~pre ~post superset entries =
  let visited = Option.value visited
      ~default:(Addr.Hash_set.create ()) in
  let pre addr =
    Hash_set.add visited addr;
    pre addr in
  Hash_set.iter entries ~f:(fun addr ->
      if not (Hash_set.mem visited addr) then
        with_ancestors_at superset ~visited ~pre ~post addr
    )


(** This traversal unlinks all non-branch jumps, collects every
    entry, which now includes newly unlinked blocks in order to
    provide a separation over traversal. *)
let visit_by_block superset
    ?(pre=(fun _ _ _ -> ())) ?(post=(fun _ _ _ -> ())) entries = 
  let (jmps,targets) = Superset.ISG.fold_edges superset
      (fun src target (jmps,targets) -> 
         let is_branch = Superset.is_branch superset target in
         let is_jmp_edge = not (Superset.is_fall_through superset src target) in
         if is_branch && is_jmp_edge then
           (Map.set jmps ~key:src ~data:target, Set.add targets target)
         else (jmps, targets)
      ) (Addr.Map.empty,Addr.Set.empty) in
  let superset = 
    Map.fold jmps ~init:superset ~f:(fun ~key ~data superset -> 
        Superset.ISG.unlink superset key data;
      ) in
  let entries = Superset.entries_of_isg superset in
  let visited = Addr.Hash_set.create () in
  let rec visit v =
    Hash_set.add visited v;
    pre jmps targets v;
    let f w =
      if not (Hash_set.mem visited w) then
        visit w
      else pre jmps targets w in
    let ancs = Superset.ISG.ancestors superset v in
    List.iter ancs ~f;
    post jmps targets v;
  in 
  Hash_set.iter entries ~f:visit;
  Map.fold jmps ~init:superset ~f:(fun ~key ~data superset -> 
      Superset.ISG.link superset key data;
    )

