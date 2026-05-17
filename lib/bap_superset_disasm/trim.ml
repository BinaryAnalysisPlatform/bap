open Core
open Bap.Std

let run superset =
  let accu = () in
  let check_pre _ accu _ = accu in
  let check_post _ accu _ = accu in
  let check_elim _ _ _ = true in
  let mark _ _ _ = () in
  let post superset accu addr =
    let module G = Superset.ISG in
    if check_elim superset accu addr then (
      mark superset accu addr;
    );
    check_post superset accu addr in
  let visited = Addr.Hash_set.create () in
  (*let superset = Superset.Core.rebalance superset in*)
  let post = post superset in
  let pre = check_pre superset in
  let _ = Superset.with_bad superset ~visited ~pre ~post accu in
  let superset = 
    Hash_set.fold visited ~init:superset ~f:(fun superset addr ->
        Superset.Core.remove superset addr
      ) in
  Hash_set.clear visited;
  Superset.Core.clear_all_bad superset;
  superset
