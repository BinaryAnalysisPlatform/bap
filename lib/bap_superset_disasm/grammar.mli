open Bap.Std

val tag_loop_contradictions : ?min_size:int -> Superset_impl.t -> Superset_impl.t
val tag_by_traversal : ?threshold:int -> Superset_impl.t -> Superset_impl.t
val linear_branch_sweep : Superset_impl.t -> Addr.Hash_set.t -> Addr.Hash_set.t
val identify_branches : Superset_impl.t -> Addr.Hash_set.t
val addrs_of_loops : addr list list -> Addr.Set.t
val addrs_of_filtered_loops : ?min_size:int -> Superset_impl.t -> Addr.Set.t
