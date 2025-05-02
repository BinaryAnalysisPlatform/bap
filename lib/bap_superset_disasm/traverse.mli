open Bap.Std

val mark_descendent_bodies_at : ?visited:Bap.Std.Addr.Hash_set.t -> 
        ?datas:Addr.Hash_set.t ->
        Superset_impl.t -> addr -> unit
val with_descendents_at : ?visited:Addr.Hash_set.t -> 
        ?post:(addr -> unit) -> 
        ?pre:(addr -> unit) -> Superset_impl.t -> addr -> unit
val with_ancestors_at : ?visited:Addr.Hash_set.t -> 
        ?post:(addr -> unit) -> 
        ?pre:(addr -> unit) -> Superset_impl.t -> addr -> unit
val visit : ?visited:Addr.Hash_set.t -> pre:(addr -> unit) -> 
        post:(addr -> unit) ->
        Superset_impl.t -> Addr.Hash_set.t -> unit
val visit_by_block : Superset_impl.t -> ?pre:(addr Addr.Map.t -> Addr.Set.t -> addr -> unit) ->
        ?post:(addr Addr.Map.t -> Addr.Set.t -> addr -> unit) -> Addr.Hash_set.t -> Superset_impl.t
