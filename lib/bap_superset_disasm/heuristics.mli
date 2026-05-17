open Bap.Std

val get_callsites : ?threshold:(int) -> Superset_impl.t -> Addr.Hash_set.t
val tag_callsites : Addr.Hash_set.t -> ?callsites:Addr.Hash_set.t -> Superset_impl.t -> Superset_impl.t
val with_featureset : f:(string -> (Superset_impl.t -> Superset_impl.t) -> 'a -> 'a)  -> init:'a -> string list -> 'b -> 'a
val with_featurepmap : string list -> Superset_impl.t -> f:((int * word * string) list Addr.Map.t -> string list -> Superset_impl.t -> 'a) -> 'a
val defaults : string list
