open Bap.Std


val iterate : int -> (Superset_impl.t -> Superset_impl.t) -> Superset_impl.t -> Superset_impl.t
val protect : Superset_impl.t -> (Superset_impl.t -> Superset_impl.t) -> Superset_impl.t
val converge : Superset_impl.t -> 'a -> 'b Addr.Map.t -> Superset_impl.t
