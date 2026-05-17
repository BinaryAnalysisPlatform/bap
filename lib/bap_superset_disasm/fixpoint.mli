open Bap.Std


val iterate : int -> ('a -> 'a) -> 'a -> 'a
val protect : Superset_impl.t -> (Superset_impl.t -> Superset_impl.t) -> Superset_impl.t
val converge : Superset_impl.t -> 'a -> 'b Addr.Map.t -> Superset_impl.t
