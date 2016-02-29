open Core_kernel.Std
open Format

type scheme
type 'a symbolizer = ('a -> string)

(** [create_scheme ~next init] create a name generator, that will
    start with [init] and apply [next] on it infinitly. *)
val create_scheme : next:(string -> string) -> string -> scheme

(** lower case symbols, starting from 'a' and moving up to 'z'.
    As 'z' is reached, all foregoing symbols will have a form
    of 'node_N' where 'N' is an increasing natural number. *)
val symbols : scheme

(** numbers from zero to inifinity ([Sys.max_int] in fact) *)
val numbers : scheme
(** empty string  *)
val nothing : scheme

val by_given_order : scheme -> ('a -> 'a -> int) -> 'a Sequence.t -> 'a symbolizer
val by_natural_order : scheme -> ('a -> 'a -> int) -> 'a Sequence.t -> 'a symbolizer



module Dot : sig
  val pp_graph :
    ?name:string ->
    ?attrs:string list ->
    ?string_of_node: 'n symbolizer ->
    ?node_label: 'n symbolizer ->
    ?edge_label: 'e symbolizer ->
    nodes_of_edge : ('e -> 'n * 'n) ->
    nodes: 'n Sequence.t ->
    edges: 'e Sequence.t -> formatter -> unit

end
