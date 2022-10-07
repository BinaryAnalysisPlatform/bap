open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_core_theory
open Bap_knowledge

type t

(** [run prog ~link_only ~no_link] returns the mapping from
    stubs to implementations.

    If [link_only] is non-empty then only the implementations
    in this set will be considered.

    Similarly, any names in [no_link] will not be considered.
*)
val run :
  ?link_only:String.Set.t ->
  ?no_link:String.Set.t ->
  program term ->
  t

val stubs : t -> Set.M(Tid).t

val links : t -> tid Map.M(Tid).t
