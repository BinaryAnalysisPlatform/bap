open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge

type t

(** [run prog] - returns the mapping from
    stubs to implementations *)
val run : program term -> t

val stubs : t -> Set.M(Tid).t

val links : t -> tid Map.M(Tid).t
