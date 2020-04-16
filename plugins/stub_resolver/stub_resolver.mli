open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge

(** [run prog] - returns the mapping from
    stubs to implementations *)
val run : program term -> tid Tid.Map.t
