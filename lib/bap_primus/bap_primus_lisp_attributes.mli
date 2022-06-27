open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute

module External : sig
  val t : String.Set.t Attribute.t
end

module Variables : sig
  val global : Set.M(Bap_primus_lisp_var).t Attribute.t
  val static : Set.M(Bap_primus_lisp_var).t Attribute.t
end

module Advice : sig
  type cmethod = Before | After
  type t
  val t : t Attribute.t
  val targets : t -> cmethod -> Set.M(KB.Name).t
end

module Visibility : sig
  type t
  val t : t Attribute.t
  val is_public : t -> bool
end
