open Core_kernel
open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute

module External : sig
  val t : string list Attribute.t
end

module Variables : sig
  val t : var list Attribute.t
end

module Advice : sig
  type cmethod = Before | After
  type t
  val t : t

  val cmethod : t -> cmethod
  val targets : t -> String.Set.t
end
