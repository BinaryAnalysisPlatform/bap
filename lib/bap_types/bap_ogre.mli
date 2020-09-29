open Bap_core_theory
open Core_kernel

type t = Ogre.doc [@@deriving bin_io, compare, sexp]
val pp : Format.formatter -> t -> unit
val slot : (Theory.Unit.cls, t) KB.slot
