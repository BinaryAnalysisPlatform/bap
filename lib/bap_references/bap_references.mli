open Bap_core_theory

type t

type ref =
  | Addr of Bitvec.t
  | Name of string
[@@deriving compare, sexp, equal]

val slot : (Theory.Unit.cls, t) KB.slot

val lookup : t -> Bitvec.t -> ref option

val provide_from_spec : unit -> unit
