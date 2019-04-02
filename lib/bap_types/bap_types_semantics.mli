open Bap_knowledge
open Bap_core_theory
open Bap_bil

type cls = unit Theory.Effect.spec
type t = cls Knowledge.value [@@deriving bin_io, compare, sexp]
val cls : cls Knowledge.cls
include Base.Comparable.S with type t := t
