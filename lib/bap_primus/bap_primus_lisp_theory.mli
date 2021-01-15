open Bap_core_theory
open Bap_primus_lisp_types

type s

module Lifter(CT : Theory.Core) : sig
  val eval : ast -> s KB.t
end
