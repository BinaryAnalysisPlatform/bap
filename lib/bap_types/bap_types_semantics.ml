open Bap_core_theory
open Core_kernel
open Bap_knowledge

type cls = unit Theory.Effect.spec
let cls = Theory.Effect.top
let empty = Knowledge.Value.empty cls
include (val Knowledge.Value.derive cls)
