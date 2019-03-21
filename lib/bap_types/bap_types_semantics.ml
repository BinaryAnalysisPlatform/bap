open Core_kernel
open Bap_knowledge

let package = "bap.std"

type semantics = unit -> unit
type cls = semantics
let cls = Knowledge.Class.declare ~package "semantic-value" ()
let empty = Knowledge.Value.empty cls
include (val Knowledge.Value.derive cls)
