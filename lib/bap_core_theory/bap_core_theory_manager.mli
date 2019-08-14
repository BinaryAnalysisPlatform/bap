open Bap_core_theory_definition

module Theory : Core

val register : ?desc:string -> name:string -> (module Core) -> unit
