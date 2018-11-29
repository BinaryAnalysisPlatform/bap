open Bap_core_theory_definition

module Theory : Basic

val register : ?desc:string -> name:string -> (module Basic) -> unit
