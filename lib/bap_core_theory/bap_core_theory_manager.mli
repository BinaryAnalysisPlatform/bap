open Bap_core_theory_definition

val instance :
  ?context:string list ->
  ?requires:string list ->
  unit -> (module Core)

val declare :
  ?desc:string ->
  ?context:string list ->
  ?provides:string list ->
  ?package:string ->
  name:string -> (module Core) -> unit

val require : ?context:string list -> ?package:string -> string -> (module Core)
