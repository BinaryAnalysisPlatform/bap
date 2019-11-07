open Bap_knowledge
open Bap_core_theory_definition

val declare :
  ?desc:string ->
  ?extends:string list ->
  ?context:string list ->
  ?provides:string list ->
  ?package:string ->
  name:string -> (module Core) -> unit

val instance :
  ?context:string list ->
  ?requires:string list ->
  unit -> theory knowledge

val require : theory -> (module Core) knowledge
