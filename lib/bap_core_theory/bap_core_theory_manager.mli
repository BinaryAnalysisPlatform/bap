open Bap_knowledge
open Bap_core_theory_definition

val declare :
  ?desc:string ->
  ?extends:string list ->
  ?context:string list ->
  ?provides:string list ->
  ?package:string ->
  name:string ->
  (module Core) knowledge ->
  unit

val instance :
  ?context:string list ->
  ?requires:string list ->
  unit -> theory knowledge

val require : theory -> (module Core) knowledge

module Documentation : sig
  module Theory : sig
    type t
    val name : t -> Knowledge.Name.t
    val desc : t -> string
    val requires : t -> string list
    val provides : t -> string list
  end

  val theories : unit -> Theory.t list
end
