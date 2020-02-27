open Bap.Std
open Bap_knowledge
open Bap_primus_types

module Observation = Bap_primus_observation

type t

type component_specification
type parse_error

val define :
  ?desc:string ->
  ?components:component_specification list ->
  ?package:string -> string -> t

val default : t

val component : ?package:string -> string -> component_specification
val exclude : component_specification -> component_specification

val name : t -> Knowledge.Name.t
val from_file : string -> (t list,parse_error) Result.t

val all_components : component_specification

val pp : Format.formatter -> t -> unit
val pp_parse_error : Format.formatter -> parse_error -> unit


module Components : sig
  open Bap_primus_types

  val register_generic :
    ?desc:string ->
    ?package:string ->
    string -> Bap_primus_types.component ->
    unit

  val register :
    ?desc:string ->
    ?package:string ->
    string -> unit Bap_primus_machine.Make(Knowledge).t ->
    unit
end


module Generic(Machine : Bap_primus_types.Machine) : sig
  val init : t -> unit Machine.t
  val run :
    ?env:string array ->
    ?argv:string array ->
    t -> project -> (exit_status * project) Machine.m
end

val run :
  ?env:string array ->
  ?argv:string array ->
  t -> project -> Knowledge.state ->
  (exit_status * project * Knowledge.state, Knowledge.conflict) result

val fini : unit Observation.t
val finish : unit Observation.statement

val init : unit Observation.t
val inited : unit Observation.statement

module Jobs : sig
  val enqueue : t -> unit
  val pending : unit -> int

  type action = Stop | Continue
  type result

  val knowledge : result -> Knowledge.state
  val project : result -> project
  val conflicts : result -> (t * Knowledge.conflict) list
  val systems : result -> t list

  val run :
    ?env:string array ->
    ?argv:string array ->
    ?on_conflict:(t -> Knowledge.conflict -> action) ->
    ?on_success:(t -> exit_status -> Knowledge.state -> action) ->
    project -> Knowledge.state -> result
end
