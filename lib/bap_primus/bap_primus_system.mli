open Bap.Std
open Bap_knowledge
open Bap_primus_types

module Observation = Bap_primus_observation

type t
type system = t
type info

type component_specification
type system_specification
type parse_error

val define :
  ?desc:string ->
  ?depends_on:system_specification list ->
  ?components:component_specification list ->
  ?package:string -> string -> t

val add_component : ?package:string -> t -> string -> t

val component : ?package:string -> string -> component_specification
val depends_on : ?package:string -> string -> system_specification

val name : t -> Knowledge.Name.t

val from_file : string -> (t list,parse_error) Result.t

val pp : Format.formatter -> t -> unit
val pp_parse_error : Format.formatter -> parse_error -> unit

module Repository : sig
  val add : system -> unit
  val get : ?package:string -> string -> system
  val find : Knowledge.Name.t -> system option
  val update : ?package:string -> string -> f:(system -> system) -> unit
  val list : unit -> info list
end

module Components : sig
  open Bap_primus_types

  val register_generic :
    ?internal:bool ->
    ?desc:string ->
    ?package:string ->
    string -> component ->
    unit

  val register :
    ?internal:bool ->
    ?desc:string ->
    ?package:string ->
    string -> unit Bap_primus_machine.Make(Knowledge).t ->
    unit

  val list : unit -> info list
end

module Generic(Machine : Bap_primus_types.Machine) : sig
  val run :
    ?envp:string array ->
    ?args:string array ->
    ?init:unit Machine.t ->
    ?start:unit Machine.t ->
    t -> project -> (exit_status * project) Machine.m
end

val run :
  ?envp:string array ->
  ?args:string array ->
  ?init:unit Bap_primus_machine.Make(Knowledge).t ->
  ?start:unit Bap_primus_machine.Make(Knowledge).t ->
  t -> project -> Knowledge.state ->
  (exit_status * project * Knowledge.state, Knowledge.conflict) result

val start : string Observation.t
val init : unit Observation.t
val fini : unit Observation.t
val stop : string Observation.t

module Job : sig
  type t

  val name : t -> string
  val desc : t -> string
  val envp : t -> string array
  val args : t -> string array
  val system : t -> system
end

module Jobs : sig
  val enqueue :
    ?name:string ->
    ?desc:string ->
    ?envp:string array ->
    ?args:string array ->
    ?init:unit Bap_primus_machine.Make(Knowledge).t ->
    ?start:unit Bap_primus_machine.Make(Knowledge).t ->
    system -> unit
  val pending : unit -> int

  type action = Stop | Continue
  type result

  val knowledge : result -> Knowledge.state
  val project : result -> project
  val failures : result -> (Job.t * Knowledge.conflict) list
  val finished : result -> Job.t list

  val run :
    ?on_failure:(Job.t -> Knowledge.conflict -> action) ->
    ?on_success:(Job.t -> exit_status -> Knowledge.state -> action) ->
    project -> Knowledge.state -> result
end

module Info : sig
  val name : info -> Knowledge.Name.t
  val desc : info -> string
  val long : info -> string
  val pp : Format.formatter -> info -> unit
end
