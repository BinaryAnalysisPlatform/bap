open Bap_knowledge
open Bap.Std
open Bap_primus_types

module Main(M : Machine) : sig
  val run :
    ?envp:string array ->
    ?args:string array ->
    project ->
    unit M.t ->
    (exit_status * project) M.m
end

val run :
  ?envp:string array ->
  ?args:string array ->
  project -> Knowledge.state ->
  unit Bap_primus_machine.Make(Knowledge).t ->
  (exit_status * project * Knowledge.state, Knowledge.conflict) result

val add_component : component -> unit
