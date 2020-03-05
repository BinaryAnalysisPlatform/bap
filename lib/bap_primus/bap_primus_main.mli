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

val add_component : component -> unit
