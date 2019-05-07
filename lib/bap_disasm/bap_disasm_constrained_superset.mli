open Core_kernel
open Bap_types.Std
open Bap_image_std
open Bap_knowledge
open Bap_core_theory

type t

val create : ?backend:string -> arch -> t Or_error.t
val scan : t -> mem -> t knowledge

type insns

val list_insns : ?rev:bool -> insns -> (mem * Theory.Label.t) list

val execution_order : delay:(Theory.Label.t -> int knowledge) -> insns ->
  (mem * Theory.Label.t) list knowledge

val explore :
  ?entry:addr ->
  ?follow:(addr -> bool knowledge) ->
  block:(mem -> insns -> 'n knowledge) ->
  node:('n -> 'c -> 'c knowledge) ->
  edge:('n -> 'n -> 'c -> 'c knowledge) ->
  init:'c ->
  t -> 'c knowledge
