open Core_kernel
open Bap_types.Std
open Bap_image_std
open Bap_knowledge
open Bap_core_theory
module Dis = Bap_disasm_basic

type state
type insns

val init : state
val scan : mem -> state -> state knowledge
val merge : state -> state -> state

val explore :
  ?entry:addr ->
  ?follow:(addr -> bool knowledge) ->
  block:(mem -> insns -> 'n knowledge) ->
  node:('n -> 'c -> 'c knowledge) ->
  edge:('n -> 'n -> 'c -> 'c knowledge) ->
  init:'c ->
  state -> 'c knowledge


val list_insns : ?rev:bool -> insns -> Theory.Label.t list
val execution_order : insns -> Theory.Label.t list knowledge
