open Core_kernel
open Bap_types.Std
open Bap_image_std
open Bap_knowledge
open Bap_core_theory

type state [@@deriving bin_io]
type insns
type jump

val init : state
val equal : state -> state -> bool
val scan : mem -> state -> state knowledge
val merge : state -> state -> state

val subroutines : state -> Set.M(Addr).t
val blocks : state -> Set.M(Addr).t
val jump : state -> addr -> jump option

val is_data : state -> addr -> bool
val is_subroutine : state -> addr -> bool
val is_jump : state -> addr -> bool
val is_block : state -> addr -> bool

val destinations : jump -> Set.M(Addr).t
val is_call : jump -> bool
val is_barrier : jump -> bool

val explore :
  ?entries:addr seq ->
  ?entry:addr ->
  ?follow:(addr -> bool knowledge) ->
  block:(mem -> insns -> 'n knowledge) ->
  node:('n -> 'c -> 'c knowledge) ->
  edge:('n -> 'n -> 'c -> 'c knowledge) ->
  init:'c ->
  state -> 'c knowledge


val list_insns : ?rev:bool -> insns -> Theory.Label.t list
val execution_order : insns -> Theory.Label.t list knowledge
