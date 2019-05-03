open Core_kernel
open Graphlib.Std
open Bap_types.Std
open Bap_image_std

module Dis = Bap_disasm_basic
type t
type edge = [`Jump | `Cond | `Fall]

val create : ?backend:string -> arch -> t Or_error.t
val scan :
  ?entries:addr list ->
  ?is_code:(addr -> bool option) ->
  t -> mem -> t

val explore :
  ?entry:addr ->
  ?follow:(addr -> bool) ->
  lift:(mem -> (mem * Dis.full_insn) list -> 'n) ->
  node:('n -> 'c -> 'c) ->
  edge:('n -> 'n -> edge -> 'c -> 'c) ->
  init:'c ->
  t -> 'c
