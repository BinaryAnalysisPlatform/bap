open Bap.Std

open Bap_primus_lisp_types
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Program = Bap_primus_lisp_program
module Value = Bap_primus_value


type resolution

type ('t,'a,'b) resolver =
  Program.t -> 't Program.item -> string -> 'a ->
  ('t Def.t * 'b,resolution) result option


val extern : (typ -> 'a -> bool) -> (Def.func, 'a list, (var * 'a) list) resolver
val defun  : (typ -> 'a -> bool) -> (Def.func, 'a list, (var * 'a) list) resolver
val macro : (Def.macro, tree list, (string * tree list) list) resolver
val primitive  : (Def.closure, unit, unit) resolver
val subst : (Def.subst, unit, unit) resolver
val const : (Def.const, unit, unit) resolver

val pp_resolution: Format.formatter -> resolution -> unit
