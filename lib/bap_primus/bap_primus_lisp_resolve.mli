open Bap.Std

open Bap_primus_lisp_types
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Program = Bap_primus_lisp_program

type resolution

type ('t,'a,'b) resolver =
  Program.t -> 't Def.t Program.item -> string -> 'a ->
  ('t Def.t* 'b,resolution) result option


val extern : arch -> (Def.func, value list, (var * value) list) resolver
val defun : arch -> (Def.func, value list, (var * value) list) resolver
val macro : (Def.macro, tree list, (string * tree list) list) resolver
val primitive : ('a Def.primitive, unit, unit) resolver
val subst : (Def.subst, unit, unit) resolver
val const : (Def.const, unit, unit) resolver
