open Core_kernel.Std
open Bap.Std

open Primus_types

module Context = Primus_context

val enter_term : tid observation
val leave_term : tid observation
val enter_level : Context.level observation
val leave_level : Context.level observation

val enter_top : program term observation
val enter_sub : sub term observation
val enter_arg : arg term observation
val enter_blk : blk term observation
val enter_phi : phi term observation
val enter_def : def term observation
val enter_jmp : jmp term observation

val leave_top : program term observation
val leave_sub : sub term observation
val leave_arg : arg term observation
val leave_blk : blk term observation
val leave_phi : phi term observation
val leave_def : def term observation
val leave_jmp : jmp term observation

val variable_access : var observation
val variable_read : (var * Bil.result) observation
val variable_written : (var * Bil.result) observation

val address_access : addr observation
val address_read : (addr * Bil.result) observation
val address_written : (addr * word) observation

module Make (Machine : Machine) : sig
  module Biri : Biri.S
    with type ('a,'e) state = ('a,'e) Machine.t
  class ['a] t : object
    inherit ['a] Biri.t
    constraint 'a = #Primus_context.t
  end
end
