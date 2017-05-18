open Core_kernel.Std
open Bap.Std

open Bap_primus_types

module Context = Bap_primus_context

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
val variable_read : (var * word) observation
val variable_written : (var * word) observation

val address_access : addr observation
val address_read : (addr * word) observation
val address_written : (addr * word) observation

module Make (Machine : Machine) : sig
  val sema : Semantics(Machine).t
end
