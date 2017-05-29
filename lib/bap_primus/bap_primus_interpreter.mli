open Core_kernel.Std
open Bap.Std

open Bap_primus_types

module Context = Bap_primus_context

val enter_term : tid observation
val leave_term : tid observation
val enter_level : Context.level observation
val leave_level : Context.level observation

val enter_sub : sub term observation
val enter_arg : arg term observation
val enter_blk : blk term observation
val enter_phi : phi term observation
val enter_def : def term observation
val enter_jmp : jmp term observation

val leave_sub : sub term observation
val leave_arg : arg term observation
val leave_blk : blk term observation
val leave_phi : phi term observation
val leave_def : def term observation
val leave_jmp : jmp term observation

module Make (Machine : Machine) : sig
  type 'a m = 'a Machine.t
  val halt : unit m
  val pos : Context.level m
  val sub : sub term -> unit m
  val blk : blk term -> unit m
  val exp : exp -> word m
end
