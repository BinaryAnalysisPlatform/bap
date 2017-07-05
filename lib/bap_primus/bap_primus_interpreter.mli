open Core_kernel.Std
open Bap.Std
open Bap_primus_types

val enter_term : tid observation
val leave_term : tid observation

val new_value : word observation
val enter_exp : exp observation
val leave_exp : exp observation
val enter_pos : pos observation
val leave_pos : pos observation

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

val halting : unit observation

type exn += Halt

module Make (Machine : Machine) : sig
  type 'a m = 'a Machine.t
  val halt : never_returns m
  val pos : pos m
  val sub : sub term -> unit m
  val blk : blk term -> unit m
  val exp : exp -> word m
end


module Init (Machine : Machine) : sig
  val run : unit -> unit Machine.t
end
