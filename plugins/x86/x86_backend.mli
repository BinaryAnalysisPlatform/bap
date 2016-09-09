open Core_kernel.Std
open Bap.Std

module IA32 : sig
  val register : X86_opcode.t -> lifter -> unit
  module Make (T : Target) : Target
end

module AMD64 : sig
  val register : X86_opcode.t -> lifter -> unit
  module Make (T : Target) : Target
end
