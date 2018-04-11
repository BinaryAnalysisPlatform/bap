open Core_kernel
open Bap.Std

type opcode = string
 
module type S = sig
  val register : opcode -> lifter -> unit  
  module Make (T : Target) : Target
end

module IA32  : S
module AMD64 : S
