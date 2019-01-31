open Core_kernel
open Bap.Std

module IA32 : sig
  module CPU : CPU
  val lift : lifter
end

module AMD64 : sig
  module CPU : CPU
  val lift : lifter
end
