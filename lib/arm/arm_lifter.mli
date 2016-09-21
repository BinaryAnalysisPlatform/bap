open Bap.Std

(** [lift mem insn] lifts instruction.  *)
val lift : lifter

module CPU : sig
  include module type of Arm_env
  include Bap.Std.CPU
end
