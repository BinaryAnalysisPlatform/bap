open Bap.Std

val lift : lifter

module CPU : sig
  include module type of Arm_env
  include CPU
end
