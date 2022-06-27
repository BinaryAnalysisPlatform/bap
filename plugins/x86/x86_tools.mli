open Core_kernel[@@warning "-D"]
open Bap.Std

module type S = X86_tools_types.S

module IA32 : S
module AMD64 : S
