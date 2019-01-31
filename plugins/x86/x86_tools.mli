open Core_kernel
open Bap.Std

module type S = X86_tools_types.S

module IA32 : S
module AMD64 : S

