open Core_kernel.Std
open Bap.Std
open X86_tools_types

module Make (CPU : X86CPU) (RR : RR) (IM : IM) : MM
