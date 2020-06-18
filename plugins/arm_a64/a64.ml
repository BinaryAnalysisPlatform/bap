open Core_kernel
open Bap_core_theory
open Bap.Std
open KB.Syntax

open A64_env
module Defs = A64_defs

type insns = Defs.insn * (Defs.op list)

module A64(Core: Theory.Core) = struct
    open Core

end