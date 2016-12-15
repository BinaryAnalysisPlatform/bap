open Core_kernel.Std
open Or_error
open Bap.Std
open X86_tools_types

module Insn = Disasm_expert.Basic.Insn
module Table = String.Table

type opcode = string

module type S = sig
  val register : opcode -> lifter -> unit
  module Make (T : Target) : Target
end

module Make (PR : PR) = struct

  let lifts : lifter Table.t = Table.create ()

  let register opcode lift =
    Table.set lifts ~key:opcode ~data:lift

  module Make (T : Target) : Target = struct
    module CPU = T.CPU

    let search tab insn =
      Option.value ~default:T.lift (Table.find tab (Insn.name insn))

    let lift mem insn =
      let lift = search lifts insn in
      lift mem insn

  end
end

module IA32 = Make (X86_tools.IA32.PR)
module AMD64 = Make (X86_tools.AMD64.PR)
