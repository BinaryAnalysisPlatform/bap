open Core_kernel.Std
open Bap.Std
open X86_tools_types

module Insn = Disasm_expert.Basic.Insn
module Table = String.Table

module type Data = sig
  val table : lifter Table.t
end

module Make (D : Data) (PR : PR) = struct
  open D
  let register op lift =
    let lift mem insn =
      try
        let open Or_error in
        match PR.parse mem op with
        | `LOCK_PREFIX -> lift mem insn >>| PR.lock
        | `REP_PREFIX ->  lift mem insn >>| PR.rep mem
        | `REPE_PREFIX ->  lift mem insn >>| PR.repe mem
        | `REPNE_PREFIX -> lift mem insn >>| PR.repne mem
        | `EXCEPTION no -> Ok [ Bil.cpuexn no ]
        | `NONE -> lift mem insn
      with exn -> Or_error.of_exn exn in
    String.Table.set
      table
      ~key:(X86_opcode.sexp_of_t op |> Sexp.to_string)
      ~data:lift

  module Make (T : Target) : Target = struct
    module CPU = T.CPU
    let lift mem insn =
      let if_found = ident in
      let if_not_found _ = T.lift in
      let key = Insn.name insn in
      let lifter =
        Table.find_and_call table key ~if_found ~if_not_found in
      lifter mem insn
  end
end

let make_table =
  String.Table.create ~size:(List.length X86_opcode.all)

module IA32 =
  Make(struct let table = make_table () end) (X86_tools.IA32.PR)

module AMD64 =
  Make(struct let table = make_table () end) (X86_tools.AMD64.PR)
