open Core_kernel
open Bap_types.Std
open Bap_image_std

module Insn = Bap_disasm_insn

type insn = Insn.t

type t = (mem * insn option) list

module Dis = Bap_disasm_basic
module Targets = Bap_disasm_target_factory

let lifter_of_arch arch =
  let module Target = (val Targets.target_of_arch arch) in
  Target.lift


let sweep ?(backend="llvm") arch mem : (mem * insn option) list Or_error.t =
  let open Or_error.Monad_infix in
  Dis.with_disasm ~backend (Arch.to_string arch) ~f:(fun dis ->
      let dis = Dis.store_asm dis in
      let dis = Dis.store_kinds dis in
      let lift = lifter_of_arch arch in
      Dis.run dis mem
        ~init:[] ~return:ident ~stopped:(fun s _ ->
            Dis.stop s (Dis.insns s)) |>
      List.map ~f:(function
          | mem, None -> mem,None
          | mem, Some insn -> match lift mem insn with
            | Ok bil -> mem, Some (Insn.of_basic ~bil insn)
            | _ -> mem, Some (Insn.of_basic insn)) |>
      Or_error.return)


module With_exn = struct
  let sweep ?backend arch mem = ok_exn (sweep ?backend arch mem)
end
