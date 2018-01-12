open Core_kernel.Std
open Bap.Std
open Bap_powerpc.Std

include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Provides lifter PowerPC architecture.";
  ]

module Make(T : Target) : Target = struct
  open Format
  include T

  let pp_insn ppf (mem,insn) =
    fprintf ppf "%a: %s"
      Addr.pp_hex (Memory.min_addr mem)
      (Disasm_expert.Basic.Insn.asm insn)

  let lift mem insn =
    match lift mem insn with
    | Error err as failed ->
      warning "can't lift instruction %a - %a"
        pp_insn (mem,insn) Error.pp err;
      failed
    | Ok bil as ok -> match Type.check bil with
      | Ok () -> ok
      | Error te ->
        warning "BIL doesn't type check %a - %a"
          pp_insn (mem,insn) Type.Error.pp te;
        Error (Error.of_string "type error")
end

module PowerPC32 = Make(T32)
module PowerPC64 = Make(T64)
module PowerPC64_le = Make(T64_le)

let () =
  Config.when_ready (fun _ ->
      register_target `ppc (module PowerPC32);
      register_target `ppc64 (module PowerPC64);
      register_target `ppc64le (module PowerPC64_le));
  Powerpc_abi.setup ()
