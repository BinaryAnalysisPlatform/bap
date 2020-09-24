open Core_kernel
open Bap.Std
open Mips.Std

include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Provides lifter for MIPS architecture"
  ]

module Make(T: Target) : Target = struct
  open Format
  include T

  let pp_insn ppf (mem, insn) =
    fprintf ppf "%a: %s" Addr.pp_hex (Memory.min_addr mem)
      (Disasm_expert.Basic.Insn.asm insn)

  let lift mem insn =
    match lift mem insn with
    | Error err as failed ->
      warning "can't lift instruction %a - %a"
        pp_insn (mem, insn) Error.pp err;
      failed
    | Ok bil as ok -> match Type.check bil with
      | Ok () -> ok
      | Error te ->
        warning "BIL doesn't type check %a - %a"
          pp_insn (mem, insn) Type.Error.pp te;
        Error (Error.of_string "type error")
end

(* For now take the most popular - 32bit big endian *)
module MIPS32 = Make(M32BE)
module MIPS32_le = Make(M32LE)
module MIPS64 = Make(M64BE)
module MIPS64_le = Make(M64LE)

let () =
  Config.when_ready (fun _ ->
      Bap_mips_target.load ();
      register_target `mips (module MIPS32);
      register_target `mipsel (module MIPS32_le);
      register_target `mips64 (module MIPS64);
      register_target `mips64el (module MIPS64_le);
      Mips_abi.setup ());
