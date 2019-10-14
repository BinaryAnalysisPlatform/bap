open Core_kernel
open Bap.Std
include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Provides lifter and ABI processor for ARM architecture.";
    `S "SEE ALSO";
    `P "$(b,bap-arm)(3)"
  ]

module ARM = struct
  open Format
  include ARM

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


let () =
  Config.when_ready (fun _ ->
      List.iter Arch.all_of_arm ~f:(fun arch ->
          register_target (arch :> arch) (module ARM);
          Arm_gnueabi.setup ()))
