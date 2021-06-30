open Core_kernel
open Bap.Std
open Powerpc.Std

include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Provides lifter for PowerPC architecture.";
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

let backend = Config.param Config.(some string) "backend"

let () =
  Config.when_ready (fun {get} ->
      info "Providing PowerPC semantics in BIL";
      Bap_powerpc_target.load ?backend:(get backend)();
      Powerpc_add.init ();
      Powerpc_branch.init ();
      Powerpc_compare.init ();
      Powerpc_cr.init ();
      Powerpc_div.init ();
      Powerpc_load.init ();
      Powerpc_logical.init ();
      Powerpc_move.init ();
      Powerpc_mul.init ();
      Powerpc_rotate.init ();
      Powerpc_shift.init ();
      Powerpc_store.init ();
      Powerpc_sub.init ();
      register_target `ppc (module PowerPC32);
      register_target `ppc64 (module PowerPC64);
      register_target `ppc64le (module PowerPC64_le);
      Powerpc_abi.setup ());
