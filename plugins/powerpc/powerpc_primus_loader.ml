open Core_kernel
open Bap.Std
open Bap_primus.Std
open Powerpc.Std

include Self()

module Component(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let initialize_regs regs =
    let zero = Primus.Generator.static 0 in
    Machine.List.iter regs ~f:(fun r -> Env.add r zero)

  let init32 () =
    let open PowerPC_32 in
    initialize_regs (ctr :: Set.to_list flags)

  let init () =
    Machine.get () >>= fun proj ->
    match Project.arch proj with
    | `ppc -> init32 ()
    | _ -> Machine.return ()

end

let init _ = Primus.Machine.add_component (module Component)

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P
        "Performs the PowerPC target specific setup. So far it just initializes
  all flags and CTR register to zero."
    ] in
  Config.when_ready init
