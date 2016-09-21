open Core_kernel.Std
open Bap.Std
include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Provides lifter and ABI processor for ARM architecture.";
    `S "SEE ALSO";
    `P "$(b,bap-arm)(3)"


  ]


let () =
  Config.when_ready (fun _ ->
  List.iter Arch.all_of_arm ~f:(fun arch ->
      register_target (arch :> arch) (module ARM));
  Arm_gnueabi.setup ())
