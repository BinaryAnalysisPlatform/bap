open Core_kernel.Std
open Bap.Std

let () =
  List.iter Arch.all_of_arm ~f:(fun arch ->
      register_target (arch :> arch) (module ARM))
