open Core_kernel
open Bap.Std
include Self()

module Thumb     = Thumb_lifter.Thumb

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Provides lifter and ABI processor for ARM architecture.";
    `S "SEE ALSO";
    `P "$(b,bap-arm)(3)"
  ]


let inst_test () = let open Or_error in Disasm.of_mem (Arch.of_string "thumb" |> Option.value_exn) (Memory.create BigEndian (Word.of_int 32 0) 
                  (Bigstring.of_string "\xbf\x00") |> Result.ok |> Option.value_exn ) >>| Disasm.insns

let () =
  Config.when_ready (*fun _ -> print_string "reg thumb";
      List.iter Arch.all_of_arm ~f:(fun arch ->
        Arch.to_string (arch :> arch) |> print_endline ;
          register_target (arch :> arch) (module Thumb);
          Thumb_gnueabi.setup ());
      List.iter Arch.all_of_thumb ~f:(fun arch ->
        Arch.to_string (arch :> arch) |> print_endline ;
          register_target (arch :> arch) (module Thumb);
          Thumb_gnueabi.setup ())*) (const ());
