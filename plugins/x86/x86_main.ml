open Bap.Std
include Self()

module AMD64 = X86_lifter.AMD64
module IA32  = X86_lifter.IA32

module Cmldine = struct
  open Cmdliner

  let man = [
    `S "SYNOPSIS";
    `P "$(b,bap) FILE [--x86-abi=ABI]";
    `P "$(b,bap) FILE [--x86-64-abi=ABI]";
    `P "$(b,bap) FILE [--x86-list-abi] [--x86-64-list-abi]";
    `S "DESCRIPTION";
    `P "Provides lifter and implements a seto of ABI for x86 and
    x86-64 architectures. No parameters are usually required. Once the
    pluing is installed it will automatically provide it
    services. However it is possible to override default abi, e.g.,
    when working with an executable compiled for MS Windows
    platform. For a full set of supported ABI use corresponding list
    option.";
    `S "EXAMPLE";
    `P "To choose a correct ABI for a binary compiled for MS Windows
    x64 target, use the following:";
    `P "$(b,bap) FILE --x86-64=ms";
  ]

  let abi name =
    let doc = "Use specified ABI as a default one" in
    Arg.(value & opt (some string) None & info [name] ~doc)

  let x32 = abi "abi"
  let x64 = abi "64-abi"
end

let () =
  register_target `x86    (module IA32);
  register_target `x86_64 (module AMD64)
