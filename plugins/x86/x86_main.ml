open Core_kernel.Std
open Bap.Std
include Self()

module AMD64 = X86_lifter.AMD64
module IA32  = X86_lifter.IA32

let main x32 x64 =
  register_target `x86    (module IA32);
  register_target `x86_64 (module AMD64);
  X86_abi.setup ~abi:(function
      | `x86 -> x32
      | `x86_64 -> x64) ()

let () =
  let () = Config.manpage [
      `S "SYNOPSIS";
      `P "$(b,bap) FILE [--x86-abi=ABI]";
      `P "$(b,bap) FILE [--x86-64-abi=ABI]";
      `S "DESCRIPTION";
      `P "Provides lifter and implements a set of ABI for x86 and
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
    ] in
  let abi arch name =
    let abis = X86_abi.supported () |> List.filter_map ~f:(fun abi ->
        if X86_abi.arch abi = arch
        then Some (X86_abi.name abi, abi)
        else None) in
    let doc = sprintf "Use specified $(docv) as a default one. The $(docv)
    must be %s" @@ Config.doc_enum abis in
    Config.(param (some (enum abis)) name ~doc) in
  let x32 = abi `x86 "abi" in
  let x64 = abi `x86_64 "64-abi" in
  Config.when_ready (fun {Config.get=(!)} -> main !x32 !x64)
