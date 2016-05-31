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

module Cmdline = struct
  open Cmdliner

  let man = [
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
  ]

  let abi arch name =
    let abis = X86_abi.supported () |> List.filter_map ~f:(fun abi ->
        if X86_abi.arch abi = arch
        then Some (X86_abi.name abi, abi)
        else None) in
    let doc = sprintf "Use specified $(docv) as a default one. The $(docv)
    must be %s" @@ Arg.doc_alts_enum abis in
    Arg.(value & opt (some (enum abis)) None & info [name] ~doc)

  let x32 = abi `x86 "abi"
  let x64 = abi `x86_64 "64-abi"
  let info = Term.info ~man name ~doc
  let args = Term.(const main $x32 $x64),info
end


let () = match Cmdliner.Term.eval ~argv Cmdline.args with
  | `Ok () -> ()
  | `Version | `Help -> exit 0
  | `Error _ -> exit 1
