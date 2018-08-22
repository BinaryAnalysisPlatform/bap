open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Bap_service
open X86_target
include Self()

type kind = Legacy | Modern | Merge [@@deriving sexp]

let declare_abi_parameter arch name =
  let abis = X86_abi.supported () |> List.filter_map ~f:(fun abi ->
      if X86_abi.arch abi = arch
      then Some (X86_abi.name abi, abi)
      else None) in
  let doc = sprintf "Use specified $(docv) as a default one. The $(docv)
    must be %s" @@ Config.doc_enum abis in
  Config.(param (some (enum abis)) name ~doc)

let kind =
  let kinds = ["legacy", Legacy;
               "modern", Modern;
               "merge", Merge] in
  let default = Merge in
  let doc =
    sprintf "Debug purpose only. The $(docv) must be %s. Default: $(docv) = %s."
      (Config.doc_enum kinds)
      (List.find_map_exn kinds
         ~f:(fun (name, kind) ->
             Option.some_if (kind = default) name)) in
  Config.(param (enum kinds) "lifter" ~doc ~default)


let x32_abi = declare_abi_parameter `x86 "abi"
let x64_abi = declare_abi_parameter `x86_64 "64-abi"

let lifter = Service.(provide lifter "edu.cmu.ece.bap/x86" [
    parameter kind;
  ])

let abi = Service.(provide abi "edu.cmu.ece.bap/x86" [
    parameter x64_abi;
    parameter x32_abi;
  ])


let register_lifter kind  =
  let ia32, amd64 = match kind with
    | Legacy -> (module IA32L : Target), (module AMD64L : Target)
    | Modern -> (module IA32 : Target), (module AMD64 : Target)
    | Merge -> (module IA32M : Target), (module AMD64M : Target) in
  register_target `x86 ia32;
  register_target `x86_64 amd64

let register_abi (!!) =
  X86_abi.setup ~abi:(function
      | `x86 -> !!x32_abi
      | `x86_64 -> !!x64_abi) ()

let () =
  Stream.observe (Service.inputs lifter) (fun inputs ->
      register_lifter (Service.get inputs kind));
  Stream.observe (Service.inputs abi) (fun inputs ->
      register_abi (Service.get inputs))

;;
Config.manpage [
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
  `S "SEE ALSO";
  `P "$(b,bap-x86-cpu)(3), $(b,bap-plugin-abi)(1), $(b,bap-plugin-arm)(1)"
]
