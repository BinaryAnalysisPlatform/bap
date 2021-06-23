open Core_kernel
open Bap.Std
open X86_targets
include Self()

type kind = Legacy | Modern | Merge [@@deriving equal]

let main backend kind x32 x64 =
  X86_target.load ~backend ();
  let kind = if String.(backend <> "llvm") then Modern else kind in
  let ia32, amd64 = match kind with
    | Legacy -> (module IA32L : Target), (module AMD64L : Target)
    | Modern -> (module IA32 : Target), (module AMD64 : Target)
    | Merge -> (module IA32M : Target), (module AMD64M : Target) in
  register_target `x86 ia32;
  register_target `x86_64 amd64;
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
    plugin is installed it will automatically provide it
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
    ] in
  let abi arch name =
    let abis = X86_abi.supported () |> List.filter_map ~f:(fun abi ->
        if Arch.equal (X86_abi.arch abi :> arch) arch
        then Some (X86_abi.name abi, abi)
        else None) in
    let doc = sprintf "Use specified $(docv) as a default one. The $(docv)
    must be %s" @@ Config.doc_enum abis in
    Config.(param (some (enum abis)) name ~doc) in
  let x32 = abi `x86 "abi" in
  let x64 = abi `x86_64 "64-abi" in
  let fp_lifter = Config.flag "with-floating-points" in
  let backend = Config.param Config.string "backend" in
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
               Option.some_if (equal_kind kind default) name)) in
    Config.(param (enum kinds) "lifter" ~doc ~default) in
  Config.when_ready (fun {Config.get=(!!)} ->
      main !!backend !!kind !!x32 !!x64;
      if !!fp_lifter then begin
        X86_legacy_bil_lifter.init ();
        X86_legacy_bil_semantics.init ();
      end)
