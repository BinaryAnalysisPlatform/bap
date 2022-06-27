open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Poly
open Bap.Std
open X86_targets
include Self()

type kind = Legacy | Modern | Merge [@@deriving equal]

let main backend kind abi =
  let abi = Option.map ~f:(Theory.Abi.read ~package:"bap") abi in
  X86_target.load ?abi ?backend ();
  let kind = if backend = Some "ghidra" then Modern else kind in
  let ia32, amd64 = match kind with
    | Legacy -> (module IA32L : Target), (module AMD64L : Target)
    | Modern -> (module IA32 : Target), (module AMD64 : Target)
    | Merge -> (module IA32M : Target), (module AMD64M : Target) in
  register_target `x86 ia32;
  register_target `x86_64 amd64

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
  let abi =
    let doc = "Override the ABI detection and use the selected ABI." in
    Config.(param (some string) "abi" ~synonyms:["64-abi"] ~doc) in
  let fp_lifter = Config.flag "with-floating-points"
      ~doc:"DEPRECATED" in
  let legacy_lifter = Config.flag "with-legacy-floating-points"
      ~doc:"Enables the legacy floating-point lifter and \
            disables the intrinsic semantics of floating-point \
            operations" in
  let disable_intrinsics =
    Config.flag "disable-floating-point-intrinsics"
      ~doc:"Disables translation of floating-point instructions \
            into calls to intrinsic functions." in
  let backend = Config.param Config.(some string) "backend"
      ~synonyms:["64-backend"] in
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
      let open Bap_core_theory in
      let open Bap_primus.Std in

      main !!backend !!kind !!abi;
      if !!fp_lifter || !!legacy_lifter then begin
        let open Bap_core_theory in
        let open Bap_primus.Std in
        if !!fp_lifter then Format.eprintf
            "The --with-floating-points flag is deprecated \
             and will be removed in the following versions of bap. \
             This flag enables the legacy lifter that is incomplete \
             and disables the new version that translates \
             floating-point operations into the intrinsic calls. \
             If you still need the legacy lifter use the \
             --x86-with-legacy-floating-points option instead.@\n%!";
        X86_legacy_bil_lifter.init ();
        X86_legacy_bil_semantics.init ();
      end else if not !!disable_intrinsics
      then
        KB.promise Primus.Lisp.Semantics.context @@ fun _ ->
        KB.return @@
        Primus.Lisp.Context.create [
          "x86-floating-points", ["intrinsic-semantics"]
        ])
