let doc = "
# DESCRIPTION

The target support package that enables support for the ARM family of
architectures.
"

open Core_kernel
open Bap.Std
open Bap_main
open Extension.Syntax

let interworking =
  let open Extension in
  Configuration.parameter Type.(some bool) "interworking"
    ~doc:"Enable ARM/Thumb interworking. Defaults to (auto),
          i.e., to the automatic detection of interworking"

let backend =
  let open Extension in
  Configuration.parameter Type.(some string) "backend"
    ~doc:"Specify the backend that is used for disassembly and
  lifting."

type arms = [
  | Arch.arm
  | Arch.armeb
] [@@deriving enumerate]

let () = Bap_main.Extension.declare ~doc @@ fun ctxt ->
  let interworking = ctxt-->interworking in
  let backend = ctxt-->backend in
  Arm_target.load ?backend ?interworking ();
  List.iter all_of_arms ~f:(fun arch ->
      register_target (arch :> arch) (module ARM);
      Arm_gnueabi.setup ());
  Ok ()
