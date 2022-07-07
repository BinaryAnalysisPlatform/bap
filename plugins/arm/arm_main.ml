let doc = "
# DESCRIPTION

The target support package that enables support for the ARM family of
architectures.
"

open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_main
open Extension.Syntax

include struct
  open Extension
  let interworking =
    Configuration.parameter Type.(some bool) "interworking"
      ~doc:"Enable ARM/Thumb interworking. Defaults to (auto),
            i.e., to the automatic detection of interworking"

  let backend =
    Configuration.parameter Type.(some string) "backend"
      ~doc:"Specify the backend that is used for disassembly and
            lifting."

  let features =
    Configuration.parameters Type.(list string) "features"
      ~doc:"Additional target features/attributes. The syntax
            and the feature names are backend-specific. For the LLVM
            backend the features are passed to the target attributes,
            see $(b,llvm-mc -mattr=help -triple <target>) for the list
            of features supported by your version of LLVM. To enable a
            feature just pass its name (you can optionally prepend
            $(b,+) to its name), to disable a feature prepend $(b,-)
            to its name."
end


type arms = [
  | Arch.arm
  | Arch.armeb
  | Arch.thumb
  | Arch.thumbeb
] [@@deriving enumerate]

let () = Bap_main.Extension.declare ~doc @@ fun ctxt ->
  let interworking = ctxt-->interworking in
  let backend = ctxt-->backend in
  let features = List.concat (ctxt-->features) in
  Arm_target.load ~features ?backend ?interworking ();
  Arm_gnueabi.setup ();
  List.iter all_of_arms ~f:(fun arch ->
      register_target (arch :> arch) (module ARM));
  Ok ()
