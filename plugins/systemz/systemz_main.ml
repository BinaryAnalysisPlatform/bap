open Bap_main
open Bap.Std
open Bap_core_theory
open KB.Syntax
module CT = Theory

include Bap_main.Loggers()

module Target = Bap_systemz_target
module Dis = Disasm_expert.Basic

(* to enable backward compatiblity with Arch.t
   we map all systemz targets to `systemz  *)
let map_arch () =
  KB.promise Arch.unit_slot @@ fun unit ->
  KB.collect Theory.Unit.target unit >>| fun t ->
  if Theory.Target.belongs Target.parent t
  then `systemz
  else `unknown

(* the same target may have different encodings
   or share encodings with other targets, in addition,
   the encoding may differ per each instruction and even
   be context-dependent, therefore target and encodings
   are separate properties of different classes. In our
   case, everything is trivial.
*)
let provide_decoding () =
  KB.promise CT.Label.encoding @@ fun label ->
  CT.Label.target label >>| fun t ->
  if CT.Target.belongs Target.parent t
  then Target.llvm_encoding
  else CT.Language.unknown


(* following the dependency injection principal, we have to provide
   the disassembler instance for our encoding.

   The _target parameter may further refine the target system, which
   could be reflected in the disassembler parameters. Not used in our
   case.*)
let enable_llvm () =
  Dis.register Target.llvm_encoding @@ fun _target ->
  Dis.create ~backend:"llvm" "systemz"

(* The file loader parses the file and provides its specification
   in the OGRE format. Our task is to analyze the specification and
   figure out if it is systemz (and if yes, then what version). For
   starters, we just assume that the loader will say "systemz" in
   the arch field.
*)
let enable_loader () =
  let request_arch doc =
    let open Ogre.Syntax in
    match Ogre.eval (Ogre.request Image.Scheme.arch) doc with
    | Error _ -> assert false (* nothing could go wrong here! *)
    | Ok arch -> arch in
  KB.promise CT.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>| request_arch >>| function
  | Some "systemz" -> Target.z9
  | _ -> CT.Target.unknown


(* the main function registers all our providers right now,
   we may later add some command line options whose values
   will be avaialble via the _ctxt parameter, which is now
   unused. *)
let main _ctxt =
  enable_llvm ();
  enable_loader ();
  provide_decoding ();
  map_arch ();
  Systemz_lifter.load ();
  Ok ()

(* semantic tags that describe what our plugin is providing,
   setting them is important not only for introspection but
   for the proper function of the cache subsystem.
*)
let provides = [
  "systemz";
  "lifter";
]

(* finally, let's register our extension and call the main function  *)
let () = Bap_main.Extension.declare main
    ~provides
