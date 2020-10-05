open Bap_main
open Bap.Std
open Bap_core_theory
open KB.Syntax
module CT = Theory

include Bap_main.Loggers()

module Target = Bap_avr_target
module Dis = Disasm_expert.Basic

let provide_decoding () =
  KB.promise CT.Label.encoding @@ fun label ->
  CT.Label.target label >>| fun t ->
  if CT.Target.belongs Target.parent t
  then Target.llvm_avr16
  else CT.Language.unknown

let enable_llvm () =
  Dis.register Target.llvm_avr16 @@ fun _target ->
  Dis.create ~backend:"llvm" "avr"

let enable_loader () =
  let request_arch doc =
    let open Ogre.Syntax in
    match Ogre.eval (Ogre.request Image.Scheme.arch) doc with
    | Error _ -> assert false (* nothing could go wrong here! *)
    | Ok arch -> arch in
  KB.promise CT.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>| request_arch >>| function
  | Some "avr" -> Target.atmega328
  | _ -> CT.Target.unknown


let main _ctxt =
  enable_llvm ();
  enable_loader ();
  provide_decoding ();
  Avr_lifter.load ();
  Ok ()

(* semantic tags that describe what our plugin is providing,
   setting them is important not only for introspection but
   for the proper function of the cache subsystem.
*)
let provides = [
  "avr";
  "lifter";
]

(* finally, let's register our extension and call the main function  *)
let () = Bap_main.Extension.declare main
    ~provides
