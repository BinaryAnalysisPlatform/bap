open Core_kernel.Std
open Ogre.Type
open Bap_llvm_ogre_types.Scheme

(** coff section in memory *)
let virtual_section_header () =
  Ogre.declare ~name:"virtual-section-header"
    (scheme name $ rel_addr $ size) Tuple.T3.create

(** coff section access flags *)
let section_flags () =
  Ogre.declare ~name:"section-flags"
    (scheme name $ readable $ writable $ executable)
    (fun name r w x -> name, (r,w,x))
