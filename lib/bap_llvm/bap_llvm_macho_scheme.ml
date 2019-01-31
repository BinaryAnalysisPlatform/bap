open Core_kernel
open Ogre.Type
open Bap_llvm_ogre_types.Scheme

(** macho segment command *)
let segment_cmd () =
  Ogre.declare ~name:"segment-command" (scheme name $ off $ size)
    Tuple.T3.create

let segment_cmd_flags () =
  Ogre.declare ~name:"segment-command-flags"
    (scheme name $ readable $ writable $ executable)
    (fun name r w x -> name, (r,w,x))

let virtual_segment_cmd () =
  Ogre.declare ~name:"virtual-segment-command"
    (scheme name $ rel_addr $ size) Tuple.T3.create

(** macho symbol that doesn't belong to any section *)
let macho_symbol () =
  Ogre.declare ~name:"macho-symbol" (scheme name $ value)
    Tuple.T2.create
