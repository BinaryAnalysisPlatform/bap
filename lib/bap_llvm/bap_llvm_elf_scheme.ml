open Core_kernel
open Ogre.Type

open Bap_llvm_ogre_types.Scheme

(** loadable entry *)
let ld = "ld" %: bool

let declare name scheme f = Ogre.declare ~name scheme f

(** elf program header as it is in file *)
let program_header () = declare "program-header"
    (scheme name $ off $ size) Tuple.T3.create

(** elf program header as it is in memory *)
let virtual_program_header () = declare "virtual-program-header"
    (scheme name $ rel_addr $ size) Tuple.T3.create

(** elf program header flags *)
let program_header_flags () = declare "program-header-flags"
    (scheme name $ ld $ readable $ writable $ executable)
    (fun name ld r w x -> name,ld,r,w,x)

(** elf section header flags *)
let section_flags () = declare "section-flags"
    (scheme name $ readable $ writable $ executable) @@ fun s r w x ->
  s,r,w,x
