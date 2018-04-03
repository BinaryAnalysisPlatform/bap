open Bap_types.Std
open Bap_image_std
open Bap_sema.Std
open Bap_disasm_std

(** [run prg insn spec] performs search for relocations in [spec] and
    add inject synthetic subroutines for each external function. Also
    fixups calls to externals in [prg] *)
val run : program term -> (mem * insn) seq -> Ogre.doc -> program term
