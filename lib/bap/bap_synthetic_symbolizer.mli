open Bap_disasm_std
open Bap_image_std
open Bap_sema.Std
open Bap_types.Std

val resolve : Ogre.Doc.t -> (mem * insn) seq -> program term -> program term
