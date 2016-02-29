open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

open Bap_disasm_block
open Bap_disasm_symtab
open Bap_disasm_source

type cfg = Bap_disasm_rec.Cfg.t

type t
type reconstructor = t

val create : (cfg -> symtab) -> t
val default : (word -> string) -> word list -> t
val of_blocks : (string * addr * addr) seq -> t
val run : t -> cfg -> symtab

module Factory : Factory with type t := t
