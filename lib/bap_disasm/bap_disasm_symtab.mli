open Bap_types.Std
open Image_internal_std
open Bap_disasm

type t = string table

(** [create roots base cfg]
    creates a symbol table from [cfg]. If no roots are
    provided, then only calls
*)
val create : addr list -> mem -> block table -> t
