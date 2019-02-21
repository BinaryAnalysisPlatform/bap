open Bap_types.Std
open Bap_disasm_std
open Bap_sema.Std

(** restores functions with multiple entry points *)
val run : cfg -> symtab -> program term -> program term
