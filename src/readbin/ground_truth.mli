open Bap.Std
open Core_kernel.Std

(** [from_unstripped_bin bin] returns the function start address sequence
    from the symbols in unstripped binary [bin] *)
val from_unstripped_bin : string -> addr seq Or_error.t

(** [from_symbol_file symbolfile ~testbin] gets the architecture and
    memory information from [testbin], and returns the function start
    address sequence from the symbols in [symbolfile] *)
val from_symbol_file : string -> testbin:string -> addr seq Or_error.t
