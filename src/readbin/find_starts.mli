open Bap.Std
open Core_kernel.Std


(** [with_ida ~which_ida testbin] evaluates the IDA from [which_ida]
    on the binary [testbin], and returns the function start address
    set *)
val with_ida: which_ida:string -> string -> Addr.Set.t Or_error.t

(** [with_byteweight testbin] evaluates bap-byteweight on the binary
    [testbin], and returns the function start address set *)
val with_byteweight: string -> Addr.Set.t Or_error.t
