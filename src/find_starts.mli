open Bap.Std
open Core_kernel.Std


(** [with_ida ~which_ida testbin] evaluates the IDA from [which_ida]
    on the binary [testbin], and returns the function start address
    sequence *)
val with_ida: which_ida:string -> string -> addr seq Or_error.t

(** [with_byteweight testbin] evaluates bap-byteweight on the binary
    [testbin], and returns the function start address sequence *)
val with_byteweight: string -> addr seq Or_error.t
