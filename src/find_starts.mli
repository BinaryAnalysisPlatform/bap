open Bap.Std
open Core_kernel.Std
open Bap_future.Std


(** [with_ida testbin] evaluates the IDA on the binary [testbin], and
    returns the function start address sequence in a future *)
val with_ida: string -> addr seq Or_error.t future

(** [with_byteweight testbin] evaluates bap-byteweight on the binary
    [testbin], and returns the function start address sequence in a future *)
val with_byteweight: string -> addr seq Or_error.t future
