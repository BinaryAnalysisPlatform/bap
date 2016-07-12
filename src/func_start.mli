open Bap.Std
open Core_kernel.Std
open Bap_future.Std

(** [of_truth truth ~testbin] given the test binary [testbin], returns
    the function start address sequence from ground truth [truth] in
    a future *)
val of_truth: string -> testbin:string -> addr seq Or_error.t future

(** [of_tool tool] returns the function start address sequence from
    [tool] in a future *)
val of_tool: string -> testbin:string -> addr seq Or_error.t future
