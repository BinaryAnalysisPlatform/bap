(** Extends [Bitvector] module with extra functions.

    Fields of this structure will be added to an [Addr] module exposed
    in standard interface module [Std].

    This is a place, where we can add functions to a bitvector
    interface, that corresponds mostly to using bitvectors as a
    representation of an address.
*)

open Core_kernel
open Bap_common
val memref : ?disp:int -> ?index:int -> ?scale:size -> addr -> addr

(** Address arithmetics  *)
module type Arith = sig
  include Integer
  val create : addr -> t Or_error.t
end

(** Arithmetics on 32-bit addresses *)
module R32 : Arith with type t = int32

(** Arithmetics on 64-bit addresses  *)
module R64 : Arith with type t = int64
