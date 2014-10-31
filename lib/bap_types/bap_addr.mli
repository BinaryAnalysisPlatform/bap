(** Extends [Bitvector] module with extra functions.

    Fields of this structure will be added to an [Addr] module exposed
    in standard interface module [Std].

    This is a place, where we can add functions to a bitvector
    interface, that corresponds mostly to using bitvectors as a
    representation of an address.
*)

open Core_kernel.Std
open Bap_common


(** [memref ?disp ?index ?scale base] mimics a memory reference syntax
    in gas assembler,   [dis(base,index,scale)]
    assembler operation. It returns address at
    [base + index * scale + dis].

    @param [disp] stands for displacement and defaults to [0]
    @param [index] defaults for [0]
    @param [scale] defaults to [`r8]

    All operations are taken modulo {% $2^n$ %},
    where [n = bitwidth base].
*)
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
