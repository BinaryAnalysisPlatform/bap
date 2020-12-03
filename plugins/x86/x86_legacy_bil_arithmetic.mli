(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)

module Bil = X86_legacy_bil
open Bil

exception ArithmeticEx of string
(*val memoize : ?size:int -> ('a -> 'b) -> 'a -> 'b *)
val power_of_two : Type.bv_size -> Z.t
val bitmask : Type.bv_size -> Z.t
val bits_of_width : Type.typ -> Type.bv_size
val to_big_int : Z.t * Type.typ -> Z.t
val to_sbig_int : Z.t * Type.typ -> Z.t
val binop :
  Type.binop_type -> Z.t * Type.typ -> Z.t * Type.typ -> Z.t * Type.typ
val unop : Type.unop_type -> Z.t * Type.typ -> Z.t * Type.typ
val cast : Type.cast_type -> Z.t * Type.typ -> Type.typ -> Z.t * Type.typ
val extract : Z.t -> Z.t -> Z.t * Type.typ -> Z.t * Type.typ
val concat : Z.t * Type.typ -> Z.t * Type.typ -> Z.t * Type.typ
val is_zero : Z.t * Type.typ -> bool
val bytes_to_int64 : [< `Big | `Little ] -> int64 list -> int64
