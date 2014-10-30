open Core_kernel.Std
open Bap_common


let memref ?(disp=0) ?(index=0) ?(scale=`r8)  addr =
  let n = Bap_size.to_bytes scale in
  Bap_bitvector.(addr ++ (n * index + disp))

module type Arith = sig
  include Integer
  val create : addr -> t Or_error.t
end

module type Core_int = sig
  include Int_intf.S
  val to_bv: t -> Bitvector.t
  val of_bv: Bitvector.t -> t Or_error.t
end


module Make(Int : Core_int) = struct
  open Or_error

  (** [create_modulo_width x] takes a number represented as a
      [n]-bits length bitvector, casts it to an int modulo [n].

      Since [Int.t] should be either [int32] or [int64] we can be sure
      that number of bits is less then maximum number represantable by
      OCaml's [int] type.
  *)
  let to_int_modulo_width (x : Int.t) : int =
    let width = Int.num_bits in
    let x = Int.to_bv x in
    let w = Bitvector.of_int width ~width in
    match Bitvector.(Int.(!$x mod !$w) >>= to_int) with
    | Error _ -> assert false
    | Ok x -> x

  module Base = struct
    include Int
    let make f a off = f a (to_int_modulo_width off)
    let lshift  = make shift_left
    let rshift  = make shift_right_logical
    let arshift = make shift_right
    let modulo  = (%)
    let lnot    = bit_not
    let logand  = bit_and
    let logor   = bit_or
    let logxor  = bit_xor
    let div = ( / )
    let mul = ( * )
    let sub = ( - )
    let add = ( + )
  end

  let create = Int.of_bv
  include Integer.Make(Base)
end

module I32 = struct
  let of_bv = Bitvector.to_int32
  let to_bv = Bitvector.of_int32
  include Int32
end

module I64 = struct
  let of_bv = Bitvector.to_int64
  let to_bv = Bitvector.of_int64
  include Int64
end


module R32 = Make(I32)
module R64 = Make(I64)
