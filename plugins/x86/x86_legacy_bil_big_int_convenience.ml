(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(** Convenience functions for Big_ints *)

open Big_int_Z

let bi0 = big_int_of_int 0x0
let bi1 = big_int_of_int 0x1
let bi2 = big_int_of_int 0x2
let bi3 = big_int_of_int 0x3
let bi4 = big_int_of_int 0x4
let bi5 = big_int_of_int 0x5
let bi6 = big_int_of_int 0x6
let bi7 = big_int_of_int 0x7
let bi8 = big_int_of_int 0x8
let bi9 = big_int_of_int 0x9
let bia = big_int_of_int 0xa
let bib = big_int_of_int 0xb
let bic = big_int_of_int 0xc
let bid = big_int_of_int 0xd
let bie = big_int_of_int 0xe
let bif = big_int_of_int 0xf

let bim1 = big_int_of_int (-0x1)
let bim2 = big_int_of_int (-0x2)
let bim3 = big_int_of_int (-0x3)
let bim4 = big_int_of_int (-0x4)
let bim5 = big_int_of_int (-0x5)
let bim6 = big_int_of_int (-0x6)
let bim7 = big_int_of_int (-0x7)
let bim8 = big_int_of_int (-0x8)
let bim9 = big_int_of_int (-0x9)
let bima = big_int_of_int (-0xa)
let bimb = big_int_of_int (-0xb)
let bimc = big_int_of_int (-0xc)
let bimd = big_int_of_int (-0xd)
let bime = big_int_of_int (-0xe)
let bimf = big_int_of_int (-0xf)

let biconst i = big_int_of_int i
let bi = biconst
let biconst32 i = big_int_of_int32 i
let bi32 = big_int_of_int32
let biconst64 i = big_int_of_int64 i
let bi64 = big_int_of_int64
let bimask8 = bi 0xff
let bimask16 = bi 0xffff
let bimask32 = bi64 0xffffffffL
let bimask64 = Z.of_string "0xffffffffffffffff"
let bimask128 = Z.of_string "0xffffffffffffffffffffffffffffffff"

let big_int_of_bool = function
  | true -> bi1
  | false -> bi0

(** Infix operator to test if two big ints are equal. *)
let (==%) bi1 bi2 = eq_big_int bi1 bi2

(** Infix operator to test for non-equality *)
let (<>%) bi1 bi2 = not (bi1 ==% bi2)

(** Infix operator for < *)
let (<%) bi1 bi2 = lt_big_int bi1 bi2

(** Infix operator for <= *)
let (<=%) bi1 bi2 = le_big_int bi1 bi2

(** Infix operator for > *)
let (>%) bi1 bi2 = gt_big_int bi1 bi2

(** Infix operator for >= *)
let (>=%) bi1 bi2 = ge_big_int bi1 bi2

(** Infix operator for  *)
let (<<%) bi1 i2 = shift_left_big_int bi1 i2

(** Infix operator for >> *)
let (>>%) bi1 i2 = shift_right_big_int bi1 i2

(** Infix operator for $>> *)
let ($>>%) bi1 i2 = shift_right_towards_zero_big_int bi1 i2

(** Infix operator for + *)
let (+%) bi1 bi2 = add_big_int bi1 bi2

(** Infix operator for * *)
let ( *%) bi1 bi2 = mult_big_int bi1 bi2

(** Operator for incremeneting *)
let (++%) bi = succ_big_int bi

(** Infix operator for - *)
let (-%) bi1 bi2 = sub_big_int bi1 bi2

(** Infix operator for | *)
let (|%) bi1 bi2 = or_big_int bi1 bi2

(** Infix operator for & *)
let (&%) bi1 bi2 = and_big_int bi1 bi2

(* Infix operator for div (/) *)
let (/%) bi1 bi2 = div_big_int bi1 bi2

(** Infix operator for mod (%) *)
let (%%) bi1 bi2 = mod_big_int bi1 bi2

(** Operator for printing as string *)
let (~%) = string_of_big_int

(** bi_is_zero bi returns true iff bi = 0 *)
let bi_is_zero bi = bi0 ==% bi

(** bi_is_one bi returns true iff bi = 1 *)
let bi_is_one bi = bi1 ==% bi

(** bi_is_minusone bi returns true iff bi = -1 *)
let bi_is_minusone bi = bim1 ==% bi

(** For big_int addresses *)
let uintmax64 = big_int_of_string "0xffffffffffffffff"
let sintmax64 = big_int_of_int64 Int64.max_int

(* Conversion between int64 and big_int addresses that properly handles signedness
   (map the upper half of the address space to negative int64s) *)
let addr_to_int64 a =
  if a >% sintmax64
  then int64_of_big_int (a -% (uintmax64 +% bi1))
  else int64_of_big_int a

let addr_of_int64 a =
  if a < 0L
  then (big_int_of_int64 a) +% (uintmax64 +% bi1)
  else big_int_of_int64 a

(* declarations for piqi to convert between big_int and int64 *)
type address = Big_int_Z.big_int

let address_to_int64 : address -> int64 = addr_to_int64
let address_of_int64 : int64 -> address = addr_of_int64

(*
    XXX: We could make this more efficient by operating one int64 at a
    time, instead of just a nibble.
*)
let big_int_to_hex ?pad n =
  if n < Big_int_Z.zero_big_int then
    failwith "big_int_to_hex: Cannot convert infinite-width negative number to hex";
  let getn n = Big_int_Z.and_big_int n (big_int_of_int 0xf) in (* Get lsnibble *)
  let getrest n = Big_int_Z.shift_right_big_int n 4 in (* Get all but lsnibble *)
  let zeroextend s = match pad with
    | None -> s
    | Some(l) ->
      let p = l - String.length s in
      assert (p >= 0);
      (String.make p '0') ^ s
  in
  let (<=%) = le_big_int in
  let rec f = function
    | bi when bi <=% (big_int_of_int 0xf) -> Printf.sprintf "%x" (Big_int_Z.int_of_big_int bi)
    | n -> (f (getrest n)) ^ (f (getn n))
  in
  zeroextend (f n)
