type t = Z.t

(* Invariant: a bitvector is always normed.

   A normed bitvector is always treated by Z as non-negative:
   - Z.sign (norm w x) >= 0

   We have to enforce this invariant since in Z negative numbers are
   not equal to their two complement forms, in other words,
   Z.(-1 != 0xFFFFF..FF).

   This makes sense, since in Z a number has an arbitrary length and
   thus the number of ones in the two complement form is essentially
   unlimited, thus any fixed length bitvector of all ones is not equal
   to any negative number. Thus in order to implement canonical
   comparison function we need to parametrize it with the length of
   the length of the bitvector, which in fact prevents us from having
   one single type for bivector sets, maps, and any regular or
   comparable interface in general. Another problem with
   non-normalized bitvectors is serialization. The [to_bits] function
   doesn't preserve the sign, the default marshaling function is not
   portable between OCaml runtimes with different word sizes, and last
   but not least, the same problem with non-canonical representation
   perists = 0xFFFF_FFFF is not equal to -1, or even more, 0xDEADBEEF
   is not equal to -559038737.

   The are two ramifications of enforcing the normalized canonical
   representation:

   - some extra performance cost due to occasional calls to normalize
   (it is usually just one instruction - logand)

   - values with more than 62 significand bits will be stored in a
     boxed notation (we loose 1 bit wrt to the non-normalized)
     representation.

*)

type modulus = {
  w : int;                      (* bitwidth *)
  m : Z.t;                      (* modulus: 2^w-1 *)
}


type 'a m = modulus -> 'a


(* we predicate the normalization, to prevent extra allocations
   in cases when the bitvector has the boxed representation, so
   that in cases when `x = norm x` we do not create a fresh new
   value with the same contents.

   The normalization is required for negative numbers, since Z
   represent them non canonically.

   Specialized versions may use a different normalization procedure,
   this is the general case that shall work with all range of widths.
*)
let norm {m; w} x =
  if Z.sign x < 0 || Z.geq x m
  then Z.(x land m)
  else x
[@@inline]


(* defining extrenal as the apply primitive will enable
   inlining (with any other operator definition the vanilla
   version of OCaml will create a closure)
*)
external (mod) : 'a m -> modulus -> 'a = "%apply"

let modulus w = {
  m = Z.(one lsl w - one);
  w;
}

let m1 = modulus 1
let m8 = modulus 8
let m32 = modulus 32
let m64 = modulus 64


let compare = Z.compare
let hash = Z.hash

let one = Z.one
let zero = Z.zero
let ones m = norm m @@ Z.minus_one
let bool x = if x then one else zero
let int x m = norm m @@ Z.of_int x [@@inline]
let int32 x m = norm m @@ Z.of_int32 x [@@inline]
let int64 x m = norm m @@ Z.of_int64 x [@@inline]
let bigint x m  = norm m @@ x [@@inline]

let append w1 w2 x y =
  let w = w1 + w2 in
  let ymask = Z.(one lsl w2 - one) in
  let zmask = Z.(one lsl w - one) in
  let ypart = Z.(y land ymask) in
  let xpart = Z.(x lsl w2) in
  Z.(xpart lor ypart land zmask)
[@@inline]

let extract ~hi ~lo x =
  Z.extract x lo (hi - lo + 1)
[@@inline]

let setbit x n = Z.(x lor (one lsl n)) [@@inline]

let select bits x =
  let rec loop n y = function
    | [] -> y
    | v :: vs ->
      let y = if Z.testbit x v
        then setbit y n
        else y in
      loop (n+1) y vs in
  loop 0 zero bits

let repeat m ~times:n x =
  let mask = Z.(one lsl m - one) in
  let x = Z.(x land mask) in
  let rec loop i y =
    if i < n
    then
      let off = i * m in
      let stamp = Z.(x lsl off) in
      loop (i+1) Z.(y lor stamp)
    else y in
  loop 0 zero

let concat m xs =
  let mask = Z.(one lsl m - one) in
  List.fold_left (fun y x ->
      let x = Z.(x land mask) in
      Z.(y lsl m lor x)) zero xs

(* we can't use higher functions such as op1 and op2,
   to lift Z operations to bitv since this will prevent
   inlining and will introduce extra indirect calls, so
   we have to make it in a verbose method. Flambda, where
   are you?
*)

let succ x m = norm m @@ Z.succ x [@@inline]
let pred x m = norm m @@ Z.pred x [@@inline]
let nsucc x n m = norm m @@ Z.(x + of_int n) [@@inline]
let npred x n m = norm m @@ Z.(x - of_int n) [@@inline]
let lnot x m = norm m @@ Z.lognot x [@@inline]
let neg x m = norm m @@ Z.neg x [@@inline]
let nth x n _ = Z.testbit x n [@@inline]
let msb x {w} = Z.testbit x (w - 1) [@@inline]
let lsb x _ = nth x 0 [@@inline]
let abs x m = if msb x m then neg x m else x [@@inline]
let add x y m = norm m @@ Z.add x y [@@inline]
let sub x y m = norm m @@ Z.sub x y [@@inline]
let mul x y m = norm m @@ Z.mul x y [@@inline]
let div x y m = if Z.(y = zero)
  then ones m
  else norm m @@ Z.div x y
[@@inline]

let sdiv x y m = match msb x m, msb y m with
  | false, false -> div x y m
  | true, false -> neg (div (neg x m) y m) m
  | false, true -> neg (div x (neg y m) m) m
  | true, true  -> div (neg x m) (neg y m) m
[@@inline]

let rem x y m =
  if Z.(y = zero)
  then x
  else norm m @@ Z.rem x y
[@@inline]

(* 2's complement signed remainder (sign follows dividend) *)
let srem x y m = match msb x m, msb y m with
  | false,false -> rem x y m
  | true,false -> neg (rem (neg x m) y m) m
  | false,true -> neg (rem x (neg y m) m) m
  | true,true -> neg (rem (neg x m) (neg y m) m) m
[@@inline]

(* 2's complement signed remained (sign follows the divisor) *)
let smod s t m =
  let u = rem s t m in
  if Z.(u = zero) then u
  else match msb s m, msb t m with
    | false,false -> u
    | true,false -> add (neg u m) t m
    | false,true -> add u t m
    | true,true -> neg u m
[@@inline]

let logand x y m = norm m @@ Z.logand x y [@@inline]
let logor x y m = norm m @@ Z.logor x y [@@inline]
let logxor x y m = norm m @@ Z.logxor x y [@@inline]


(* extracts no more than [m] bits from [x].
   The [Z.to_int] function may allocate since it throws
   an exception in case of the overflow. So we have to
   pay with the GC check after each call to [Z.to_int],
   so instead of no-op we have a call, and a couple of
   blocks with a dozen of instructions.

   The implementation below is safe and doens't rely on
   whether the FAST_PATH or NATINT options are enabled
   in zarith. It first looks whether [x] is actually an
   [int] and then casts it to the [int] type, otherwise
   it extracts the low (min m Sys.int_size) bits and
   cast them to_int using the slow Z.to_int, so that in
   case if zarith is not using NATINT representation,
   we will still be on the safe side.

   Note: this function is pure optimization, so in case
   if you're not sure, it could be replaced with its
   else branch.
*)
let to_int_fast x m : int =
  if Obj.is_int (Obj.repr x) then Obj.magic x
  else Z.to_int @@ Z.signed_extract x 0 (min m Sys.int_size)
[@@inline]

let lshift x y m =
  if Z.(geq y (of_int m.w))
  then zero
  else norm m @@ Z.shift_left x (to_int_fast y m.w)
[@@inline]

let rshift x y m =
  if Z.(geq y (of_int m.w))
  then zero
  else norm m @@ Z.shift_right x (to_int_fast y m.w)
[@@inline]

let gcd x y m =
  if Z.(equal x zero) then y else
  if Z.(equal y zero) then x else
    norm m @@ Z.gcd x y
[@@inline]

let lcm x y m =
  if Z.(equal x zero) || Z.(equal y zero) then zero
  else norm m @@ Z.lcm x y
[@@inline]

let gcdext x y m =
  if Z.(equal x zero) then (y,zero,one) else
  if Z.(equal y zero) then (x,one,zero) else
    let (g,a,b) = Z.gcdext x y in
    (norm m g, norm m a, norm m b)
[@@inline]

let signed_compare x y m = match msb x m, msb y m with
  | true, true -> compare y x
  | false,false -> compare x y
  | true,false -> -1
  | false,true -> 1
[@@inline]

module type S = sig
  type 'a m
  val bool : bool -> t


  val int : int -> t m
  val int32 : int32 -> t m
  val int64 : int64 -> t m
  val bigint : Z.t -> t m
  val zero : t
  val one : t
  val ones : t m
  val succ : t -> t m
  val nsucc : t -> int -> t m
  val pred : t -> t m
  val npred : t -> int -> t m
  val neg : t -> t m
  val lnot : t -> t m
  val abs  : t -> t m
  val add     : t -> t -> t m
  val sub     : t -> t -> t m
  val mul     : t -> t -> t m
  val div : t -> t -> t m
  val sdiv : t -> t -> t m
  val rem : t -> t -> t m
  val srem : t -> t -> t m
  val smod : t -> t -> t m
  val nth : t -> int -> bool m
  val msb : t -> bool m
  val lsb : t -> bool m
  val logand  : t -> t -> t m
  val logor   : t -> t -> t m
  val logxor  : t -> t -> t m
  val lshift  : t -> t -> t m
  val rshift  : t -> t -> t m
  val arshift : t -> t -> t m
  val gcd    : t -> t -> t m
  val lcm    : t -> t -> t m
  val gcdext : t -> t -> (t * t * t) m

  val (!$) : string -> t
  val (!!) : int -> t m
  val (~-) : t -> t m
  val (~~) : t -> t m
  val ( + )  : t -> t -> t m
  val ( - )  : t -> t -> t m
  val ( * )  : t -> t -> t m
  val ( / )  : t -> t -> t m
  val ( /$ )  : t -> t -> t m
  val (%)  : t -> t -> t m
  val (%$) : t -> t -> t m
  val (%^) : t -> t -> t m
  val (land) : t -> t -> t m
  val (lor)  : t -> t -> t m
  val (lxor) : t -> t -> t m
  val (lsl)  : t -> t -> t m
  val (lsr)  : t -> t -> t m
  val (asr)  : t -> t -> t m
  val (++) : t -> int -> t m
  val (--) : t -> int -> t m
end

module type Modulus = sig
  val modulus : modulus
end

module Make(W : Modulus) = struct
  include W


end [@@inline]

let to_string = Z.format "%#x"
let of_string x =
  let r = Z.of_string x in
  if Z.sign r < 0
  then invalid_arg
      (x ^ " - invalid string representation, sign is not expected")
  else r

let (!$) x = of_string x [@@inline]
let (!!) x m = int x m [@@inline]

let fits_int = Z.fits_int
let fits_int32 = Z.fits_int32
let fits_int64 = Z.fits_int64

let doesn't_fit r x =
  failwith (to_string x ^ " doesn't the " ^ r ^ " type")

let to_int x = if fits_int x then Z.to_int x
  else doesn't_fit "int" x

let to_int32 x = if fits_int32 x then Z.to_int32 x
  else doesn't_fit "int32" x

let to_int64 x = if fits_int64 x then Z.to_int64 x
  else doesn't_fit "int64" x

let of_binary = Z.of_bits
let to_binary = Z.to_bits
let pp ppf x =
  Format.fprintf ppf "%s" (to_string x)
