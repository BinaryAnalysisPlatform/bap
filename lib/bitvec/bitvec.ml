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

type modulus = {m : Z.t} [@@unboxed]


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
let norm {m} x =
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
}

let m1 = modulus 1
let m8 = modulus 8
let m32 = modulus 32
let m64 = modulus 64


let compare x y =
  if x == y then 0 else Z.compare x y
[@@inline]

let hash x = Z.hash x [@@inline]

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
let msb x m = Z.(equal m.m (norm m x lor (m.m asr 1))) [@@inline]
let lsb x _ = Z.is_odd x [@@inline]
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

let shift n {m} ~overshift ~in_bounds =
  let w = Z.numbits m in
  if Z.(lt n (of_int w))
  then in_bounds w (to_int_fast n w)
  else overshift
[@@inline]

let lshift x n m = shift n m
    ~overshift:zero
    ~in_bounds:(fun _ n -> norm m @@ Z.shift_left x n)
[@@inline]

let rshift x n m = shift n m
    ~overshift:zero
    ~in_bounds:(fun _ n -> norm m @@ Z.shift_right x n)
[@@inline]

let arshift x n m =
  let msb = msb x m in
  shift n m
    ~in_bounds:(fun w n ->
        let x = Z.(x asr n) in
        norm m @@ if msb
        then
          let n = w - n in
          let y = ones mod m in
          Z.(y lsl n lor x)
        else x)
    ~overshift:(if msb then ones m else zero)
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

module Syntax = struct
  let (!!) x m = int x m [@@inline]
  let (~-) x m = neg x m [@@inline]
  let (~~) x m = lnot x m [@@inline]
  let (+) x y m = add x y m [@@inline]
  let (-) x y m = sub x y m [@@inline]
  let ( * ) x y m = mul x y m [@@inline]
  let (/) x y m = div x y m [@@inline]
  let (/$) x y m = sdiv x y m [@@inline]
  let (%) x y m = rem x y m [@@inline]
  let (%$) x y m = smod x y m [@@inline]
  let (%^) x y m = srem x y m [@@inline]
  let (land) x y m = logand x y m [@@inline]
  let (lor) x y m = logor x y m [@@inline]
  let (lxor) x y m = logxor x y m [@@inline]
  let (lsl) x y m = lshift x y m [@@inline]
  let (lsr) x y m = rshift x y m [@@inline]
  let (asr) x y m = arshift x y m [@@inline]
  let (++) x n m = nsucc x n m [@@inline]
  let (--) x n m = npred x n m [@@inline]
end

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

let to_string = Z.format "%#x"

let nonnegative r =
  if Z.sign r < 0
  then invalid_arg
      "invalid string representation, sign is not expected";
  r
[@@inline]

let defaults_to_length ?len x = match len with
  | None -> String.length x
  | Some n -> n


let of_string x = nonnegative (Z.of_string x) [@@inline]
let of_string_base b x = nonnegative (Z.of_string_base b x) [@@inline]
let of_substring ?(pos=0) ?len x =
  nonnegative @@
  Z.of_substring x ~pos ~len:(defaults_to_length ?len x)
let of_substring_base ?(pos=0) ?len b x =
  nonnegative @@
  Z.of_substring_base b x ~pos ~len:(defaults_to_length ?len x)

let (!$) x = of_string x [@@inline]
let (!!) x m = int x m [@@inline]

(* all bitvectors are normalized and therefore non-negative,
   so we don't need to check the lower bound.
*)
let max_uint = Z.(one lsl Sys.int_size - one)
let max_sint = Z.(of_int max_int)
let max_uint32 = Z.(one lsl 32 - one)
let max_sint32 = Z.(one lsl 31 - one)
let max_uint64 = Z.(one lsl 64 - one)
let max_sint64 = Z.(one lsl 63 - one)


let fits_int = Z.fits_int
let fits_int32 = Z.fits_int32
let fits_int64 = Z.fits_int64

let doesn't_fit r x =
  failwith (to_string x ^ " doesn't fit the " ^ r ^ " type")

let convert tname size convert max_signed max_unsigned x =
  if Z.(x <= max_signed) then convert x
  else if Z.(x <= max_unsigned)
  then convert (Z.signed_extract x 0 size)
  else doesn't_fit tname x

let to_int x =
  convert "int" Sys.int_size Z.to_int max_sint max_uint x
[@@inline]

let to_int32 x =
  convert "int32" 32 Z.to_int32 max_sint32 max_uint32 x
[@@inline]

let to_int64 x =
  convert "int" 64 Z.to_int64 max_sint64 max_uint64 x
[@@inline]

let to_bigint x = x [@@inline]

let of_binary = Z.of_bits
let to_binary = Z.to_bits
let pp ppf x =
  Format.fprintf ppf "%s" (to_string x)

module Make(M : Modulus) : S with type 'a m = 'a = struct
  type 'a m = 'a
  let m = M.modulus

  let bool x = bool x [@@inline]
  let int x = int x mod m [@@inline]
  let int32 x = int32 x mod m [@@inline]
  let int64 x = int64 x mod m [@@inline]
  let bigint x = bigint x mod m [@@inline]
  let zero = zero
  let one = one
  let ones = ones mod m
  let succ x = succ x mod m [@@inline]
  let nsucc x n = nsucc x n mod m [@@inline]
  let pred x = pred x mod m [@@inline]
  let npred x n = npred x n mod m [@@inline]
  let neg x = neg x mod m [@@inline]
  let lnot x = lnot x mod m [@@inline]
  let abs x = abs x mod m [@@inline]
  let add x y = add x y mod m [@@inline]
  let sub x y = sub x y mod m [@@inline]
  let mul x y = mul x y mod m [@@inline]
  let div x y = div x y mod m [@@inline]
  let sdiv x y = sdiv x y mod m [@@inline]
  let rem x y = rem x y mod m [@@inline]
  let srem x y = srem x y mod m [@@inline]
  let smod x y = smod x y mod m [@@inline]
  let nth x y = nth x y mod m [@@inline]
  let msb x = msb x mod m [@@inline]
  let lsb x = lsb x mod m [@@inline]
  let logand x y = logand x y mod m [@@inline]
  let logor  x y = logor  x y mod m [@@inline]
  let logxor x y = logxor x y mod m [@@inline]
  let lshift x y = lshift x y mod m [@@inline]
  let rshift x y = rshift x y mod m [@@inline]
  let arshift x y = arshift x y mod m [@@inline]
  let gcd x y = gcd x y mod m [@@inline]
  let lcm x y = lcm x y mod m [@@inline]
  let gcdext x y = gcdext x y mod m [@@inline]

  let (!$) x = of_string x [@@inline]
  let (!!) x = int x [@@inline]
  let (~-) x = neg x [@@inline]
  let (~~) x = lnot x [@@inline]
  let (+) x y = add x y [@@inline]
  let (-) x y = sub x y [@@inline]
  let ( * ) x y = mul x y [@@inline]
  let (/) x y = div x y [@@inline]
  let (/$) x y = sdiv x y [@@inline]
  let (%) x y = rem x y [@@inline]
  let (%$) x y = smod x y [@@inline]
  let (%^) x y = srem x y [@@inline]
  let (land) x y = logand x y [@@inline]
  let (lor) x y = logor x y [@@inline]
  let (lxor) x y = logxor x y [@@inline]
  let (lsl) x y = lshift x y [@@inline]
  let (lsr) x y = rshift x y [@@inline]
  let (asr) x y = arshift x y [@@inline]
  let (++) x n = nsucc x n [@@inline]
  let (--) x n = npred x n [@@inline]
end [@@inline]

module M1 = Make(struct
    let modulus = m1
  end)

module M8 = Make(struct
    let modulus = m8
  end)

module M32 = Make(struct
    let modulus = m32
  end)

module M64 = Make(struct
    let modulus = m64
  end)

include Syntax
let equal x y = compare x y = 0 [@@inline]
let (<) x y = compare x y < 0 [@@inline]
let (>) x y = compare x y > 0 [@@inline]
let (<=) x y = compare x y <= 0 [@@inline]
let (>=) x y = compare x y >= 0 [@@inline]
let (=) x y = compare x y = 0 [@@inline]
let (<>) x y = compare x y <> 0 [@@inline]
