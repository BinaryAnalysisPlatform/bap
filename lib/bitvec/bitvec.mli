(** abstract representation of a fixed size bitvector  *)
type t

(** a computation in some modulo *)
type 'a m

(** type denoting the arithmetic modulus *)
type modulus


(** [modulus s] is the modulus of bitvectors with size [s].

    This is a number $2^s-1$, also known as a Mersenne number.
*)
val modulus : int -> modulus


(** [m1 = modulus 1] = $1$ is the modulus of bitvectors with size [1]  *)
val m1  : modulus

(** [m8 = modulus 8] = $255$ is the modulus of bitvectors with size [8]  *)
val m8  : modulus

(** [m32 = modulus 32] = $2^32-1$ is the modulus of bitvectors with size [32]  *)
val m32 : modulus

(** [m64 = modulus 64] = $2^64-1$ is the modulus of bitvectors with size [64]  *)
val m64 : modulus


(** [(x <op> y) mod m] applies operation [<op>] modulo [m].

    Example: [(x + y) mod m] returns the sum of [x] and [y] modulo [m].

    Note: the [mod] function is declared as a primitive to enable
    support for inlining in non flambda versions of OCaml. Indeed,
    underneath the hood the ['a m] type is defined as the reader monad
    [modulus -> 'a], however we don't want to create a closure every
    time we compute an operation over bitvectors. With this trick, all
    versions of OCaml no matter the optimization options will inline
    [(x+y) mod m] and won't create any closures, even if [m] is not
    known at compile time.
*)
external (mod) : 'a m -> modulus -> 'a = "%apply"

module type S = sig
  (** an abstract representation of an operation modulo some number.*)
  type 'a m

  (** [bool x] returns [one] if [x] and [zero] otherwise.   *)
  val bool : bool -> t

  (** [int n mod m] is [n] modulo [m]. *)
  val int : int -> t m

  (** [int32 n mod m] is [n] modulo [m]. *)
  val int32 : int32 -> t m

  (** [int64 n mod m] is [n] modulo [m]. *)
  val int64 : int64 -> t m

  (** [bigint n mod m] is [n] modulo [m]. *)
  val bigint : Z.t -> t m

  (** [zero] is [0].  *)
  val zero : t

  (** [one] is [1].  *)
  val one : t

  (** [ones mod m] is a bitvector of size [m] with all bits set  *)
  val ones : t m

  (** [succ x mod m] is the successor of [x] modulo [m]  *)
  val succ : t -> t m


  (** [nsucc x n mod m] is the [n]th successor of [x] modulo [m]  *)
  val nsucc : t -> int -> t m


  (** [pred x mod m] is the predecessor of [x] modulo [m] *)
  val pred : t -> t m

  (** [npred x n mod m] is the [n]th predecessor of [x] modulo [m] *)
  val npred : t -> int -> t m

  (** [neg x mod m] is the 2-complement of [x] modulo [m]. *)
  val neg : t -> t m

  (** [lnot x] is the 1-complement of [x] modulo [m].  *)
  val lnot : t -> t m

  (** [abs x mod m] absolute value of [x] modulo [m].

      The absolute value of [x] is equal to [neg x] if
      [msb x] and to [x] otherwise. *)
  val abs  : t -> t m

  (** [add x y mod m] is [x + y] modulo [m] *)
  val add     : t -> t -> t m

  (** [sub x y mod m] is [x - y] modulo [m] *)
  val sub     : t -> t -> t m

  (** [mul x y mod m] is [x * y] modulo [m] *)
  val mul     : t -> t -> t m

  (** [div x y mod m] is [x / y] modulo [m],

      where [/] is the truncating towards zero division,
      that returns [ones m] if [y = 0].
  *)
  val div : t -> t -> t m


  (** [sdiv x y mod m] is signed division of [x] by [y] modulo [m],

      The signed division operator is defined in terms of the [div]
      operator as follows:
      {v
                        /
                        | div x y mod m : if not mx /\ not my
                        | neg (div (neg x) y) mod m if mx /\ not my
      x sdiv y mod m = <
                        | neg (div x (neg y)) mod m if not mx /\ my
                        | div (neg x) (neg y) mod m if mx /\ my
                        \

      where mx = msb x mod m,
        and my = msb y mod m.
      v}

  *)
  val sdiv : t -> t -> t m

  (** [rem x y mod m] is the remainder of [x / y] modulo [m]. *)
  val rem : t -> t -> t m

  (** [srem x y mod m] is the signed remainder [x / y] modulo [m].

      This version of the signed remainder where the sign follows the
      dividend, and is defined via the [rem] operation as follows

      {v
                        /
                        | rem x y mod m : if not mx /\ not my
                        | neg (rem (neg x) y) mod m if mx /\ not my
      x srem y mod m = <
                        | neg (rem x (neg y)) mod m if not mx /\ my
                        | neg (rem (neg x) (neg y)) mod m if mx /\ my
                        \

      where mx = msb x mod m,
        and my = msb y mod m.
      v}
  *)
  val srem : t -> t -> t m

  (** [smod x y mod m] is the signed remainder of [x / y] modulo [m].

      This version of the signed remainder where the sign follows the
      divisor, and is defined in terms of the [rem] operation as
      follows:

      {v
                        /
                        | u if u = 0
      x smod y mod m = <
                        | v if u <> 0
                        \

                        /
                        | u if not mx /\ not my
                        | add (neg u) y mod m if mx /\ not my
                   v = <
                        | add u x mod m if not mx /\ my
                        | neg u mod m if mx /\ my
                        \

      where mx = msb x mod m,
        and my = msb y mod m,
        and u = rem s t mod m.
      v}
  *)
  val smod : t -> t -> t m


  (** [nth x n mod m] is [true] if [n]th bit of [x] is [set].

      Returns [msb x mod m] if [n >= m]
          and [lsb x mod m] if [n < 0]
  *)
  val nth : t -> int -> bool m


  (** [msb x mod m] returns the most significand bit of [x].  *)
  val msb : t -> bool m


  (** [lsb x mod m] returns the least significand bit of [x].  *)
  val lsb : t -> bool m

  (** [logand x y mod m] is a bitwise logical and of [x] and [y] modulo [m]  *)
  val logand  : t -> t -> t m

  (** [logor x y mod m] is a bitwise logical or of [x] and [y] modulo [m].  *)
  val logor   : t -> t -> t m

  (** [logxor x y mod m] is exclusive [or] between [x] and [y] modulo [m] *)
  val logxor  : t -> t -> t m

  (** [lshift x y mod m] shifts [x] to left by [y].
      Returns [0] is [y >= m].
  *)
  val lshift  : t -> t -> t m

  (** [rshift x y mod m] shifts [x] right by [y] bits.
      Returns [0] if [y >= m]
  *)
  val rshift  : t -> t -> t m

  (** [arshift x y mod m] shifts [x] right by [y] with [msb x]
      filling.

      Returns [ones mod m] if [y >= m /\ msb x mod m]
          and [zero] if [y >= m /\ msb x mod m = 0]
  *)
  val arshift : t -> t -> t m


  (** [gcd x y mod m] returns the greatest common divisor modulo [m]

      [gcd x y] is the meet operation of the divisibility lattice,
      with [0] being the top of the lattice and [1] being the bottom,
      therefore [gcd x 0 = gcd x 0 = x].
  *)
  val gcd : t -> t -> t m


  (** [lcm x y mod] returns the least common multiplier modulo [m].

      [lcm x y] is the meet operation of the divisibility lattice,
      with [0] being the top of the lattice and [1] being the bottom,
      therefore [lcm x 0 = lcm 0 x = 0]

  *)
  val lcm : t -> t -> t m


  (** [(g,a,b) = gcdext x y mod m], where
      - [g = gcd x y mod m],
      - [g = (a * x + b * y) mod m].

      The operation is well defined if one or both operands are equal
      to [0], in particular:
      - [(x,1,0) = gcdext(x,0)],
      - [(x,0,1) = gcdext(0,x)].
  *)
  val gcdext : t -> t -> (t * t * t) m



  (** [!$x] is [of_string x]  *)
  val (!$) : string -> t

  (** [!!x mod m] is [int x mod m]  *)
  val (!!) : int -> t m


  (** [~-x mod m] is [neg x mod m]  *)
  val (~-) : t -> t m


  (** [~~x mod m] is [lnot x mod m] *)
  val (~~) : t -> t m

  (** [(x + y) mod m] is [add x y mod m] *)
  val ( + )  : t -> t -> t m

  (** [(x - y) mod m] is [sub x y mod m *)
  val ( - )  : t -> t -> t m

  (** [(x * y) mod m] is [mul x y mod m] *)
  val ( * )  : t -> t -> t m

  (** [(x / y) mod m] is [div x y mod m]  *)
  val ( / )  : t -> t -> t m

  (** [x /$ y mod m] is [sdiv x y mod m]  *)
  val ( /$ )  : t -> t -> t m

  (** [(x % y) mod m] is [rem x y mod m] *)
  val (%)  : t -> t -> t m

  (** [(x %$ y) mod m] is [smod x y mod m] *)
  val (%$) : t -> t -> t m

  (** [(x %^ y) mod m] is [srem x y mod m] *)
  val (%^) : t -> t -> t m

  (** [(x land y) mod m] is [logand x y mod m] *)
  val (land) : t -> t -> t m

  (** [(x lor y) mod m] is [logor x y mod m] *)
  val (lor)  : t -> t -> t m

  (** [(x lxor y) mod m] is [logxor x y mod m]  *)
  val (lxor) : t -> t -> t m

  (** [(x lsl y) mod m] [lshift x y mod m]  *)
  val (lsl)  : t -> t -> t m

  (** [(x lsr y) mod m] is [rshift x y mod m]  *)
  val (lsr)  : t -> t -> t m

  (** [(x asr y) = arshift x y]  *)
  val (asr)  : t -> t -> t m

  (** [(x ++ n) mod m] is [nsucc x n mod m] *)
  val (++) : t -> int -> t m


  (** [(x -- n) mod m]is [npred x n mod m] *)
  val (--) : t -> int -> t m
end




(** [compare x y] compares [x] and [y] as unsigned integers,
    i.e.,
    [compare x y] = [compare (to_nat x) (to_nat y)]
*)
val compare : t -> t -> int

(** [equal x y] is true if [x] and [y] represent the same integers *)
val equal : t -> t -> bool


val (<) : t -> t -> bool          (** [x < y] iff [compare x y = -1]  *)
val (>) : t -> t -> bool          (** [x > y] iff [compare x y =  1]  *)
val (=) : t -> t -> bool          (** [x = y] iff [compare x y =  0]  *)
val (<>) : t -> t -> bool          (** [x <> y] iff [compare x y <> 0]  *)
val (<=) : t -> t -> bool          (** [x <= y] iff [compare x y <= 0]  *)
val (>=) : t -> t -> bool          (** [x >= y] iff [compare x y >= 0]  *)

(** [hash x] returns such [z] that forall [y] s.t. [x=y], [hash y = z] *)
val hash : t -> int


(** [pp ppf x] is a pretty printer for the bitvectors.

    Could be used standalone or as an argument to the [%a] format
    specificator, e.g.,

    {[
      Format.fprintf "0xBEEF != %a" Bitvec.pp !$"0xBEAF"
    ]}

*)
val pp : Format.formatter -> t -> unit

(** [to_binary x] returns a canonical binary representation of [x] *)
val to_binary : t -> string

(** [of_binary s] returns a bitvector [x] s.t. [to_binary x = s].*)
val of_binary : string -> t


(** [to_string x] returns a textual (human readable) representation
    of the bitvector [x]. *)
val to_string : t -> string

(** [of_string s] returns a bitvector that corresponds to [s].

    The set of accepted strings is defined by the following EBNF grammar:

    {v
       valid-numbers ::=
        | "0b", bin-digit, {bin-digit}
        | "0o", oct-digit, {oct-digit}
        | "0x", hex-digit, {hex-digit}
        | dec-digit, {dec-digit}

      bin-digit ::= '0' | '1'
      oct-digit ::= '0'-'7'
      dec-digit ::= '0'-'9'
      hex-digit ::= '0'-'9' |'a'-'f'|'A'-'F'
    v}

    The function is not defined if [s] is not in [valid-numbers].
*)
val of_string : string -> t

(** [fits_int x] is [true] if [x] could be represented with the OCaml
    [int] type.

    Note: it is not always true that [fits_int (int x mod m)], since
    depending on [m] a negative number might not fit into the OCaml
    representation. For positive numbers it is true, however.
*)
val fits_int : t -> bool


(** [to_int x] returns an OCaml integer that has the same
    representation as [x].

    The function is undefined if [not (fits_int x)].
*)
val to_int : t -> int


(** [fits_int32 x] is [true] if [x] could be represented with the OCaml
    [int] type.

    Note: it is not always true that [fits_int32 (int32 x mod m)],
    since depending on [m] the negative [x] may not fit back into the
    [int32] representation. For positive numbers it is true, however.
*)
val fits_int32 : t -> bool

(** [to_int32 x] returns an OCaml integer that has the same
    representation as [x].

    The function is undefined if [not (fits_int32 x)].
*)
val to_int32 : t -> int32

(** [fits_int64 x] is [true] if [x] could be represented with the OCaml
    [int] type.

    Note: it is not always true that [fits_int64 (int64 x mod m)],
    since depending on [m] the negative [x] might not fit back into the
    [int64] representation. For positive numbers it is true, however.
*)
val fits_int64 : t -> bool

(** [to_int64 x] returns an OCaml integer that has the same
    representation as [x].

    The function is undefined if [not (fits_int64 x)].
*)
val to_int64 : t -> int64


(** [to_bigint x] returns a natural number that corresponds to [x].

    The returned value is always positive.
*)
val to_bigint : t -> Z.t


(** [extract ~hi ~lo x] extracts bits from [lo] to [hi].

    The operation is effectively equivalent to
    [(x lsr lo) mod (hi-lo+1)]
*)
val extract : hi:int -> lo:int -> t -> t


(** [select bits x] builds a bitvector from [bits] of [x].

    Returns a bitvector [y] such that [nth] bit of it is
    equal to [List.nth bits n] bit of [x].

    Returns [zero] if [bits] are empty.
*)
val select : int list -> t -> t

(** [append m n x y] takes [m] bits of [x] and [n] bits of [y]
    and returns their concatenation. The result has [m+n] bits.


    Examples:
    - [append 16 16 !$"0xdead" !$"0xbeef" = !$"0xdeadbeef"];
    - [append 12 20 !$"0xbadadd" !$"0xbadbeef" = !$"0xadddbeef"];;
*)
val append : int -> int -> t -> t -> t

(** [repeat m ~times:n x] repeats [m] bits of [x] [n] times.

    The result has [m*n] bits.
*)
val repeat : int -> times:int -> t -> t

(** [concat m xs] concatenates [m] bits of each [x] in [xs].

    The operation is the reduction of the [append] operation with [m=n].
    The result has [m * List.length xs] bits and is equal to [0]
    if [xs] is empty.
*)
val concat : int -> t list -> t

include S with type 'a m := 'a m

module type Modulus = sig
  val modulus : modulus
end


(** [module Mx = Make(Modulus)] produces a module [Mx]
    which implements all operation in [S] modulo
    [Modulus.modulus], so that all operations return a
    bitvector directly.
*)
module Make(M : Modulus) : sig
  include S with type 'a m = 'a
end


(** [M1] specializes [Make(struct let modulus = m1 end)]

    The specialization relies on a few arithmetic equalities
    and on an efficient implementation of the modulo operation
    as the [even x] aka [lsb x] operation.
*)
module M1  : S with type 'a m = 'a

(** [M8] specializes [Make(struct let modulus = m8 end)]

    This specialization relies on a fact, that 8 bitvectors
    always fit into OCaml integer representation, so it avoids
    calls to the underlying arbitrary precision arithmetic
    library.
*)
module M8  : S with type 'a m = 'a


(** [M32] specializes [Make(struct let modulus = m32 end)]

    This specialization relies on a fact, that 32 bitvectors
    always fit into OCaml integer representation, so it avoids
    calls to the underlying arbitrary precision arithmetic
    library.
*)
module M32 : S with type 'a m = 'a


(** [M64] specializes [Make(struct let modulus = m64 end)]

    This specialization tries to minimize calls to the arbitrary
    precision arithmetic library whenever, it is known that the result
    will not overflow the OCaml int representation.

*)
module M64 : S with type 'a m = 'a
