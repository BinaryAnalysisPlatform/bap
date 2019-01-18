open Format


(** type of integer  *)
type t

val compare : t -> t -> int


val bitwidth : t -> int

val to_bits : t -> string
val of_string : string -> t
val of_bool  : bool -> t
val of_int   : width:int -> int -> t
val of_int32 : ?width:int -> int32 -> t
val of_int64 : ?width:int -> int64 -> t
val b0 : t
val b1 : t
val one: int -> t
val zero: int -> t
val ones : int -> t

(** [succ n] successor of [n]  *)
val succ : t -> t

(** [pred n] is a predecessor of [n] *)
val pred : t -> t

(** [abs x] absolute value of [x] *)
val abs  : t -> t

(** [neg x] = [-x] *)
val neg  : t -> t

(** [add x y] is [x + y] *)
val add     : t -> t -> t

(** [sub x y] is [x - y] *)
val sub     : t -> t -> t

(** [mul x y] is [x * y]  *)
val mul     : t -> t -> t

(** [div x y] is [x / y]  *)
val div     : t -> t -> t

(** [modulo  x y] is [x mod y]  *)
val modulo  : t -> t -> t

(** [lnot x] is a logical negation of [x] (1-complement) *)
val lnot    : t -> t
(** [logand x y] is a conjunction of [x] and [y]  *)
val logand  : t -> t -> t

(** [logor x y] is a disjunction of [x] and [y]  *)
val logor   : t -> t -> t

(** [logxor x y] is exclusive or between [x] and [y]  *)
val logxor  : t -> t -> t

(** [lshift x y] shift [x] by [y] bits left *)
val lshift  : t -> t -> t

(** [rshift x y] shift [x] by [y] bits to the right  *)
val rshift  : t -> t -> t

(** [arshift x y] shift [x] by [y] bits to the right and fill with
    the sign bit.  *)
val arshift : t -> t -> t

val gcd    : t -> t -> t
val lcm    : t -> t -> t
val gcdext : t -> t -> (t * t * t)

module Syntax : sig
  (** {3 A common set of infix operators} *)

  (** [~-x = neg x]  *)
  val ( ~-)  : t -> t

  (** [x + y = add x y]  *)
  val ( + )  : t -> t -> t

  (** [x - y = sub x y]  *)
  val ( - )  : t -> t -> t

  (** [x * y = mul x y]  *)
  val ( * )  : t -> t -> t

  (** [x / y = div x y]  *)
  val ( / )  : t -> t -> t

  (** [x mod y = modulo x y]  *)
  val (mod)  : t -> t -> t

  (** [x land y = logand x y]  *)
  val (land) : t -> t -> t

  (** [x lor y = logor x y]  *)
  val (lor)  : t -> t -> t

  (** [lxor x y = logxor x y]  *)
  val (lxor) : t -> t -> t

  (** [x lsl y = lshift x y]  *)
  val (lsl)  : t -> t -> t

  (** [x lsr y] = rshift x y  *)
  val (lsr)  : t -> t -> t

  (** [x asr y = arshift x y]  *)
  val (asr)  : t -> t -> t

  val (++) : t -> int -> t
  val (--) : t -> int -> t

end


val pp_generic :
  ?case:[`upper | `lower ] ->
  ?prefix:[`auto | `base | `none | `this of string ] ->
  ?suffix:[`none | `full | `size ] ->
  ?format:[`hex | `dec | `oct | `bin ] ->
  formatter -> t -> unit

val pp_hex : formatter -> t -> unit
val pp_dec : formatter -> t -> unit
val pp_oct : formatter -> t -> unit
val pp_bin : formatter -> t -> unit

val pp_hex_full : formatter -> t -> unit
val pp_dec_full : formatter -> t -> unit
val pp_oct_full : formatter -> t -> unit
val pp_bin_full : formatter -> t -> unit

val string_of_value : ?hex:bool -> t -> string
