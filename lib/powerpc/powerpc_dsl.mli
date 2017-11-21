open Core_kernel.Std
open Bap.Std

type cast  = Bil.cast [@@deriving bin_io, compare, sexp]
type binop = Bil.binop [@@deriving bin_io, compare, sexp]
type unop  = Bil.unop [@@deriving bin_io, compare, sexp]

type e   [@@deriving bin_io, compare, sexp]
type rtl [@@deriving bin_io, compare, sexp]

(** [var v]  *)
val var : var -> e

(** [cast t w x]  *)
val cast  : cast -> int -> e -> e
val unsigned : cast
val signed : cast
val high : cast
val low : cast

(** [int w ]  *)
val int : word -> e

(** [extract ~hi ~lo x ]  *)
val extract : hi:int -> lo:int -> e -> e

(** [concat x y -> Concat (x,y)]  *)
val concat : e -> e -> e

(** [if_ cond s1 s2]  *)
val if_ : e -> rtl list -> rtl list -> rtl

(** [jmp x ] *)
val jmp : e -> rtl

(** [bil d] - returns a program in BIL language   *)
val bil : rtl list -> bil

(** [powerpc_fail error_string] - raise a failure with [error_string] *)
val powerpc_fail : ('a, unit, string, 'b) format4 -> 'a

(** [load addr_size ~addr size endian] - load from a memory *)
val load : addr_size -> addr:e -> endian -> size -> e

(** [load addr_size ~addr size endian] - store to a memory *)
val store : addr_size -> addr:e -> endian -> size -> e -> rtl

(** [fresh name type] - returns a fresh variable of [type] *)
val fresh : string -> typ -> var

(** [low32 e] - extracts low 32 bits from [e] *)
val low32 : e -> e

(** [is_negative addr_size e] - returns an eression that
    checks an [e] for negative depending on address size *)
val is_negative : addr_size -> e -> e

(** [is_positive addr_size e] - returns an eression that
    checks an [e] for positive depending on address size *)
val is_positive : addr_size -> e -> e

(** [is_zero addr_size e] - returns an eression that
    checks an [e] for zero depending on address size *)
val is_zero : addr_size -> e -> e

(** [find reg] - returns variable with the same name as
    register [reg] or thrown failure if variable is not found*)
val find : reg -> var

(** [exists reg] - returns true if exists a variable with the
    same name as register [reg].*)
val exists : reg -> bool

(** [cr_bit n] returns a CR bit number [n]. *)
val cr_bit : int -> var

(** [cr_field reg] returns a CR field bits from [reg]. *)
val cr_field : reg -> var * var * var * var

(** Infix operators  *)
module Infix : sig
  (** [x := y -> x assign y]  *)
  val (:=) : var -> e -> rtl

  (** {2 Arithmetic operations} *)

  (** [x + y ]   *)
  val ( + )   : e -> e -> e

  (** [x - y ]  *)
  val ( - )   : e -> e -> e

  (** [x * y -> x TIMES y]  *)
  val ( * )   : e -> e -> e

  (** [x / y -> x DIVIDE y]  *)
  val ( / )   : e -> e -> e

  (** [x /$ y -> x SDIVIDE y ]  *)
  val ( /$ )  : e -> e -> e

  (** [x mod y -> x MOD y]  *)
  val ( mod ) : e -> e -> e

  (** [x %$ y -> x SMOD y)]  *)
  val ( %$ )  : e -> e -> e

  (** {2 Bit operations} *)

  (** [x lsl y = x LSHIFT y]  *)
  val ( lsl ) : e -> e -> e

  (** [x lsr y = x RSHIFT y]  *)
  val ( lsr ) : e -> e -> e

  (** [x asr y = x ARSHIFT y]  *)
  val ( asr ) : e -> e -> e

  (** [x land y = x AND y]  *)
  val ( land) : e -> e -> e

  (** [x lor y = x OR y]  *)
  val ( lor ) : e -> e -> e

  (** [x lxor y = x XOR y]  *)
  val ( lxor) : e -> e -> e

  (** [lnot x = NOT x]  *)
  val lnot    : e -> e

  (** {2 Equality tests} *)

  (** [x = y -> x EQ y]  *)
  val ( = )   : e -> e -> e

  (** [x = y -> x NEQ y]  *)
  val ( <> )   : e -> e -> e

  (** [x < y -> x LT y]  *)
  val ( < )   : e -> e -> e

  (** [x > y -> x LT y]  *)
  val ( > )   : e -> e -> e

  (** [x <= y -> x LE y]  *)
  val ( <= )   : e -> e -> e

  (** [x <= y -> x LE y)]  *)
  val ( >= )   : e -> e -> e

  (** {3 Signed comparison}  *)

  (** [x <$ x -> x SLT y]  *)
  val ( <$ )  : e -> e -> e

  (** [x >$ x -> x SLT y)]  *)
  val ( >$ )  : e -> e -> e

  (** [x <=$ x -> x SLE y]  *)
  val ( <=$ ) : e -> e -> e

  (** [x >=$ x -> x SLE y]  *)
  val ( >=$ ) : e -> e -> e

  (** {2 Misc operations} *)

  (** [x ^ y -> x Concat y] *)
  val ( ^ )   : e -> e -> e
end

include module type of Infix
