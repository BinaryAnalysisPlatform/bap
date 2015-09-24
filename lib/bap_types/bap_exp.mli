(** Extends [exp] interface.  *)
open Core_kernel.Std
open Bap_common
open Bap_bil

include Regular with type t := exp

module Cast : sig
  val unsigned : cast
  val signed : cast
  val high : cast
  val low : cast
end

module Binop : sig
  val plus : binop
  val minus : binop
  val times : binop
  val divide : binop
  val sdivide : binop
  val modulo : binop
  val smodulo : binop
  val lshift : binop
  val rshift : binop
  val arshift : binop
  val bit_and : binop
  val bit_or  : binop
  val bit_xor : binop
  val eq : binop
  val neq : binop
  val lt : binop
  val le : binop
  val slt : binop
  val sle : binop
  val is_commutative : binop -> bool
  val is_associative : binop -> bool
end

module Unop : sig
  val neg : unop
  val not : unop
end

module Exp : sig
  val load : mem:var -> addr:exp -> endian -> size -> exp
  val store : mem:var -> addr:exp -> exp -> endian -> size -> exp
  val binop : binop -> exp -> exp -> exp
  val unop : unop -> exp -> exp
  val var : var -> exp
  val int : Bitvector.t -> exp
  val cast : cast -> nat1 -> exp -> exp
  val let_ : var -> exp -> exp -> exp
  val unknown : string -> typ -> exp
  val ite : if_:exp -> then_:exp -> else_:exp -> exp
  val extract : hi:nat1 -> lo:nat1 -> exp -> exp
  val concat : exp -> exp -> exp
end

module Infix : sig
  (** Arithmetic operations *)
  val ( + )   : exp -> exp -> exp
  val ( - )   : exp -> exp -> exp
  val ( * )   : exp -> exp -> exp
  val ( / )   : exp -> exp -> exp
  val ( /$ )  : exp -> exp -> exp
  val ( mod ) : exp -> exp -> exp
  val ( %$ )  : exp -> exp -> exp

  (** Bit operations *)
  val ( lsl ) : exp -> exp -> exp
  val ( lsr ) : exp -> exp -> exp
  val ( asr ) : exp -> exp -> exp
  val ( land) : exp -> exp -> exp
  val ( lor ) : exp -> exp -> exp
  val ( lxor) : exp -> exp -> exp
  val lnot    : exp -> exp

  (** Equality tests *)
  val ( = )   : exp -> exp -> exp
  val ( <> )   : exp -> exp -> exp
  val ( < )   : exp -> exp -> exp
  val ( > )   : exp -> exp -> exp
  val ( <= )   : exp -> exp -> exp
  val ( >= )   : exp -> exp -> exp
  val ( <$ )  : exp -> exp -> exp
  val ( >$ )  : exp -> exp -> exp
  val ( <=$ ) : exp -> exp -> exp
  val ( >=$ ) : exp -> exp -> exp

  (** Misc operations *)
  (** [a ^ b] contatenate [a] and [b]  *)
  val ( ^ )   : exp -> exp -> exp
end
