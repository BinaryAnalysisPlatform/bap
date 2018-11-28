open Core_kernel
open Caml.Format

open Bap_knowledge

type 'a sort
type bit
type 'a bitv
type ('a,'b) mem
type 'a var
type data = private Data_Effect
type ctrl = private Ctrl_Effect
type ('e,'s) float
type rmode
type 'a value
type 'a eff
type word = Bap.Std.Bitvector.t (* we will extract bitvector from BAP later *)

module Theory : sig
  type 'a t = 'a knowledge

  module type Init = sig
    val var : 'a var -> 'a value t
    val unk : 'a sort -> 'a value t
    val let_ : 'a var -> 'a value t -> 'b value t -> 'b value t
  end

  module type Bool = sig
    val b0 : bit value t
    val b1 : bit value t
    val inv : bit value t -> bit value t
    val and_ : bit value t -> bit value t -> bit value t
    val or_ : bit value t -> bit value t -> bit value t
  end

  module type Bitv = sig
    val int : 'a bitv sort -> word -> 'a bitv value t
    val msb : 'a bitv value t -> bit value t
    val lsb : 'a bitv value t -> bit value t
    val neg  : 'a bitv value t -> 'a bitv value t
    val not    : 'a bitv value t -> 'a bitv value t
    val add  : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val sub  : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val mul  : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val div  : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val sdiv : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val modulo : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val smodulo : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val logand : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val logor  : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val logxor  : 'a bitv value t -> 'a bitv value t -> 'a bitv value t
    val shiftr : bit value t -> 'a bitv value t -> 'b bitv value t -> 'a bitv value t
    val shiftl : bit value t -> 'a bitv value t -> 'b bitv value t -> 'a bitv value t
    val ite : bit value t -> 'a value t -> 'a value t -> 'a value t
    val sle : 'a bitv value t -> 'a bitv value t -> bit value t
    val ule : 'a bitv value t -> 'a bitv value t -> bit value t
    val cast : 'a bitv sort -> bit value t -> 'b bitv value t -> 'a bitv value t
    val concat : 'a bitv sort -> 'b bitv value t list -> 'a bitv value t
    val append : 'a bitv sort -> 'b bitv value t -> 'c bitv value t -> 'a bitv value t
  end

  module type Memory = sig
    val load : ('a,'b) mem value t -> 'a bitv value t -> 'b bitv value t
    val store : ('a,'b) mem value t -> 'a bitv value t -> 'b bitv value t -> ('a,'b) mem value t
  end

  module type Effect = sig
    val pass : data eff t
    val skip : ctrl eff t
    val set : 'a var -> 'a value t -> data eff t
    val jmp  : _ bitv value t -> ctrl eff t
    val goto : label -> ctrl eff t
    val seq : 'a eff t -> 'a eff t -> 'a eff t
    val blk : label -> data eff t -> ctrl eff t -> unit eff t
    val repeat : bit value t -> data eff t -> data eff t
    val branch : bit value t -> 'a eff t -> 'a eff t -> 'a eff t
    val atomic : data eff t -> data eff t
    val mfence : data eff t
    val lfence : data eff t
    val sfence : data eff t
  end

  module type Minimal = sig
    include Init
    include Bool
    include Bitv
    include Memory
    include Effect
  end

  module type Basic = sig
    include Minimal
    val zero : 'a bitv sort -> 'a bitv value t
    val is_zero  : 'a bitv value t -> bit value t
    val non_zero : 'a bitv value t -> bit value t

    val succ : 'a bitv value t -> 'a bitv value t
    val pred : 'a bitv value t -> 'a bitv value t

    val nsucc : 'a bitv value t -> int -> 'a bitv value t
    val npred : 'a bitv value t -> int -> 'a bitv value t


    val high : 'a bitv sort -> 'b bitv value t -> 'a bitv value t
    val low  : 'a bitv sort -> 'b bitv value t -> 'a bitv value t
    val signed : 'a bitv sort -> 'b bitv value t -> 'a bitv value t
    val unsigned  : 'a bitv sort -> 'b bitv value t -> 'a bitv value t
    val extract : 'a bitv sort -> 'b bitv value t -> 'b bitv value t -> _ bitv value t -> 'a bitv value t
    val loadw : 'c bitv sort -> bit value t ->
      ('a, _) mem value t -> 'a bitv value t -> 'c bitv value t

    val storew : bit value t ->
      ('a, 'b) mem value t -> 'a bitv value t -> 'c bitv value t ->
      ('a, 'b) mem value t

    val arshift : 'a bitv value t -> 'b bitv value t -> 'a bitv value t
    val rshift : 'a bitv value t -> 'b bitv value t -> 'a bitv value t
    val lshift : 'a bitv value t -> 'b bitv value t -> 'a bitv value t

    val eq  : 'a bitv value t -> 'a bitv value t -> bit value t
    val neq : 'a bitv value t -> 'a bitv value t -> bit value t
    val slt : 'a bitv value t -> 'a bitv value t -> bit value t
    val ult : 'a bitv value t -> 'a bitv value t -> bit value t
    val sgt : 'a bitv value t -> 'a bitv value t -> bit value t
    val ugt : 'a bitv value t -> 'a bitv value t -> bit value t
    val sge : 'a bitv value t -> 'a bitv value t -> bit value t
    val uge : 'a bitv value t -> 'a bitv value t -> bit value t
  end

  module type Fbasic = sig
    val finite : ('e,'k) float sort -> bit t ->
      'e bitv value t -> 'k bitv value t ->
      ('e,'k) float value t

    val rmode : rmode sort -> rmode value t

    val pinf : ('e,'k) float sort -> ('e,'k) float value t
    val ninf : ('e,'k) float sort -> ('e,'k) float value t
    val snan : ('e,'k) float sort -> 'x bitv value t -> ('e,'k) float value t
    val qnan : ('e,'k) float sort -> 'x bitv value t -> ('e,'k) float value t

    val exponent    : ('e,'k) float value t -> 'e bitv value t
    val significand : ('e,'k) float value t -> 'k bitv value t
    val fsign       : ('e,'k) float value t -> bit value t

    val is_finite : ('e,'k) float value t -> bit value t
    val is_fzero  : ('e,'k) float value t -> bit value t
    val is_pinf   : ('e,'k) float value t -> bit value t
    val is_ninf   : ('e,'k) float value t -> bit value t
    val is_snan   : ('e,'k) float value t -> bit value t
    val is_qnan   : ('e,'k) float value t -> bit value t

    val cast_float  : ('e,'k) float sort  -> rmode value t -> 'a bitv value t -> ('e,'k) float value t
    val cast_sfloat : ('e,'k) float sort -> rmode value t -> 'a bitv value t -> ('e,'k) float value t
    val cast_int    : 'a bitv sort -> rmode value t -> ('e,'k) float value t -> 'a bitv value t

    val fneg    : ('e,'k) float value t -> ('e,'k) float value t
    val fabs    : ('e,'k) float value t -> ('e,'k) float value t

    val fadd    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val fsub    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val fmul    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val fdiv    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val sqrt       : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val fmodulo : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val fmad    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t

    val fround   : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val fconvert : rmode value t -> ('e,'k) float sort -> (_,_) float value t -> ('e,'k) float value t

    val fsucc  : ('e,'k) float value t -> ('e,'k) float value t
    val fpred  : ('e,'k) float value t -> ('e,'k) float value t
    val forder : ('e,'k) float value t -> ('e,'k) float value t -> bit value t
  end

  module type Float = sig
    include Fbasic
    val pow      : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val powr     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val compound : rmode value t -> ('e,'k) float value t -> 'a bitv value t -> ('e,'k) float value t
    val rootn    : rmode value t -> ('e,'k) float value t -> 'a bitv value t -> ('e,'k) float value t
    val pownn    : rmode value t -> ('e,'k) float value t -> 'a bitv value t -> ('e,'k) float value t
    val rsqrt    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val hypot    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
  end

  module type Trans = sig
    val exp      : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val expm1    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val exp2     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val exp2m1   : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val exp10    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val exp10m1  : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val log      : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val log2     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val log10    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val logp1    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val log2p1   : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val log10p1  : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val sin      : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val cos      : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val tan      : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val sinpi    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val cospi    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val atanpi   : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val atan2pi  : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val asin     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val acos     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val atan     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val atan2    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t -> ('e,'k) float value t
    val sinh     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val cosh     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val tanh     : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val asinh    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val acosh    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
    val atanh    : rmode value t -> ('e,'k) float value t -> ('e,'k) float value t
  end

  module type Core = sig
    include Basic
    include Float
    include Trans
  end

  module Basic : sig
    module Make(S : Minimal) : Basic
  end

  module Core : sig
    module Empty : Core
  end

end

module Sort : sig
  type 'a t = 'a sort

  type exp =
    | Bool
    | Cons of string * param list
  and param =
    | Sort of exp
    | Index of int
  [@@deriving compare, sexp]

  val define : exp -> 'a -> 'a t
  val name : 'a t -> string
  val exp : 'a t -> exp
  val type_equal : 'a t -> 'b t -> ('a t, 'b t) Type_equal.t option
  val same : 'a t -> 'b t -> bool
  val pp_exp : formatter -> exp -> unit
  val pp : formatter -> 'a t -> unit
  val compare : 'a t -> 'a t -> int
end

module Effect : sig
  type 'a t

  val data : data t
  val ctrl : ctrl t
  val unit : unit t

  val pp : formatter -> 'a t -> unit
  val name : 'a t -> string
end

module Bool : sig
  type t = bit
  val t : bit sort
  val parse : Sort.exp -> bit sort option
  val cast : 'a sort -> bit sort option
end


module Bits : sig
  type 'a t = 'a bitv
  val define : int -> 'a bitv sort
  val size : 'a bitv sort -> int
  val parse : Sort.exp -> 'b bitv sort option
  val cast : 'a sort -> 'b bitv sort option
end

module Mems : sig
  type ('a,'b) t = ('a,'b) mem
  val define : 'a bitv sort -> 'b bitv sort -> ('a,'b) mem sort
  val keys : ('a,'b) mem sort -> 'a bitv sort
  val vals : ('a,'b) mem sort -> 'b bitv sort
  val parse : Sort.exp -> ('a,'b) mem sort option
  val cast : _ sort -> ('a,'b) mem sort option
end

module Var : sig
  type 'a t = 'a var

  val create : 'a sort -> string -> 'a var
  val name : 'a var -> string
  val sort : 'a var -> 'a sort
end

module Value : sig
  type 'a t = 'a value

  val empty : 'a sort -> 'a t
  val get : 'b domain -> 'a t -> 'b
  val put : 'b domain -> 'a t -> 'b -> 'a t
  val sort : 'a t -> 'a sort
  val partial : 'a t -> 'a t -> Domain.Order.partial
  val merge : 'a t -> 'a t -> 'a t
  val semantics : 'a t -> semantics
end


module Eff : sig
  type 'a t = 'a eff

  val empty : 'a Effect.t -> 'a t
  val get : 'b domain -> 'a t -> 'b
  val put : 'b domain -> 'a t -> 'b -> 'a t
  val kind : 'a t -> 'a Effect.t
  val partial : 'a t -> 'a t -> Domain.Order.partial
  val merge : 'a t -> 'a t -> 'a t
  val semantics : 'a t -> semantics
end


module Floats : sig
  type ('e,'s) t = ('e,'s) float
  val define : 'e bitv sort -> 's bitv sort -> ('e,'s) t sort
  val exps : ('e,'s) t sort -> 'e bitv sort
  val sigs : ('e,'s) t sort -> 's bitv sort
end

module Rmode : sig
  type mode = RNE | RNA | RTP | RTN | RTZ
  type t = rmode
  val rne : t sort
  val rna : t sort
  val rtp : t sort
  val rtn : t sort
  val rtz : t sort

  val describe : t sort -> mode
end
