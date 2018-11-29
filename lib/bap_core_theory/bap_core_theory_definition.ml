open Bap.Std
open Bap_knowledge

open Bap_core_theory_sort

module Var = Bap_core_theory_var
module Eff = Bap_core_theory_eff
module Kind = Bap_core_theory_kind
module Value = Bap_core_theory_value


type 'a t = 'a knowledge
type 'a var = 'a Var.t
type 'a value = 'a Value.t
type 'a eff = 'a Eff.t
type data = Kind.data
type ctrl = Kind.ctrl
type ('e,'k) float = ('e,'k) Floats.t
type rmode = Rmode.t

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
  val finite : ('e,'k) float sort -> bit value t ->
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
  val fconvert : ('e,'k) float sort ->  rmode value t -> (_,_) float value t -> ('e,'k) float value t

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
