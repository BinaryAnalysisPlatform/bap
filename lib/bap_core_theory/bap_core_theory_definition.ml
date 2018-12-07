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
type 'f float = 'f Floats.t
type ('r,'s) format = ('r,'s) Floats.format
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

  val float : ('r,'s) format float sort -> 's bitv value t -> ('r,'s) format float value t
  val fbits :  ('r,'s) format float value t -> 's bitv value t

  val is_finite : 'f float value t -> bit value t
  val is_nan : 'f float value t -> bit value t
  val is_inf : 'f float value t -> bit value t
  val is_fzero : 'f float value t -> bit value t
  val is_fpos : 'f float value t -> bit value t
  val is_fneg : 'f float value t -> bit value t

  val rne : rmode value t
  val rna : rmode value t
  val rtp : rmode value t
  val rtn : rmode value t
  val rtz : rmode value t
  val requal : rmode value t -> rmode value t -> bit value t

  val cast_float  : 'f float sort  -> rmode value t -> 'a bitv value t -> 'f float value t
  val cast_sfloat : 'f float sort -> rmode value t -> 'a bitv value t -> 'f float value t
  val cast_int    : 'a bitv sort -> rmode value t -> 'f float value t -> 'a bitv value t
  val cast_sint   : 'a bitv sort -> rmode value t -> 'f float value t -> 'a bitv value t

  val fneg    : 'f float value t -> 'f float value t
  val fabs    : 'f float value t -> 'f float value t

  val fadd    : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val fsub    : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val fmul    : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val fdiv    : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val fsqrt   : rmode value t -> 'f float value t -> 'f float value t
  val fmodulo : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val fmad    : rmode value t -> 'f float value t -> 'f float value t ->
    'f float value t ->
    'f float value t

  val fround   : rmode value t -> 'f float value t -> 'f float value t
  val fconvert : 'f float sort ->  rmode value t -> _ float value t -> 'f float value t

  val fsucc  : 'f float value t -> 'f float value t
  val fpred  : 'f float value t -> 'f float value t
  val forder : 'f float value t -> 'f float value t -> bit value t
end

module type Float = sig
  include Fbasic
  val pow      : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val powr     : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val compound : rmode value t -> 'f float value t -> 'a bitv value t -> 'f float value t
  val rootn    : rmode value t -> 'f float value t -> 'a bitv value t -> 'f float value t
  val pownn    : rmode value t -> 'f float value t -> 'a bitv value t -> 'f float value t
  val rsqrt    : rmode value t -> 'f float value t -> 'f float value t
  val hypot    : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
end

module type Trans = sig
  val exp      : rmode value t -> 'f float value t -> 'f float value t
  val expm1    : rmode value t -> 'f float value t -> 'f float value t
  val exp2     : rmode value t -> 'f float value t -> 'f float value t
  val exp2m1   : rmode value t -> 'f float value t -> 'f float value t
  val exp10    : rmode value t -> 'f float value t -> 'f float value t
  val exp10m1  : rmode value t -> 'f float value t -> 'f float value t
  val log      : rmode value t -> 'f float value t -> 'f float value t
  val log2     : rmode value t -> 'f float value t -> 'f float value t
  val log10    : rmode value t -> 'f float value t -> 'f float value t
  val logp1    : rmode value t -> 'f float value t -> 'f float value t
  val log2p1   : rmode value t -> 'f float value t -> 'f float value t
  val log10p1  : rmode value t -> 'f float value t -> 'f float value t
  val sin      : rmode value t -> 'f float value t -> 'f float value t
  val cos      : rmode value t -> 'f float value t -> 'f float value t
  val tan      : rmode value t -> 'f float value t -> 'f float value t
  val sinpi    : rmode value t -> 'f float value t -> 'f float value t
  val cospi    : rmode value t -> 'f float value t -> 'f float value t
  val atanpi   : rmode value t -> 'f float value t -> 'f float value t
  val atan2pi  : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val asin     : rmode value t -> 'f float value t -> 'f float value t
  val acos     : rmode value t -> 'f float value t -> 'f float value t
  val atan     : rmode value t -> 'f float value t -> 'f float value t
  val atan2    : rmode value t -> 'f float value t -> 'f float value t -> 'f float value t
  val sinh     : rmode value t -> 'f float value t -> 'f float value t
  val cosh     : rmode value t -> 'f float value t -> 'f float value t
  val tanh     : rmode value t -> 'f float value t -> 'f float value t
  val asinh    : rmode value t -> 'f float value t -> 'f float value t
  val acosh    : rmode value t -> 'f float value t -> 'f float value t
  val atanh    : rmode value t -> 'f float value t -> 'f float value t
end



module type Core = sig
  include Basic
  include Float
  include Trans
end
