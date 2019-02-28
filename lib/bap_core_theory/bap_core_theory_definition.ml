open Bap_knowledge

open Bap_core_theory_sort

module Var = Bap_core_theory_var
module Effect = Bap_core_theory_effect

type 'a sort = 'a Sort.t
type 'a effect = 'a Effect.t

type 'a t = 'a Knowledge.value Knowledge.t
type 'a pure = ('a sort -> unit) t
type 'a eff = ('a effect -> unit) t

type bool = Bool.t pure
type 'a bitv = 'a Bitv.t pure
type ('a,'b) mem = ('a,'b) Mem.t pure
type 'f float = 'f Float.t pure
type rmode = Rmode.t pure

type data = Effect.data
type ctrl = Effect.ctrl
type full = Effect.full

type ('r,'s) format = ('r,'s) Float.format

type word = Bitvec.t
type 'a var
type link
type label = link Knowledge.Object.t



module type Init = sig
  val var : 'a var -> 'a pure t
  val unk : 'a sort -> 'a pure t
  val let_ : 'a var -> 'a pure t -> 'b pure t -> 'b pure t
end

module type Bool = sig
  val b0 : bool
  val b1 : bool
  val inv : bool -> bool
  val and_ : bool -> bool -> bool
  val or_ : bool -> bool -> bool
end

module type Bitv = sig
  val int : 'a bitv sort -> word -> 'a bitv
  val msb : 'a bitv -> bool
  val lsb : 'a bitv -> bool
  val neg  : 'a bitv -> 'a bitv
  val not    : 'a bitv -> 'a bitv
  val add  : 'a bitv -> 'a bitv -> 'a bitv
  val sub  : 'a bitv -> 'a bitv -> 'a bitv
  val mul  : 'a bitv -> 'a bitv -> 'a bitv
  val div  : 'a bitv -> 'a bitv -> 'a bitv
  val sdiv : 'a bitv -> 'a bitv -> 'a bitv
  val modulo : 'a bitv -> 'a bitv -> 'a bitv
  val smodulo : 'a bitv -> 'a bitv -> 'a bitv
  val logand : 'a bitv -> 'a bitv -> 'a bitv
  val logor  : 'a bitv -> 'a bitv -> 'a bitv
  val logxor  : 'a bitv -> 'a bitv -> 'a bitv
  val shiftr : bool -> 'a bitv -> 'b bitv -> 'a bitv
  val shiftl : bool -> 'a bitv -> 'b bitv -> 'a bitv
  val ite : bool -> 'a -> 'a -> 'a
  val sle : 'a bitv -> 'a bitv -> bool
  val ule : 'a bitv -> 'a bitv -> bool
  val cast : 'a bitv sort -> bool -> 'b bitv -> 'a bitv
  val concat : 'a bitv sort -> 'b bitv list -> 'a bitv
  val append : 'a bitv sort -> 'b bitv -> 'c bitv -> 'a bitv
end

module type Memory = sig
  val load : ('a,'b) mem -> 'a bitv -> 'b bitv
  val store : ('a,'b) mem -> 'a bitv -> 'b bitv -> ('a,'b) mem
end

module type Effect = sig
  val perform : 'a effect -> 'a eff
  val set : 'a var -> 'a pure t -> data eff
  val jmp  : _ bitv -> ctrl eff
  val goto : label -> ctrl eff
  val seq : 'a eff -> 'a eff -> 'a eff
  val blk : label -> data eff -> ctrl eff -> unit eff
  val repeat : bool -> data eff -> data eff
  val branch : bool -> 'a eff -> 'a eff -> 'a eff
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
  val zero : 'a bitv sort -> 'a bitv
  val is_zero  : 'a bitv -> bool
  val non_zero : 'a bitv -> bool
  val succ : 'a bitv -> 'a bitv
  val pred : 'a bitv -> 'a bitv
  val nsucc : 'a bitv -> int -> 'a bitv
  val npred : 'a bitv -> int -> 'a bitv
  val high : 'a bitv sort -> 'b bitv -> 'a bitv
  val low  : 'a bitv sort -> 'b bitv -> 'a bitv
  val signed : 'a bitv sort -> 'b bitv -> 'a bitv
  val unsigned  : 'a bitv sort -> 'b bitv -> 'a bitv
  val extract : 'a bitv sort -> 'b bitv -> 'b bitv -> _ bitv -> 'a bitv
  val loadw : 'c bitv sort -> bool -> ('a, _) mem -> 'a bitv -> 'c bitv
  val storew : bool -> ('a, 'b) mem -> 'a bitv -> 'c bitv -> ('a, 'b) mem
  val arshift : 'a bitv -> 'b bitv -> 'a bitv
  val rshift : 'a bitv -> 'b bitv -> 'a bitv
  val lshift : 'a bitv -> 'b bitv -> 'a bitv
  val eq  : 'a bitv -> 'a bitv -> bool
  val neq : 'a bitv -> 'a bitv -> bool
  val slt : 'a bitv -> 'a bitv -> bool
  val ult : 'a bitv -> 'a bitv -> bool
  val sgt : 'a bitv -> 'a bitv -> bool
  val ugt : 'a bitv -> 'a bitv -> bool
  val sge : 'a bitv -> 'a bitv -> bool
  val uge : 'a bitv -> 'a bitv -> bool
end

module type Fbasic = sig
  val float : ('r,'s) format float sort -> 's bitv -> ('r,'s) format float
  val fbits : ('r,'s) format float -> 's bitv


  val is_finite : 'f float -> bool
  val is_nan : 'f float -> bool
  val is_inf : 'f float -> bool
  val is_fzero : 'f float -> bool
  val is_fpos : 'f float -> bool
  val is_fneg : 'f float -> bool

  val rne : rmode
  val rna : rmode
  val rtp : rmode
  val rtn : rmode
  val rtz : rmode
  val requal : rmode -> rmode -> bool

  val cast_float  : 'f float sort  -> rmode -> 'a bitv -> 'f float
  val cast_sfloat : 'f float sort -> rmode -> 'a bitv -> 'f float
  val cast_int    : 'a bitv sort -> rmode -> 'f float -> 'a bitv
  val cast_sint   : 'a bitv sort -> rmode -> 'f float -> 'a bitv

  val fneg    : 'f float -> 'f float
  val fabs    : 'f float -> 'f float

  val fadd    : rmode -> 'f float -> 'f float -> 'f float
  val fsub    : rmode -> 'f float -> 'f float -> 'f float
  val fmul    : rmode -> 'f float -> 'f float -> 'f float
  val fdiv    : rmode -> 'f float -> 'f float -> 'f float
  val fsqrt   : rmode -> 'f float -> 'f float
  val fmodulo : rmode -> 'f float -> 'f float -> 'f float
  val fmad    : rmode -> 'f float -> 'f float -> 'f float -> 'f float

  val fround   : rmode -> 'f float -> 'f float
  val fconvert : 'f float sort ->  rmode -> _ float -> 'f float

  val fsucc  : 'f float -> 'f float
  val fpred  : 'f float -> 'f float
  val forder : 'f float -> 'f float -> bool
end

module type Float = sig
  include Fbasic
  val pow      : rmode -> 'f float -> 'f float -> 'f float
  val powr     : rmode -> 'f float -> 'f float -> 'f float
  val compound : rmode -> 'f float -> 'a bitv -> 'f float
  val rootn    : rmode -> 'f float -> 'a bitv -> 'f float
  val pownn    : rmode -> 'f float -> 'a bitv -> 'f float
  val rsqrt    : rmode -> 'f float -> 'f float
  val hypot    : rmode -> 'f float -> 'f float -> 'f float
end

module type Trans = sig
  val exp      : rmode -> 'f float -> 'f float
  val expm1    : rmode -> 'f float -> 'f float
  val exp2     : rmode -> 'f float -> 'f float
  val exp2m1   : rmode -> 'f float -> 'f float
  val exp10    : rmode -> 'f float -> 'f float
  val exp10m1  : rmode -> 'f float -> 'f float
  val log      : rmode -> 'f float -> 'f float
  val log2     : rmode -> 'f float -> 'f float
  val log10    : rmode -> 'f float -> 'f float
  val logp1    : rmode -> 'f float -> 'f float
  val log2p1   : rmode -> 'f float -> 'f float
  val log10p1  : rmode -> 'f float -> 'f float
  val sin      : rmode -> 'f float -> 'f float
  val cos      : rmode -> 'f float -> 'f float
  val tan      : rmode -> 'f float -> 'f float
  val sinpi    : rmode -> 'f float -> 'f float
  val cospi    : rmode -> 'f float -> 'f float
  val atanpi   : rmode -> 'f float -> 'f float
  val atan2pi  : rmode -> 'f float -> 'f float -> 'f float
  val asin     : rmode -> 'f float -> 'f float
  val acos     : rmode -> 'f float -> 'f float
  val atan     : rmode -> 'f float -> 'f float
  val atan2    : rmode -> 'f float -> 'f float -> 'f float
  val sinh     : rmode -> 'f float -> 'f float
  val cosh     : rmode -> 'f float -> 'f float
  val tanh     : rmode -> 'f float -> 'f float
  val asinh    : rmode -> 'f float -> 'f float
  val acosh    : rmode -> 'f float -> 'f float
  val atanh    : rmode -> 'f float -> 'f float
end


module type Core = sig
  include Basic
  include Float
  include Trans
end
