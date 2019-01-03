open Core_kernel
open Caml.Format

open Bap_knowledge

type 'a sort
type 'a kind
type bit
type 'a bitv
type ('a,'b) mem
type 'a var
type data = private Data_Effect
type ctrl = private Ctrl_Effect
type 'f float
type ('r,'s) format
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
    val float : ('r,'s) format float sort -> 's bitv value t -> ('r,'s) format float value t
    val fbits : ('r,'s) format float value t -> 's bitv value t


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

  module Basic : sig
    module Make(S : Minimal) : Basic
    module Empty : Basic
  end

  module Core : sig
    module Empty : Core
  end

  module Manager : Core

  val register : ?desc:string -> name:string -> (module Core) -> unit
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

module Kind : sig
  type 'a t = 'a kind

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


module Floats : sig
  type 'f t = 'f float

  val define : ('r,'s) format -> ('r,'s) format float sort
  val format : ('r,'s) format float sort -> ('r,'s) format
  val size : ('r,'s) format float sort -> 's bitv sort

  module Format : sig
    type ('r,'s) t = ('r,'s) format
    val define : Sort.exp -> 'r -> 's bitv sort -> ('r,'s) format
    val exp : ('r,'s) format -> Sort.exp
    val bits : ('r,'s) format -> 's bitv sort
  end
end

module Rmode : sig
  type t = rmode
  val t : t sort
end

module IEEE754 : sig
  type ('a,'e,'t) t
  type ('a,'e,'t) ieee754 = ('a,'e,'t) t

  type parameters = private {
    base : int;
    bias : int;
    k : int;
    p : int;
    w : int;
    t : int;
  }


  val binary16 : parameters
  val binary32 : parameters
  val binary64 : parameters
  val binary80 : parameters
  val binary128 : parameters
  val decimal32 : parameters
  val decimal64 : parameters
  val decimal128 : parameters

  val binary : int -> parameters option
  val decimal : int -> parameters option

  module Sort : sig
    val define : parameters -> (('b,'e,'t) ieee754,'s) format float sort
    val exps : (('b,'e,'t) ieee754,'s) format float sort -> 'e bitv sort
    val sigs : (('b,'e,'t) ieee754,'s) format float sort -> 't bitv sort
    val bits : (('b,'e,'t) ieee754,'s) format float sort -> 's bitv sort
    val spec : (('b,'e,'t) ieee754,'s) format float sort -> parameters
  end
end


module Var : sig
  type 'a t = 'a var
  type ident [@@deriving bin_io, compare, sexp]

  val define : 'a sort -> string -> 'a t
  val create : 'a sort -> ident -> 'a t

  val ident : 'a t -> ident
  val name : 'a t -> string
  val sort : 'a t -> 'a sort
  val is_virtual : 'a t -> bool
  val is_mutable : 'a t -> bool
  val fresh : 'a sort -> 'a t knowledge
  val scoped : 'a sort -> ('a t -> 'b value knowledge) -> 'b value knowledge
  module Ident : Identifiable with type t := ident
end

module Value : sig
  type 'a t = 'a value

  val create : 'a sort -> semantics -> 'a t
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
  val create : 'a kind -> semantics -> 'a t
  val empty : 'a kind -> 'a t
  val get : 'b domain -> 'a t -> 'b
  val put : 'b domain -> 'a t -> 'b -> 'a t
  val kind : 'a t -> 'a kind
  val partial : 'a t -> 'a t -> Domain.Order.partial
  val merge : 'a t -> 'a t -> 'a t
  val semantics : 'a t -> semantics
end


module Link : sig
  type t

  type 'a field

  val addr : word field
  val name : string field
  val ivec : int field


  val empty : t
  val update : 'a field -> label -> 'a -> t -> t
  val lookup : 'a field -> label -> t -> 'a option

  val link : 'a field -> 'a -> label Knowledge.t
  val resolve : 'a field -> label -> 'a option Knowledge.t

  module Syntax : sig
    val link_addr : word -> label Knowledge.t
    val link_name : string -> label Knowledge.t
    val link_ivec : int -> label Knowledge.t

    val resolve_addr : label -> word option Knowledge.t
    val resolve_name : label -> string option Knowledge.t
    val resolve_ivec : label -> int option Knowledge.t
  end
end

module Grammar : sig
  type ieee754 = IEEE754.parameters
  module type Bitv = sig
    type t
    type exp
    type rmode

    val error : t

    val unsigned : int -> exp -> t
    val signed : int -> exp -> t
    val high : int -> exp -> t
    val low : int -> exp -> t
    val cast : int -> exp -> exp -> t
    val extract : int -> exp -> exp -> exp -> t

    val add : exp -> exp -> t
    val sub : exp -> exp -> t
    val mul : exp -> exp -> t
    val div : exp -> exp -> t
    val sdiv : exp -> exp -> t
    val modulo : exp -> exp -> t
    val smodulo : exp -> exp -> t
    val lshift : exp -> exp -> t
    val rshift : exp -> exp -> t
    val arshift : exp -> exp -> t
    val logand : exp -> exp -> t
    val logor: exp -> exp -> t
    val logxor : exp -> exp -> t

    val neg : exp -> t
    val not : exp -> t

    val load_word : int -> exp -> exp -> exp -> t
    val load : exp -> exp -> t


    val var : string -> int -> t
    val int : word -> t
    val unknown : int -> t
    val ite : exp -> exp -> exp -> t

    val let_bit : string -> exp -> exp -> t
    val let_reg : string -> exp -> exp -> t
    val let_mem : string -> exp -> exp -> t
    val let_float : string -> exp -> exp -> t

    val append : exp -> exp -> t
    val concat : exp list -> t

    val cast_int : int -> rmode -> exp -> t
    val cast_sint : int -> rmode -> exp -> t
    val fbits : exp -> t
  end

  module type Bool = sig
    type t
    type exp

    val error : t

    val eq : exp -> exp -> t
    val neq : exp -> exp -> t
    val lt : exp -> exp -> t
    val le : exp -> exp -> t
    val slt : exp -> exp -> t
    val sle : exp -> exp -> t
    val var : string -> t
    val int : word -> t
    val unknown : unit -> t
    val ite : exp -> exp -> exp -> t
    val let_bit : string -> exp -> exp -> t
    val let_reg : string -> exp -> exp -> t
    val let_mem : string -> exp -> exp -> t
    val let_float : string -> exp -> exp -> t

    val high : exp -> t
    val low : exp -> t
    val extract : int -> exp -> t

    val not : exp -> t
    val logand : exp -> exp -> t
    val logor: exp -> exp -> t
    val logxor : exp -> exp -> t

    val is_inf : exp -> t
    val is_nan : exp -> t
    val is_fzero : exp -> t
    val is_fpos : exp -> t
    val is_fneg : exp -> t

    val fle  : exp -> exp -> t
    val flt  : exp -> exp -> t
    val feq  : exp -> exp -> t
  end


  module type Mem = sig
    type t
    type exp

    val error : t

    (** [store mem key data] *)
    val store : exp -> exp -> exp -> t


    (** [store_word dir mem key data ]  *)
    val store_word : exp -> exp -> exp -> exp -> t
    val var : string -> int -> int -> t
    val unknown : int -> int -> t
    val ite : exp -> exp -> exp -> t
    val let_bit : string -> exp -> exp -> t
    val let_reg : string -> exp -> exp -> t
    val let_mem : string -> exp -> exp -> t
    val let_float : string -> exp -> exp -> t
  end

  module type Stmt = sig
    type t
    type exp
    type rmode
    type stmt

    val error : t

    val set_mem : string -> int -> int -> exp -> t
    val set_reg : string -> int -> exp -> t
    val set_bit : string -> exp -> t
    val set_ieee754 : string -> ieee754 -> exp -> t
    val set_rmode : string -> rmode -> t

    val tmp_mem : string -> exp -> t
    val tmp_reg : string -> exp -> t
    val tmp_bit : string -> exp -> t
    val tmp_float : string -> exp -> t
    val tmp_rmode : string -> rmode -> t

    val let_mem : string -> exp -> stmt -> t
    val let_reg : string -> exp -> stmt -> t
    val let_bit : string -> exp -> stmt -> t
    val let_float : string -> exp -> stmt -> t
    val let_rmode : string -> rmode -> stmt -> t

    val jmp : exp -> t
    val goto :  word -> t
    val special : string -> t
    val cpuexn : int -> t

    val while_ : exp -> stmt list -> t
    val if_ : exp -> stmt list -> stmt list -> t

    val seq : stmt list -> t
  end

  module type Float = sig
    type t
    type exp
    type rmode

    val error : t

    val ieee754 : ieee754 -> exp -> t
    val ieee754_var : ieee754 -> string -> t
    val ieee754_unk : ieee754 -> t
    val ieee754_cast : ieee754 -> rmode -> exp -> t
    val ieee754_cast_signed : ieee754 -> rmode -> exp -> t
    val ieee754_convert : ieee754 -> rmode -> exp -> t

    val ite : exp -> exp -> exp -> t

    val fadd : rmode -> exp -> exp -> t
    val fsub : rmode -> exp -> exp -> t
    val fmul : rmode -> exp -> exp -> t
    val fdiv : rmode -> exp -> exp -> t
    val frem : rmode -> exp -> exp -> t
    val fmin : exp -> exp -> t
    val fmax : exp -> exp -> t

    val fabs : exp -> t
    val fneg : exp -> t
    val fsqrt : rmode -> exp -> t
    val fround : rmode -> exp -> t

    val let_bit : string -> exp -> exp -> t
    val let_reg : string -> exp -> exp -> t
    val let_mem : string -> exp -> exp -> t
    val let_float : string -> exp -> exp -> t
  end

  module type Rmode = sig
    type t
    type exp

    val error : t

    val rne : t
    val rtz : t
    val rtp : t
    val rtn : t
    val rna : t
  end
end

module Parser : sig
  type ('a,'e,'r) bitv_parser =
    (module Grammar.Bitv with type t = 'a
                          and type exp = 'e
                          and type rmode = 'r) ->
    'e -> 'a

  type ('a,'e,'r) bool_parser =
    (module Grammar.Bool with type t = 'a
                          and type exp = 'e) ->
    'e -> 'a

  type ('a,'e) mem_parser =
    (module Grammar.Mem with type t = 'a
                         and type exp = 'e) ->
    'e -> 'a

  type ('a,'e,'r,'s) stmt_parser =
    (module Grammar.Stmt with type t = 'a
                          and type exp = 'e
                          and type stmt = 's
                          and type rmode = 'r) ->
    's -> 'a

  type ('a,'e,'r) float_parser =
    (module Grammar.Float with type t = 'a
                           and type exp = 'e
                           and type rmode = 'r) ->
    'e -> 'a

  type ('a,'e) rmode_parser =
    (module Grammar.Rmode with type t = 'a
                           and type exp = 'e) ->
    'e -> 'a

  type ('e,'r,'s) t = {
    bitv : 'a. ('a,'e,'r) bitv_parser;
    bool : 'a. ('a,'e,'r) bool_parser;
    mem  : 'a. ('a,'e) mem_parser;
    stmt : 'a. ('a,'e,'r,'s) stmt_parser;
    float : 'a . ('a,'e,'r) float_parser;
    rmode : 'a . ('a,'r) rmode_parser;
  }

  type ('e,'r,'s) parser = ('e,'r,'s) t

  module Make(S : Theory.Core) : sig
    val run : ('e,'r,'s) parser -> 's list -> unit eff knowledge
  end
end
