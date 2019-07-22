open Core_kernel
open Caml.Format

open Bap_knowledge

module KB = Knowledge

module Theory : sig
  module Value : sig
    type +'a sort
    type cls

    type 'a t = (cls,'a sort) KB.cls KB.value
    val cls : (cls,unit) KB.cls

    val empty : 'a sort -> 'a t
    val sort : 'a t -> 'a sort

    module Sort : sig
      type +'a t = 'a sort
      type +'a sym
      type +'a num
      type name

      val sym : name -> 'a sym sort
      val int : int -> 'a num sort
      val app : 'a sort -> 'b sort -> ('a -> 'b) sort
      val (@->) : 'a sort -> 'b sort -> ('a -> 'b) sort

      val value : 'a num sort -> int
      val name :  'a sym sort -> name

      val hd : ('a -> 'b) sort -> 'a sort
      val tl : ('a -> 'b) sort -> 'b sort

      val refine : name -> unit sort -> 'a t option

      val forget : 'a t -> unit t

      val same : 'a t -> 'b t -> bool

      val pp : formatter -> 'a t -> unit

      module Top : sig
        type t = unit sort [@@deriving bin_io, compare, sexp]
        include Base.Comparable.S with type t := t
      end

      module Name : sig
        type t
        val declare : ?package:string -> string -> name
        include Base.Comparable.S with type t := t
      end
    end
  end

  module Effect : sig
    type +'a sort
    type cls

    type 'a t = (cls,'a sort) KB.cls KB.value
    val cls : (cls,unit) KB.cls

    val empty : 'a sort -> 'a t
    val sort : 'a t -> 'a sort

    module Sort : sig
      type +'a t = 'a sort
      type data = private Data
      type ctrl = private Ctrl

      val data : string -> data t
      val ctrl : string -> ctrl t

      val top : unit t
      val bot : 'a t

      val both : 'a t -> 'a t -> 'a t
      val (&&) : 'a t -> 'a t -> 'a t
      val union : 'a t list -> 'a t
      val join : 'a t list -> 'b t list -> unit t

      val order : 'a t -> 'b t -> KB.Order.partial


      val rreg : data t
      val wreg : data t
      val rmem : data t
      val wmem : data t
      val barr : data t
      val fall : ctrl t
      val jump : ctrl t
      val cjmp : ctrl t
    end
  end

  type 'a value = 'a Value.t
  type 'a effect = 'a Effect.t

  module Bool : sig
    type t
    val t : t Value.sort
    val refine : unit Value.sort -> t Value.sort option
  end


  module Bitv : sig
    type 'a t
    val define : int -> 'a t Value.sort
    val refine : unit Value.sort -> 'a t Value.sort option
    val size : 'a t Value.sort -> int
  end

  module Mem : sig
    type ('a,'b) t
    val define : 'a Bitv.t Value.sort -> 'b Bitv.t Value.sort -> ('a,'b) t Value.sort
    val refine : unit Value.sort -> ('a,'b) t Value.sort option
    val keys : ('a,'b) t Value.sort -> 'a Bitv.t Value.sort
    val vals : ('a,'b) t Value.sort -> 'b Bitv.t Value.sort
  end

  module Float : sig
    module Format : sig
      type ('r,'s) t
      val define : 'r Value.sort -> 's Bitv.t Value.sort -> ('r,'s) t Value.sort
      val bits : ('r,'s) t Value.sort -> 's Bitv.t Value.sort
      val exp : ('r,'s) t Value.sort -> 'r Value.sort
    end

    type ('r,'s) format = ('r,'s) Format.t
    type 'f t

    val define : ('r,'s) format Value.sort -> ('r,'s) format t Value.sort
    val refine : unit Value.sort -> ('r,'s) format t Value.sort option
    val format : ('r,'s) format t Value.sort -> ('r,'s) format Value.sort
    val size : ('r,'s) format t Value.sort -> 's Bitv.t Value.sort
  end

  module Rmode : sig
    type t
    val t : t Value.sort
    val refine : unit Value.sort -> t Value.sort option
  end

  type 'a pure = 'a value knowledge
  type 'a eff = 'a effect knowledge

  type ('r,'s) format = ('r,'s) Float.format

  module Var : sig
    type 'a t
    type ident [@@deriving bin_io, compare, sexp]
    type ord

    val define : 'a Value.sort -> string -> 'a t
    val create : 'a Value.sort -> ident -> 'a t
    val forget : 'a t -> unit t
    val resort : 'a t -> 'b Value.sort -> 'b t

    val versioned: 'a t -> int -> 'a t
    val version : 'a t -> int

    val ident : 'a t -> ident
    val name : 'a t -> string
    val sort : 'a t -> 'a Value.sort
    val is_virtual : 'a t -> bool
    val is_mutable : 'a t -> bool
    val fresh : 'a Value.sort -> 'a t knowledge
    val scoped : 'a Value.sort -> ('a t -> 'b pure) -> 'b pure

    module Ident : sig
      type t = ident [@@deriving bin_io, compare, sexp]
      include Stringable.S with type t := t
      include Base.Comparable.S with type t := t
                                 and type comparator_witness = ord
    end

    module Top : sig
      type nonrec t = unit t [@@deriving bin_io, compare, sexp]
      include Base.Comparable.S with type t := t
    end
  end

  type data = Effect.Sort.data
  type ctrl = Effect.Sort.ctrl

  type word = Bitvec.t
  type 'a var = 'a Var.t

  type program
  type label = program KB.Object.t

  module Program : sig
    type t = (program,unit) KB.cls KB.value
    val cls : (program,unit) KB.cls
    module Semantics : sig
      type cls = Effect.cls
      type t = unit Effect.t
      val cls : (cls, unit Effect.sort) Knowledge.cls
      val slot : (program, t) Knowledge.slot
      include Knowledge.Value.S with type t := t
    end
    include Knowledge.Value.S with type t := t
  end

  module Label : sig
    type t = label

    val addr : (program, Bitvec.t option) KB.slot
    val name : (program, string option) KB.slot
    val ivec : (program, int option) KB.slot
    val aliases : (program, Set.M(String).t) KB.slot

    val is_valid : (program, bool option) KB.slot
    val is_subroutine : (program, bool option) KB.slot

    val for_addr : Bitvec.t -> t knowledge
    val for_name : string -> t knowledge
    val for_ivec : int -> t knowledge

    include Knowledge.Object.S with type t := t
  end


  type bool = Bool.t pure
  type 'a bitv = 'a Bitv.t pure
  type ('a,'b) mem = ('a,'b) Mem.t pure
  type 'f float = 'f Float.t pure
  type rmode = Rmode.t pure

  module type Init = sig
    val var : 'a var -> 'a pure
    val unk : 'a Value.sort -> 'a pure
    val let_ : 'a var -> 'a pure -> 'b pure -> 'b pure
  end

  module type Bool = sig
    val b0 : bool
    val b1 : bool
    val inv : bool -> bool
    val and_ : bool -> bool -> bool
    val or_ : bool -> bool -> bool
  end

  module type Bitv = sig
    val int : 'a Bitv.t Value.sort -> word -> 'a bitv
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
    val ite : bool -> 'a pure -> 'a pure -> 'a pure
    val sle : 'a bitv -> 'a bitv -> bool
    val ule : 'a bitv -> 'a bitv -> bool
    val cast : 'a Bitv.t Value.sort -> bool -> 'b bitv -> 'a bitv
    val concat : 'a Bitv.t Value.sort -> 'b bitv list -> 'a bitv
    val append : 'a Bitv.t Value.sort -> 'b bitv -> 'c bitv -> 'a bitv
  end

  module type Memory = sig
    val load : ('a,'b) mem -> 'a bitv -> 'b bitv
    val store : ('a,'b) mem -> 'a bitv -> 'b bitv -> ('a,'b) mem
  end

  module type Effect = sig
    val perform : 'a Effect.sort -> 'a eff
    val set : 'a var -> 'a pure -> data eff
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
    val zero : 'a Bitv.t Value.sort -> 'a bitv
    val is_zero  : 'a bitv -> bool
    val non_zero : 'a bitv -> bool
    val succ : 'a bitv -> 'a bitv
    val pred : 'a bitv -> 'a bitv
    val nsucc : 'a bitv -> int -> 'a bitv
    val npred : 'a bitv -> int -> 'a bitv
    val high : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv
    val low  : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv
    val signed : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv
    val unsigned  : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv
    val extract : 'a Bitv.t Value.sort -> 'b bitv -> 'b bitv -> _ bitv -> 'a bitv
    val loadw : 'c Bitv.t Value.sort -> bool -> ('a, _) mem -> 'a bitv -> 'c bitv
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
    val float : ('r,'s) format Float.t Value.sort -> 's bitv -> ('r,'s) format float
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

    val cast_float  : 'f Float.t Value.sort  -> rmode -> 'a bitv -> 'f float
    val cast_sfloat : 'f Float.t Value.sort -> rmode -> 'a bitv -> 'f float
    val cast_int    : 'a Bitv.t Value.sort -> rmode -> 'f float -> 'a bitv
    val cast_sint   : 'a Bitv.t Value.sort -> rmode -> 'f float -> 'a bitv

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
    val fconvert : 'f Float.t Value.sort ->  rmode -> _ float -> 'f float

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

  module Basic : sig
    module Make(S : Minimal) : Basic
    module Empty : Basic
  end

  module Core : sig
    module Empty : Core
  end

  module Manager : Core

  val register : ?desc:string -> name:string -> (module Core) -> unit


  module IEEE754 : sig
    type ('a,'e,'t) t
    type ('a,'e,'t) ieee754 = ('a,'e,'t) t
    (*  see IEEE754 3.6 *)
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
      val define : parameters -> (('b,'e,'t) ieee754,'s) format Float.t Value.sort
      val exps : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 'e Bitv.t Value.sort
      val sigs : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 't Bitv.t Value.sort
      val bits : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 's Bitv.t Value.sort
      val spec : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> parameters
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
      val int : word -> int -> t
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
      val call : string -> t
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

    module Make(S : Core) : sig
      val run : ('e,'r,'s) parser -> 's list -> unit eff
    end
  end
end
