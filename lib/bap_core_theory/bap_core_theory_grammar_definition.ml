type word = Bap.Std.word

module type Bitv = sig
  type t
  type exp
  type rmode

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
  val let_ : string -> exp -> exp -> t
  val append : exp -> exp -> t
  val concat : exp list -> t

  val cast_int : int -> rmode -> exp -> t
  val cast_sint : int -> rmode -> exp -> t
end

module type Bool = sig
  type t
  type exp
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
  val let_ : string -> exp -> exp -> t

  val high : exp -> t
  val low : exp -> t
  val extract : int -> exp -> t

  val not : exp -> t
  val logand : exp -> exp -> t
  val logor: exp -> exp -> t
  val logxor : exp -> exp -> t

  val is_snan : exp -> t
  val is_qnan : exp -> t
  val is_pinf : exp -> t
  val is_ninf : exp -> t

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

  (** [store mem key data] *)
  val store : exp -> exp -> exp -> t


  (** [store_word dir mem key data ]  *)
  val store_word : exp -> exp -> exp -> exp -> t
  val var : string -> int -> int -> t
  val unknown : int -> int -> t
  val ite : exp -> exp -> exp -> t
  val let_ : string -> exp -> exp -> t
end


module type Stmt = sig
  type t
  type exp
  type rmode
  type stmt

  val set_mem : string -> int -> int -> exp -> t
  val set_reg : string -> int -> exp -> t
  val set_bit : string -> exp -> t
  val set_float : string -> int -> int -> exp -> t
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

  val var : int -> int -> string -> t
  val unk : int -> int -> t

  val finite : int -> int -> exp -> exp -> exp -> t
  val pinf : int -> int -> t
  val ninf : int -> int -> t
  val snan : int -> int -> exp -> t
  val qnan : int -> int -> exp -> t

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

  val cast_float : int -> int -> rmode -> exp -> t
  val cast_sfloat : int -> int -> rmode -> exp -> t

  val convert : int -> int -> rmode -> exp -> t
end

module type Rmode = sig
  type t
  type exp

  val rne : t
  val rtz : t
  val rtp : t
  val rtn : t
  val rna : t
end
