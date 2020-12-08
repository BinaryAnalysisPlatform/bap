(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)

module Bil = X86_legacy_bil
open Bil

val cjmp : Ast.exp -> Ast.exp -> Ast.stmt
val unknown : Type.typ -> string -> Ast.exp
val binop : Type.binop_type -> Ast.exp -> Ast.exp -> Ast.exp
val unop : Type.unop_type -> Ast.exp -> Ast.exp
val concat : Ast.exp -> Ast.exp -> Ast.exp
val extract : int -> int -> Ast.exp -> Ast.exp
val exp_and : Ast.exp -> Ast.exp -> Ast.exp
val exp_or : Ast.exp -> Ast.exp -> Ast.exp
val exp_eq : Ast.exp -> Ast.exp -> Ast.exp
val exp_not : Ast.exp -> Ast.exp
val exp_implies : Ast.exp -> Ast.exp -> Ast.exp
val exp_shl : Ast.exp -> Ast.exp -> Ast.exp
val exp_shr : Ast.exp -> Ast.exp -> Ast.exp
val ( +* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( -* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( ** ) : Ast.exp -> Ast.exp -> Ast.exp
val ( <<* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( >>* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( >>>* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( &* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( |* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( ^* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( ==* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( <>* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( <* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( >* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( <=* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( >=* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( =* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( ++* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( %* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( $%* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( /* ) : Ast.exp -> Ast.exp -> Ast.exp
val ( $/* ) : Ast.exp -> Ast.exp -> Ast.exp
val _maybe_float : Ast.exp -> bool
val fadd : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val fsub : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val fmul : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val fdiv : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val frem : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val fmin : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val fmax : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val fle : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val flt : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val feq : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp -> Ast.exp
val fabs : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fneg : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fsqrt : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fround : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fisnorm : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fissub : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fiszero : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fisinf : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fisnan : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fisneg : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val fispos : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
val ftoubv :
  ?rm:Type.roundmode_type -> bv_size:Type.bv_size -> Ast.exp -> Ast.exp
val ftosbv :
  ?rm:Type.roundmode_type -> bv_size:Type.bv_size -> Ast.exp -> Ast.exp
val ftoieeebv :
  ?rm:Type.roundmode_type -> bv_size:Type.bv_size -> Ast.exp -> Ast.exp
val bvtouf :
  ?rm:Type.roundmode_type -> float_size:Type.float_size -> Ast.exp -> Ast.exp
val bvtosf :
  ?rm:Type.roundmode_type -> float_size:Type.float_size -> Ast.exp -> Ast.exp
val ftof :
  ?rm:Type.roundmode_type -> float_size:Type.float_size -> Ast.exp -> Ast.exp
val ieeebvtof :
  ?rm:Type.roundmode_type -> float_size:Type.float_size -> Ast.exp -> Ast.exp
val fnan : ?rm:Type.roundmode_type -> float_size:Type.float_size -> Ast.exp
val cast : Type.cast_type -> Type.typ -> Ast.exp -> Ast.exp
val cast_low : Type.typ -> Ast.exp -> Ast.exp
val cast_high : Type.typ -> Ast.exp -> Ast.exp
val cast_signed : Type.typ -> Ast.exp -> Ast.exp
val cast_unsigned : Type.typ -> Ast.exp -> Ast.exp
val exp_int : Big_int_Z.big_int -> Type.bv_size -> Ast.exp
val int_to_big_uint : width:Type.bv_size -> value:int -> Z.t
val it : int -> Type.typ -> Ast.exp
val exp_ite : ?t:Type.typ -> Ast.exp -> Ast.exp -> Ast.exp -> Ast.exp
val min_symbolic : signed:bool -> Ast.exp -> Ast.exp -> Ast.exp
val max_symbolic : signed:bool -> Ast.exp -> Ast.exp -> Ast.exp
val extract_element : Type.typ -> Ast.exp -> int -> Ast.exp
