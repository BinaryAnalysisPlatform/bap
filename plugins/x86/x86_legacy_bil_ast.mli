(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)

module Type = X86_legacy_bil_type
module Var = X86_legacy_bil_var

type var = Var.t [@@deriving sexp]

type exp =
    Load of (exp * exp * exp * Type.typ)  (** Load(arr,idx,endian,t) *)
  | Store of (exp * exp * exp * exp * Type.typ)  (** Store(arr,idx,val,endian,t) *)
  | BinOp of (Type.binop_type * exp * exp)
  | UnOp of (Type.unop_type * exp)
  | Var of var
  | Lab of string
  | Int of (Big_int_Z.big_int * Type.typ)
  | Cast of (Type.cast_type * Type.typ * exp) (** Cast to a new type. *)
  | Let of (var * exp * exp)
  | Unknown of (string * Type.typ)
  (* Expression types below here are just syntactic sugar for the above *)
  | Ite of (exp * exp * exp)
  | Extract of (Big_int_Z.big_int * Big_int_Z.big_int * exp) (** Extract hbits to lbits of e (Reg type) *)
  | Concat of (exp * exp) (** Concat two reg expressions together *)
[@@deriving sexp]

type attrs = Type.attributes

type stmt =
    Move of (var * exp * attrs)  (** Assign the value on the right to the
                                     var on the left *)
  | Jmp of (exp * attrs) (** Jump to a label/address *)
  | CJmp of (exp * exp * attrs)
  (** Conditional jump. If e1 is true, jumps to e2, otherwise fallthrough *)
  | Label of (Type.label * attrs) (** A label we can jump to *)
  | Halt of (exp * attrs)
  | Assert of (exp * attrs)
  | Assume of (exp * attrs)
  | Comment of (string * attrs) (** A comment to be ignored *)
  | Special of (string * Var.defuse option * attrs) (** A "special" statement. (does magic) *)

type program = stmt list

val lab_of_exp: exp -> Type.label option
val exp_false: exp
val exp_true: exp
val little_endian: exp
val big_endian: exp
