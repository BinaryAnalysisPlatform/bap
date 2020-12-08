(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(**
    The Abstract Syntax Tree.
    This IL allows nested expressions, making it closer to VEX and
    the concrete syntax than our SSA form. However, in most cases, this
    makes analysis harder, so you will generally want to convert to SSA
    for analysis.

    @author Ivan Jager
*)

open X86_legacy_bil_type
open X86_legacy_bil_var
open Big_int_Z

module Type = X86_legacy_bil_type
module Var = X86_legacy_bil_var


(* TODO: remove if Core.Std is opened in this file. *)
let string_of_sexp = Core_kernel.string_of_sexp
let sexp_of_string = Core_kernel.sexp_of_string

(** Support for s-expressions *)
let big_int_of_sexp sexp =
  Core_kernel.String.t_of_sexp sexp |> Big_int_Z.big_int_of_string
let sexp_of_big_int bi =
  Big_int_Z.string_of_big_int bi |> Core_kernel.String.sexp_of_t

type var = Var.t [@@deriving sexp]

type exp =
  | Load of (exp * exp * exp * typ)  (** Load(arr,idx,endian,t) *)
  | Store of (exp * exp * exp * exp * typ)  (** Store(arr,idx,val,endian,t) *)
  | BinOp of (binop_type * exp * exp)
  | UnOp of (unop_type * exp)
  | Var of var
  | Lab of string
  | Int of (big_int * typ)
  | Cast of (cast_type * typ * exp) (** Cast to a new type. *)
  | Let of (var * exp * exp)
  | Unknown of (string * typ)
  (* Expression types below here are just syntactic sugar for the above *)
  | Ite of (exp * exp * exp)
  | Extract of (big_int * big_int * exp) (** Extract hbits to lbits of e (Reg type) *)
  | Concat of (exp * exp) (** Concat two reg expressions together *)
[@@deriving sexp]

type attrs = Type.attributes

type stmt =
  | Move of (var * exp * attrs)  (** Assign the value on the right to the
                                     var on the left *)
  | Jmp of (exp * attrs) (** Jump to a label/address *)
  | CJmp of (exp * exp * attrs)
  (** Conditional jump. If e1 is true, jumps to e2, otherwise fallthrough *)
  | Label of (label * attrs) (** A label we can jump to *)
  | Halt of (exp * attrs)
  | Assert of (exp * attrs)
  | Assume of (exp * attrs)
  | Comment of (string * attrs) (** A comment to be ignored *)
  | Special of (string * defuse option * attrs) (** A "special" statement. (does magic) *)

type program = stmt list

(* XXX: Should we move all of these to ast_convenience? *)


(** If possible, make a label that would be refered to by the given
    expression. *)
let lab_of_exp =
  let re = Str.regexp "^pc_\\(.*\\)+" in
  function
  (* VEX style pc_0x1234 labels *)
  | Lab s when Str.string_match re s 0 ->
    Some(Addr(big_int_of_string (Str.matched_group 1 s)))
  | Lab s -> Some(Name s)
  | Int(i, t) ->
    Some(Addr i)
  | _ -> None

(** False constant. (If convenient, refer to this rather than building your own.) *)
let exp_false = Int(Big_int_Z.zero_big_int, reg_1)
(** True constant. *)
let exp_true = Int(Big_int_Z.unit_big_int, reg_1)

let little_endian = exp_false
let big_endian = exp_true
