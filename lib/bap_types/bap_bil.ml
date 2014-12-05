open Core_kernel.Std
open Bap_common

type var = Bap_var.t
with bin_io, compare, sexp

module Exp = struct
  type t =
    (** Load    (mem,  idx,  endian,  size) *)
    | Load    of t * t * endian * size
    (** Store   (mem,  idx,  val,  endian,  size) *)
    | Store   of t * t * t * endian * size
    | BinOp   of binop * t * t
    | UnOp    of unop * t
    | Var     of var
    | Int     of word
    (** Cast to a new type *)
    | Cast    of cast * nat1 * t
    | Let     of var * t * t
    | Unknown of string * typ
    | Ite     of t * t * t
    (** Extract hbits to lbits of e (Reg type) *)
    | Extract of nat1 * nat1 * t
    (** Concat two reg expressions together *)
    | Concat  of t * t
  with bin_io, compare, sexp, variants
end

type exp = Exp.t with bin_io, compare, sexp

module Stmt = struct
  type t =
    (** Assign the value on the right to the var on the left *)
    | Move    of var * exp
    (** Jump to a address *)
    | Jmp     of exp
    (** Statement with semantics not expressible in BIL *)
    | Special of string
    | While   of exp * t list
    | If      of exp * t list * t list
    | CpuExn  of int
  with bin_io, compare, sexp, variants
end

type stmt = Stmt.t with bin_io, compare, sexp
