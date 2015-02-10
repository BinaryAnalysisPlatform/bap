open Core_kernel.Std
open Bap_common

type var = Bap_var.t
with bin_io, compare, sexp

(** Different forms of casting *)
module Cast = struct
  type cast =
    | UNSIGNED (** 0-padding widening cast. *)
    | SIGNED   (** Sign-extending widening cast. *)
    | HIGH     (** Narrowning cast. Keeps the high bits. *)
    | LOW      (** Narrowing cast. Keeps the low bits. *)
  with bin_io, compare, sexp
end

type cast = Cast.cast
with bin_io, compare, sexp

(** Binary operations implemented in the IR *)
module Binop = struct
  type binop =
    | PLUS    (** Integer addition. (commutative, associative) *)
    | MINUS   (** Subtract second integer from first. *)
    | TIMES   (** Integer multiplication. (commutative, associative) *)
    | DIVIDE  (** Unsigned integer division. *)
    | SDIVIDE (** Signed integer division. *)
    | MOD     (** Unsigned modulus. *)
    | SMOD    (** Signed modulus. *)
    | LSHIFT  (** Left shift. *)
    | RSHIFT  (** Right shift, fill with 0. *)
    | ARSHIFT (** Right shift, sign extend. *)
    | AND     (** Bitwise and. (commutative, associative) *)
    | OR      (** Bitwise or. (commutative, associative) *)
    | XOR     (** Bitwise xor. (commutative, associative) *)
    | EQ      (** Equals. (commutative) (associative on booleans) *)
    | NEQ     (** Not equals. (commutative) (associative on booleans) *)
    | LT      (** Unsigned less than. *)
    | LE      (** Unsigned less than or equal to. *)
    | SLT     (** Signed less than. *)
    | SLE     (** Signed less than or equal to. *)
  with bin_io, compare, sexp
end

type binop = Binop.binop
with bin_io, compare, sexp


(** Unary operations implemented in the IR *)
module Unop = struct
  type unop =
    | NEG (** Negate. (2's complement) *)
    | NOT (** Bitwise not. *)
  with bin_io, compare, sexp
end

type unop = Unop.unop
with bin_io, compare, sexp

module Exp = struct
  type exp =
    (** Load    (mem,  idx,  endian,  size) *)
    | Load    of exp * exp * endian * size
    (** Store   (mem,  idx,  val,  endian,  size) *)
    | Store   of exp * exp * exp * endian * size
    | BinOp   of binop * exp * exp
    | UnOp    of unop * exp
    | Var     of var
    | Int     of word
    (** Cast to a new type *)
    | Cast    of cast * nat1 * exp
    | Let     of var * exp * exp
    | Unknown of string * typ
    | Ite     of exp * exp * exp
    (** Extract hbits to lbits of e (Reg type) *)
    | Extract of nat1 * nat1 * exp
    (** Concat two reg expressions together *)
    | Concat  of exp * exp
  with bin_io, compare, sexp
end

type exp = Exp.exp with bin_io, compare, sexp

module Stmt = struct
  type stmt =
    (** Assign the value on the right to the var on the left *)
    | Move    of var * exp
    (** Jump to a address *)
    | Jmp     of exp
    (** Statement with semantics not expressible in BIL *)
    | Special of string
    | While   of exp * stmt list
    | If      of exp * stmt list * stmt list
    | CpuExn  of int
  with bin_io, compare, sexp
end

type stmt = Stmt.stmt with bin_io, compare, sexp
type bil = stmt list with bin_io, compare, sexp
