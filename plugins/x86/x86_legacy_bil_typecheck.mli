(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(** Type checking and inference for AST programs.

    @author Ed Schwartz
*)

module Bil = X86_legacy_bil
open Bil

exception TypeError of string
(** Exception raised when a type check error occurs. *)

(** {3 Type inference of expressions} *)

val infer_ast : Ast.exp -> Type.typ
(** [infer_ast e] returns the type of the expression [e]. *)

(** {3 Type checking} *)

val typecheck_expression : Ast.exp -> unit
(** [typecheck_expression e] type checks the expression [e].

    @raise TypeError if the expression does not type check.
*)

val typecheck_stmt : Ast.stmt -> unit
(** [typecheck_expression s] type checks the statement [s].

    @raise TypeError if the statement does not type check.
*)

val typecheck_prog : Ast.stmt list -> unit
(** [typecheck_expression p] type checks the program [p].

    @raise TypeError if the program does not type check.
*)

(** {3 Helper functions} *)

val is_integer_type : Type.typ -> bool
(** [is_integer_type t] returns true iff [t] is a register type. *)

val is_mem_type : Type.typ -> bool
(** [is_mem_type t] returns true iff [t] is of memory or array type. *)

val is_float_type : Type.typ -> bool
(** [is_float_type t] returns true iff [t] is a float type. *)

val index_type_of : Type.typ -> Type.typ
(** [index_type_of t] returns the index type for memory loads and
    stores in a memory of type [t].

    @raise Invalid_arg if [t] is a non-memory type. *)

val value_type_of : Type.typ -> Type.typ
(** [index_type_of t] returns the value type for memory loads and
    stores in a memory of type [t]. For instance, a value type of [Reg
    8] means the memory is byte addressable.

    @raise Invalid_arg if [t] is a non-memory type. *)

val bits_of_width : Type.typ -> int
(** [bits_of_width t] returns the number of bits in the register type
    [t].

    @raise Invalid_argument if [t] is not of register type. *)
val bytes_of_width : Type.typ -> int
(** [bytes_of_width t] returns the number of bytes in the register
    type [t].

    @raise Invalid_argument if [t] is not of register type, or if t is
    a register type but its size is not expressible in bytes. *)
