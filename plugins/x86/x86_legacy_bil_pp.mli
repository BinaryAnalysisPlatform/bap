(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)

module Bil = X86_legacy_bil
open Bil

module VH = Var.Table
module F = Format
val output_varnums : bool Core_kernel.ref
val many_parens : bool Core_kernel.ref
val typ_to_string : Type.typ -> string
val ct_to_string : Type.cast_type -> string
val fbinop_to_string : Type.fbinop_type -> string
val roundmode_to_string : Type.roundmode_type -> string
val funop_to_string : Type.funop_type -> string
val binop_to_string : Type.binop_type -> string
val unop_to_string : Type.unop_type -> string
val option_may : f:('a -> unit) -> 'a option -> unit
val reasonable_size_varctx : int
val printed_varctx_warning : bool Core_kernel.ref
type varctx = string VH.t * (string, unit) Core_kernel.Hashtbl.t
val var_to_string :
  ?ctx:('a, string) VH.t_ * (string, unit) Core_kernel.Hashtbl.t ->
  Var.t -> string
class pp :
  F.formatter ->
  object
    method ast_endian : Ast.exp -> unit
    method ast_exp : ?prec:int -> Ast.exp -> unit
    method ast_program : Ast.stmt Core_kernel.List.t -> unit
    method ast_stmt : Ast.stmt -> unit
    method attr : Type.attribute -> unit
    method attrs : Type.attribute Core_kernel.List.t -> unit
    method close : unit
    method du : Var.defuse -> unit
    method int : Big_int_Z.big_int -> Type.typ -> unit
    method label : Type.label -> unit
    method typ : Type.typ -> unit
    method var : Ast.var -> unit
    method vars : Ast.var list -> unit
  end
val buf : Buffer.t
val ft : Format.formatter
val pp2string_with_pp : 'a -> ('a -> 'b -> unit) -> 'b -> string
val pp2string : (pp -> 'a -> unit) -> 'a -> string
val make_varctx : unit -> pp
val label_to_string : Type.label -> string
val ast_exp_to_string : Ast.exp -> string
val ast_stmt_to_string : Ast.stmt -> string
val ast_prog_to_string : Ast.stmt Core_kernel.List.t -> Core_kernel.String.t
