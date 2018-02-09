(** Recursive Descent Disassembler  *)

open Core_kernel.Std
open Bap_types.Std
open Graphlib.Std

open Image_internal_std
open Bap_disasm_basic
open Bap_disasm_brancher
open Bap_disasm_rooter

type t

type insn = Bap_disasm_insn.t
type block = Bap_disasm_block.t

type cfg [@@deriving compare]
module Cfg : Graph with type t = cfg
                    and type node = block
                    and type Edge.label = Bap_disasm_block.edge


type error = [
  | `Failed_to_disasm of mem
  | `Failed_to_lift of mem * full_insn * Error.t
] [@@deriving sexp_of]

val run :
  ?backend:string ->
  ?cpu:string ->
  ?brancher:brancher ->
  ?rooter:rooter -> arch -> mem -> t Or_error.t

val cfg : t -> Cfg.t

val errors : t -> error list
