(** Recursive Descent Disassembler  *)

open Core_kernel[@@warning "-D"]
open Bap_types.Std
open Graphlib.Std
open Bap_knowledge
open Image_internal_std
open Bap_disasm_basic
open Bap_disasm_brancher
open Bap_disasm_rooter

module Driver = Bap_disasm_driver

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
  ?brancher:brancher ->
  ?rooter:rooter -> arch -> mem -> t Or_error.t

(** [run_in_scope ?brancher ?rooter arch mem] is [run] as a knowledge
    computation: the scan runs in the caller's machine, so dynamic
    scopes the caller establishes (the current theory, in particular)
    hold while the instructions reify. *)
val run_in_scope :
  ?brancher:brancher ->
  ?rooter:rooter -> arch -> mem -> t Or_error.t knowledge

val cfg : t -> Cfg.t

val errors : t -> error list

val scan : arch -> mem -> Driver.state -> Driver.state knowledge
val global_cfg : Driver.state -> Cfg.t knowledge
