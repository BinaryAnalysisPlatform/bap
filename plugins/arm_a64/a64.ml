open Core_kernel
open Bap_core_theory
open Bap.Std
open KB.Syntax
open A64_defs

module Env  = A64_env.Env


(* maybe a wrong definition *)
type insn_t = simple_insn_t * (operand_t list)

module A64(Core: Theory.Core) = struct
	open Core

	module Mem = A64_mem.Mem(Core)

	let lift_mem_insn insn ops =
		let open Mem in
		match insn, ops with
		| `LDRi_pre, [rt; rn_sp; imm] ->
		Mem.lift_LDRi_adapter ~rt:rt ~rn_sp:rn_sp ~imm:imm ~wback:true ~post_index:false ~is_64:true
		(*
    | `LDRi_post
    | `LDRi_unoff*)
		| _ -> raise (Failure "todo")
end