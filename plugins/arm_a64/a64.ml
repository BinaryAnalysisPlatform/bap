open Core_kernel
open Bap_core_theory
open Bap.Std
open KB.Syntax

open A64_env
module Defs = A64_defs


(* maybe a wrong definition *)
type insn_t = Defs.simple_insn_t * (Defs.operand_t list) * (Defs.shift_insn_t option)

module A64(Core: Theory.Core) = struct
	open Core

	module Mem = A64_mem.Mem(Core)
	let lift_mem_insn insn ops =
		let open Defs in 
		let open Mem in
		match insn, ops with
		(*
    | `LDRi_pre, [] -> lift_LDRi ~rt=rt ~rn_sp=rn_sp ~imm=imm ~wback=true ~post_index=false ~is_64=true
    | `LDRi_post
    | `LDRi_unoff*)
		| _ -> raise (Failure "todo")
end