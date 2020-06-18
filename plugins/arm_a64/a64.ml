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
		match insn, ops with
		| `LDR, [r; addr] -> 
		Mem.lift_mem_insn_single ~dest:r ~src1:addr ~op:LD ~size:D
		| _ -> raise (Failure "todo")
end