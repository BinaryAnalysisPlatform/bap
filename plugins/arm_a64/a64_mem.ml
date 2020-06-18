open Bap_core_theory
open Base
open KB.Syntax

module Env  = A64_env.Env
module Defs = A64_defs
module Utils = A64_utils

module Mem(Core : Theory.Core) = struct
	open Core
	let lift_mem_insn_single ?(sign = false) ~dest ~src1 ?(src2 = None) ~op ~size =
		let open Defs in
		let open Utils in
		let dest_reg = 
			match dest with
			| `Reg r -> Env.load_register r
			| _ -> raise (Lift_Error "dest must be a register") in
		let addr = 
			match src1, src2 with
			| `Reg r, None -> var @@ Env.load_register r
			| _ -> raise (Failure "todo") in
		if is_64bit_register_op dest then begin 
			match op with
			| LD -> 
				let value = (match size with
				| D -> loadw Env.double_word_sort b0 (var Env.memory) addr 
				| _ -> raise (Failure "todo")
				) in set dest_reg value
			| ST -> raise (Failure "todo")
		end else raise (Failure "todo")
	
end