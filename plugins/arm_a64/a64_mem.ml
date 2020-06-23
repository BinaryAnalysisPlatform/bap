open Bap_core_theory
open Base
open KB.Syntax
open A64_exceptions
open A64_defs

module Env  = A64_env.Env


module Mem(Core : Theory.Core) = struct
	open Core

	module Utils = A64_utils.Utils(Core)
	open Utils

	let lift_LDRi ~rt ~rn_sp ~imm ~wback ~post_index ~is_64 = 
		let scale = const_bv64_of (if is_64 then 3 else 2) in
		let offset = match wback, post_index with
		| true, true | true, false -> signed Env.bv64_sort imm
		| flase, false -> lshift (unsigned Env.bv64_sort imm) scale
		| false, true -> raise Unexpected_Situation 
		in
		let address = if post_index then var rn_sp else add (var rn_sp) offset in
		let get_eff ~assem_reg_sort = loadw assem_reg_sort b0 (var Env.memory) address |> unsigned Env.bv64_sort |> set rt in
		let eff1 = if is_64 then get_eff Env.bv64_sort else get_eff Env.bv32_sort in
		let eff2 = if wback then set rn_sp (if post_index then add address offset else address) else pass in
		seq eff1 eff2

		let lift_LDRi_adapter ~rt ~rn_sp ~imm ~wback ~post_index ~is_64 =
			lift_LDRi ~rt:(load_register rt) 
			~rn_sp:(load_register rn_sp) 
			~imm:(load_immediate imm) 
			~wback:wback ~post_index:post_index ~is_64:is_64
(*
	let lift_mem_insn_single ?(sign = false) ~dest ~src1 ?(src2 = None) ~op ~size =
		let open Defs in
		let open Utils in
		let extend = if sign then signed else unsigned in
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
*)
end