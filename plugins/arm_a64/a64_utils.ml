open Bap_core_theory
open A64_defs
open A64_exceptions

module Env = A64_env.Env

module Utils(Core : Theory.Core) = struct
	open Core
	
	let skip : Theory.ctrl Theory.eff = perform Theory.Effect.Sort.bot
	let pass : Theory.data Theory.eff = perform Theory.Effect.Sort.bot

	let is_64bit_register (r: registers_t) = 
		match r with
		| `X0 | `X1 | `X2 | `X3 | `X4 | `X5 | `X6 | `X7 | `X8 | `X9
		| `X10 | `X11 | `X12 | `X13 | `X14 | `X15 | `X16 | `X17 | `X18 | `X19
		| `X20 | `X21 | `X22 | `X23 | `X24 | `X25 | `X26 | `X27 | `X28 | `X29 
		| `X30 |`XZR | `SP | `PC | `ELR | `SPSR -> true
		| _ -> false
	
	
	let is_64bit_register_op (operand: operand_t) = 
		match operand with
		| `Reg r -> is_64bit_register r
		| `Imm i -> raise (Failure "todo")
	
	let const_bv64_of (n: int) = int Env.bv64_sort (QWord.int n)
	
	let load_register_raw (r: registers_t) =
		let open Env in
		match r with
		| `X0 | `W0 -> x0
		| `X1 | `W1 -> x1
		| `X2 | `W2 -> x2
		| `X3 | `W3 -> x3
		| `X4 | `W4 -> x4
		| `X5 | `W5 -> x5
		| `X6 | `W6 -> x6
		| `X7 | `W7 -> x7
		| `X8 | `W8 -> x8
		| `X9 | `W9 -> x9
		| `X10 | `W10 -> x10
		| `X11 | `W11 -> x11
		| `X12 | `W12 -> x12
		| `X13 | `W13 -> x13
		| `X14 | `W14 -> x14
		| `X15 | `W15 -> x15
		| `X16 | `W16 -> x16
		| `X17 | `W17 -> x17
		| `X18 | `W18 -> x18
		| `X19 | `W19 -> x19
		| `X20 | `W20 -> x20
		| `X21 | `W21 -> x21
		| `X22 | `W22 -> x22
		| `X23 | `W23 -> x23
		| `X24 | `W24 -> x24
		| `X25 | `W25 -> x25
		| `X26 | `W26 -> x26
		| `X27 | `W27 -> x27
		| `X28 | `W28 -> x28
		| `X29 | `W29 -> x29
		| `X30 | `W30 -> x30
		| `XZR | `WZR -> xzr
		| `SP | `WSP -> sp
		| `PC -> pc
		| `ELR -> elr
		| `SPSR -> spsr

	let load_register (r: operand_t) = 
		let open Env in
		match r with
		| `Reg rr -> load_register_raw rr
		| `Imm _ -> raise Unexpected_Situation
	
	let load_immediate (r: operand_t) = 
		let open Env in
		match r with
		| `Imm imm -> int Env.bv32_sort imm
		| `Reg _ -> raise Unexpected_Situation
		
end




  