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

  let load_mem_to_dest_eff_raw assem_reg_sort to_load_bv_sort address rt sign = 
    let extend = if sign then signed else unsigned in
    (* unsigned/signed extend to logical reg size, then unsigned extend to physical reg size *)
    little_load_bv to_load_bv_sort address |> extend assem_reg_sort |> unsigned Env.bv64_sort |> set rt

  let load_mem_to_dest_eff is_64_reg to_load_bv address rt sign =
    if is_64_reg then 
      let f x = load_mem_to_dest_eff_raw Env.bv64_sort x address rt sign in
      match to_load_bv with
      | B -> f Env.byte_sort
      | H -> f Env.half_word_sort
      | W -> f Env.word_sort
      | X -> f Env.double_word_sort 
    else 
      let f x = load_mem_to_dest_eff_raw Env.bv32_sort x address rt sign in
      match to_load_bv with
      | B -> f Env.byte_sort
      | H -> f Env.half_word_sort
      | W -> f Env.word_sort
      | X -> f Env.double_word_sort 

  let lift_LDRi ~rt ~rn_sp ~imm ~wback ~post_index ~is_64_reg ~to_load_bv = 
    let scale = const_bv64_of (if is_64_reg then 3 else 2) in
    let offset = match wback, post_index with
      | true, true | true, false -> signed Env.bv64_sort imm
      | flase, false -> lshift (unsigned Env.bv64_sort imm) scale
      | false, true -> raise Unexpected_Situation 
    in
    let address = if post_index then var rn_sp else add (var rn_sp) offset in
    let eff1 = load_mem_to_dest_eff is_64_reg to_load_bv address rt false in
    let eff2 = if wback then set rn_sp (if post_index then add address offset else address) else pass in
    seq eff1 eff2

  let lift_LDRi_wrapper ~rt ~rn_sp ~imm ~wback ~post_index ~is_64_reg ~to_load_bv =
    lift_LDRi ~rt:(load_register rt) 
      ~rn_sp:(load_register rn_sp) 
      ~imm:(load_immediate imm) 
      ~wback:wback ~post_index:post_index ~is_64_reg:is_64_reg ~to_load_bv:to_load_bv

  let lift_LDRl ~rt ~pc ~imm ~is_64_reg ~to_load_bv ~sign =
    (* offset = SignedExtend(imm19:'00', 64) *)
    let offset = append Env.bv64_sort imm (const_0bv_of_size 2) |> signed Env.bv64_sort in
    let address = add (var pc) offset in  
    load_mem_to_dest_eff is_64_reg to_load_bv address rt sign

  let lift_LDRr ~rt ~rn_sp ~rm ~is_64_reg ~to_load_bv ~extend_type ~scaled = 
    let scale = if is_64_reg then 3 else 2 in
    let shift_len = if scaled then scale else 0 in
    let offset = extend_reg ~r:rm ~extend_type:extend_type ~is_64_reg:is_64_reg ~shift_len:shift_len in
    let address = add (var rn_sp) offset in
    load_mem_to_dest_eff is_64_reg to_load_bv address rt false


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
		if is_64_regbit_register_op dest then begin 
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