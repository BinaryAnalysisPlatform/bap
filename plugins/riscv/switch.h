//
//  switch.h
//
//  DANGER - This is machine generated code
//

#ifndef rv_switch_h
#define rv_switch_h

/* Decode Instruction Opcode */

template <bool rv32, bool rv64, bool rv128, bool rvi, bool rvm, bool rva, bool rvs, bool rvf, bool rvd, bool rvq, bool rvc>
inline opcode_t decode_inst_op(riscv::inst_t inst)
{
	opcode_t op = rv_op_illegal;
	switch (((inst >> 0) & 0b11) /* inst[1:0] */) {
		case 0:
			// c.addi4spn c.fld c.lw c.flw c.fsd c.sw c.fsw c.ld c.sd c.lq c.sq
			switch (((inst >> 13) & 0b111) /* inst[15:13] */) {
				case 0: if (rvc) op = rv_op_c_addi4spn; break;
				case 1: if (rvc) op = rv_op_c_fld; break; // c.fld c.lq
				case 2: if (rvc) op = rv_op_c_lw; break;
				case 3: 
					if (rvc && rv32) op = rv_op_c_flw;
					else if (rvc && rv64) op = rv_op_c_ld;
					break;
				case 5: if (rvc) op = rv_op_c_fsd; break; // c.fsd c.sq
				case 6: if (rvc) op = rv_op_c_sw; break;
				case 7: 
					if (rvc && rv32) op = rv_op_c_fsw;
					else if (rvc && rv64) op = rv_op_c_sd;
					break;
			}
			break;
		case 1:
			// c.nop c.addi c.jal c.li c.addi16sp c.lui c.srli c.srai c.andi c.sub c.xor c.or ...
			switch (((inst >> 13) & 0b111) /* inst[15:13] */) {
				case 0:
					// c.nop c.addi
					switch (((inst >> 2) & 0b11111111111) /* inst[12:2] */) {
						case 0: if (rvc) op = rv_op_c_nop; break;
						default: if (rvc) op = rv_op_c_addi; break;
					}
					break;
				case 1: 
					if (rvc && rv32) op = rv_op_c_jal;
					else if (rvc && rv64) op = rv_op_c_addiw;
					break;
				case 2: if (rvc) op = rv_op_c_li; break;
				case 3:
					// c.addi16sp c.lui
					switch (((inst >> 7) & 0b11111) /* inst[11:7] */) {
						case 2: if (rvc) op = rv_op_c_addi16sp; break;
						default: if (rvc) op = rv_op_c_lui; break;
					}
					break;
				case 4:
					// c.srli c.srai c.andi c.sub c.xor c.or c.and c.subw c.addw c.srli c.srai
					switch (((inst >> 10) & 0b11) /* inst[11:10] */) {
						case 0: 
							if (rvc && rv32) op = rv_op_c_srli;
							else if (rvc && rv64) op = rv_op_c_srli;
							break;
						case 1: 
							if (rvc && rv32) op = rv_op_c_srai;
							else if (rvc && rv64) op = rv_op_c_srai;
							break;
						case 2: if (rvc) op = rv_op_c_andi; break;
						case 3:
							// c.sub c.xor c.or c.and c.subw c.addw
							switch (((inst >> 10) & 0b100) | ((inst >> 5) & 0b011) /* inst[12|6:5] */) {
								case 0: if (rvc) op = rv_op_c_sub; break;
								case 1: if (rvc) op = rv_op_c_xor; break;
								case 2: if (rvc) op = rv_op_c_or; break;
								case 3: if (rvc) op = rv_op_c_and; break;
								case 4: if (rvc) op = rv_op_c_subw; break;
								case 5: if (rvc) op = rv_op_c_addw; break;
							}
							break;
					}
					break;
				case 5: if (rvc) op = rv_op_c_j; break;
				case 6: if (rvc) op = rv_op_c_beqz; break;
				case 7: if (rvc) op = rv_op_c_bnez; break;
			}
			break;
		case 2:
			// c.slli c.fldsp c.lwsp c.flwsp c.jr c.mv c.ebreak c.jalr c.add c.fsdsp c.swsp c.fswsp ...
			switch (((inst >> 13) & 0b111) /* inst[15:13] */) {
				case 0: 
					if (rvc && rv32) op = rv_op_c_slli;
					else if (rvc && rv64) op = rv_op_c_slli;
					break;
				case 1: if (rvc) op = rv_op_c_fldsp; break; // c.fldsp c.lqsp
				case 2: if (rvc) op = rv_op_c_lwsp; break;
				case 3: 
					if (rvc && rv32) op = rv_op_c_flwsp;
					else if (rvc && rv64) op = rv_op_c_ldsp;
					break;
				case 4:
					// c.jr c.mv c.ebreak c.jalr c.add
					switch (((inst >> 12) & 0b1) /* inst[12] */) {
						case 0:
							// c.jr c.mv
							switch (((inst >> 2) & 0b11111) /* inst[6:2] */) {
								case 0: if (rvc) op = rv_op_c_jr; break;
								default: if (rvc) op = rv_op_c_mv; break;
							}
							break;
						case 1:
							// c.ebreak c.jalr c.add
							switch (((inst >> 2) & 0b11111) /* inst[6:2] */) {
								case 0:
									// c.ebreak c.jalr
									switch (((inst >> 7) & 0b11111) /* inst[11:7] */) {
										case 0: if (rvc) op = rv_op_c_ebreak; break;
										default: if (rvc) op = rv_op_c_jalr; break;
									}
									break;
								default: if (rvc) op = rv_op_c_add; break;
							}
							break;
					}
					break;
				case 5: if (rvc) op = rv_op_c_fsdsp; break; // c.fsdsp c.sqsp
				case 6: if (rvc) op = rv_op_c_swsp; break;
				case 7: 
					if (rvc && rv32) op = rv_op_c_fswsp;
					else if (rvc && rv64) op = rv_op_c_sdsp;
					break;
			}
			break;
		case 3:
			// lui auipc jal jalr beq bne blt bge bltu bgeu lb lh ...
			switch (((inst >> 2) & 0b11111) /* inst[6:2] */) {
				case 0:
					// lb lh lw lbu lhu lwu ld ldu
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0: if (rvi) op = rv_op_lb; break;
						case 1: if (rvi) op = rv_op_lh; break;
						case 2: if (rvi) op = rv_op_lw; break;
						case 3: if (rvi) op = rv_op_ld; break;
						case 4: if (rvi) op = rv_op_lbu; break;
						case 5: if (rvi) op = rv_op_lhu; break;
						case 6: if (rvi) op = rv_op_lwu; break;
						case 7: if (rvi && rv128) op = rv_op_ldu; break;
					}
					break;
				case 1:
					// flw fld flq
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 2: if (rvf) op = rv_op_flw; break;
						case 3: if (rvd) op = rv_op_fld; break;
						case 4: if (rvq) op = rv_op_flq; break;
					}
					break;
				case 3:
					// fence fence.i lq
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0: if (rvi) op = rv_op_fence; break;
						case 1: if (rvi) op = rv_op_fence_i; break;
						case 2: if (rvi && rv128) op = rv_op_lq; break;
					}
					break;
				case 4:
					// addi slti sltiu xori ori andi slli srli srai slli srli srai ...
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0: if (rvi) op = rv_op_addi; break;
						case 1:
							// slli slli slli
							switch (((inst >> 27) & 0b11111) /* inst[31:27] */) {
								case 0: 
									if (rvi && rv32) op = rv_op_slli;
									else if (rvi && rv64) op = rv_op_slli;
									else if (rvi && rv128) op = rv_op_slli;
									break;
							}
							break;
						case 2: if (rvi) op = rv_op_slti; break;
						case 3: if (rvi) op = rv_op_sltiu; break;
						case 4: if (rvi) op = rv_op_xori; break;
						case 5:
							// srli srai srli srai srli srai
							switch (((inst >> 27) & 0b11111) /* inst[31:27] */) {
								case 0: 
									if (rvi && rv32) op = rv_op_srli;
									else if (rvi && rv64) op = rv_op_srli;
									else if (rvi && rv128) op = rv_op_srli;
									break;
								case 8: 
									if (rvi && rv32) op = rv_op_srai;
									else if (rvi && rv64) op = rv_op_srai;
									else if (rvi && rv128) op = rv_op_srai;
									break;
							}
							break;
						case 6: if (rvi) op = rv_op_ori; break;
						case 7: if (rvi) op = rv_op_andi; break;
					}
					break;
				case 5: if (rvi) op = rv_op_auipc; break;
				case 6:
					// addiw slliw srliw sraiw
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0: if (rvi) op = rv_op_addiw; break;
						case 1:
							// slliw
							switch (((inst >> 25) & 0b1111111) /* inst[31:25] */) {
								case 0: if (rvi) op = rv_op_slliw; break;
							}
							break;
						case 5:
							// srliw sraiw
							switch (((inst >> 25) & 0b1111111) /* inst[31:25] */) {
								case 0: if (rvi) op = rv_op_srliw; break;
								case 32: if (rvi) op = rv_op_sraiw; break;
							}
							break;
					}
					break;
				case 8:
					// sb sh sw sd sq
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0: if (rvi) op = rv_op_sb; break;
						case 1: if (rvi) op = rv_op_sh; break;
						case 2: if (rvi) op = rv_op_sw; break;
						case 3: if (rvi) op = rv_op_sd; break;
						case 4: if (rvi && rv128) op = rv_op_sq; break;
					}
					break;
				case 9:
					// fsw fsd fsq
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 2: if (rvf) op = rv_op_fsw; break;
						case 3: if (rvd) op = rv_op_fsd; break;
						case 4: if (rvq) op = rv_op_fsq; break;
					}
					break;
				case 11:
					// lr.w sc.w amoswap.w amoadd.w amoxor.w amoor.w amoand.w amomin.w amomax.w amominu.w amomaxu.w lr.d ...
					switch (((inst >> 24) & 0b11111000) | ((inst >> 12) & 0b00000111) /* inst[31:27|14:12] */) {
						case 2: if (rva) op = rv_op_amoadd_w; break;
						case 3: if (rva) op = rv_op_amoadd_d; break;
						case 4: if (rva && rv128) op = rv_op_amoadd_q; break;
						case 10: if (rva) op = rv_op_amoswap_w; break;
						case 11: if (rva) op = rv_op_amoswap_d; break;
						case 12: if (rva && rv128) op = rv_op_amoswap_q; break;
						case 18:
							// lr.w
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rva) op = rv_op_lr_w; break;
							}
							break;
						case 19:
							// lr.d
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rva) op = rv_op_lr_d; break;
							}
							break;
						case 20:
							// lr.q
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rva && rv128) op = rv_op_lr_q; break;
							}
							break;
						case 26: if (rva) op = rv_op_sc_w; break;
						case 27: if (rva) op = rv_op_sc_d; break;
						case 28: if (rva && rv128) op = rv_op_sc_q; break;
						case 34: if (rva) op = rv_op_amoxor_w; break;
						case 35: if (rva) op = rv_op_amoxor_d; break;
						case 36: if (rva && rv128) op = rv_op_amoxor_q; break;
						case 66: if (rva) op = rv_op_amoor_w; break;
						case 67: if (rva) op = rv_op_amoor_d; break;
						case 68: if (rva && rv128) op = rv_op_amoor_q; break;
						case 98: if (rva) op = rv_op_amoand_w; break;
						case 99: if (rva) op = rv_op_amoand_d; break;
						case 100: if (rva && rv128) op = rv_op_amoand_q; break;
						case 130: if (rva) op = rv_op_amomin_w; break;
						case 131: if (rva) op = rv_op_amomin_d; break;
						case 132: if (rva && rv128) op = rv_op_amomin_q; break;
						case 162: if (rva) op = rv_op_amomax_w; break;
						case 163: if (rva) op = rv_op_amomax_d; break;
						case 164: if (rva && rv128) op = rv_op_amomax_q; break;
						case 194: if (rva) op = rv_op_amominu_w; break;
						case 195: if (rva) op = rv_op_amominu_d; break;
						case 196: if (rva && rv128) op = rv_op_amominu_q; break;
						case 226: if (rva) op = rv_op_amomaxu_w; break;
						case 227: if (rva) op = rv_op_amomaxu_d; break;
						case 228: if (rva && rv128) op = rv_op_amomaxu_q; break;
					}
					break;
				case 12:
					// add sub sll slt sltu xor srl sra or and mul mulh ...
					switch (((inst >> 22) & 0b1111111000) | ((inst >> 12) & 0b0000000111) /* inst[31:25|14:12] */) {
						case 0: if (rvi) op = rv_op_add; break;
						case 1: if (rvi) op = rv_op_sll; break;
						case 2: if (rvi) op = rv_op_slt; break;
						case 3: if (rvi) op = rv_op_sltu; break;
						case 4: if (rvi) op = rv_op_xor; break;
						case 5: if (rvi) op = rv_op_srl; break;
						case 6: if (rvi) op = rv_op_or; break;
						case 7: if (rvi) op = rv_op_and; break;
						case 8: if (rvm) op = rv_op_mul; break;
						case 9: if (rvm) op = rv_op_mulh; break;
						case 10: if (rvm) op = rv_op_mulhsu; break;
						case 11: if (rvm) op = rv_op_mulhu; break;
						case 12: if (rvm) op = rv_op_div; break;
						case 13: if (rvm) op = rv_op_divu; break;
						case 14: if (rvm) op = rv_op_rem; break;
						case 15: if (rvm) op = rv_op_remu; break;
						case 256: if (rvi) op = rv_op_sub; break;
						case 261: if (rvi) op = rv_op_sra; break;
					}
					break;
				case 13: if (rvi) op = rv_op_lui; break;
				case 14:
					// addw subw sllw srlw sraw mulw divw divuw remw remuw
					switch (((inst >> 22) & 0b1111111000) | ((inst >> 12) & 0b0000000111) /* inst[31:25|14:12] */) {
						case 0: if (rvi) op = rv_op_addw; break;
						case 1: if (rvi) op = rv_op_sllw; break;
						case 5: if (rvi) op = rv_op_srlw; break;
						case 8: if (rvm) op = rv_op_mulw; break;
						case 12: if (rvm) op = rv_op_divw; break;
						case 13: if (rvm) op = rv_op_divuw; break;
						case 14: if (rvm) op = rv_op_remw; break;
						case 15: if (rvm) op = rv_op_remuw; break;
						case 256: if (rvi) op = rv_op_subw; break;
						case 261: if (rvi) op = rv_op_sraw; break;
					}
					break;
				case 16:
					// fmadd.s fmadd.d fmadd.q
					switch (((inst >> 25) & 0b11) /* inst[26:25] */) {
						case 0: if (rvf) op = rv_op_fmadd_s; break;
						case 1: if (rvd) op = rv_op_fmadd_d; break;
						case 3: if (rvq) op = rv_op_fmadd_q; break;
					}
					break;
				case 17:
					// fmsub.s fmsub.d fmsub.q
					switch (((inst >> 25) & 0b11) /* inst[26:25] */) {
						case 0: if (rvf) op = rv_op_fmsub_s; break;
						case 1: if (rvd) op = rv_op_fmsub_d; break;
						case 3: if (rvq) op = rv_op_fmsub_q; break;
					}
					break;
				case 18:
					// fnmsub.s fnmsub.d fnmsub.q
					switch (((inst >> 25) & 0b11) /* inst[26:25] */) {
						case 0: if (rvf) op = rv_op_fnmsub_s; break;
						case 1: if (rvd) op = rv_op_fnmsub_d; break;
						case 3: if (rvq) op = rv_op_fnmsub_q; break;
					}
					break;
				case 19:
					// fnmadd.s fnmadd.d fnmadd.q
					switch (((inst >> 25) & 0b11) /* inst[26:25] */) {
						case 0: if (rvf) op = rv_op_fnmadd_s; break;
						case 1: if (rvd) op = rv_op_fnmadd_d; break;
						case 3: if (rvq) op = rv_op_fnmadd_q; break;
					}
					break;
				case 20:
					// fadd.s fsub.s fmul.s fdiv.s fsgnj.s fsgnjn.s fsgnjx.s fmin.s fmax.s fsqrt.s fle.s flt.s ...
					switch (((inst >> 25) & 0b1111111) /* inst[31:25] */) {
						case 0: if (rvf) op = rv_op_fadd_s; break;
						case 1: if (rvd) op = rv_op_fadd_d; break;
						case 3: if (rvq) op = rv_op_fadd_q; break;
						case 4: if (rvf) op = rv_op_fsub_s; break;
						case 5: if (rvd) op = rv_op_fsub_d; break;
						case 7: if (rvq) op = rv_op_fsub_q; break;
						case 8: if (rvf) op = rv_op_fmul_s; break;
						case 9: if (rvd) op = rv_op_fmul_d; break;
						case 11: if (rvq) op = rv_op_fmul_q; break;
						case 12: if (rvf) op = rv_op_fdiv_s; break;
						case 13: if (rvd) op = rv_op_fdiv_d; break;
						case 15: if (rvq) op = rv_op_fdiv_q; break;
						case 16:
							// fsgnj.s fsgnjn.s fsgnjx.s
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvf) op = rv_op_fsgnj_s; break;
								case 1: if (rvf) op = rv_op_fsgnjn_s; break;
								case 2: if (rvf) op = rv_op_fsgnjx_s; break;
							}
							break;
						case 17:
							// fsgnj.d fsgnjn.d fsgnjx.d
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvd) op = rv_op_fsgnj_d; break;
								case 1: if (rvd) op = rv_op_fsgnjn_d; break;
								case 2: if (rvd) op = rv_op_fsgnjx_d; break;
							}
							break;
						case 19:
							// fsgnj.q fsgnjn.q fsgnjx.q
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvq) op = rv_op_fsgnj_q; break;
								case 1: if (rvq) op = rv_op_fsgnjn_q; break;
								case 2: if (rvq) op = rv_op_fsgnjx_q; break;
							}
							break;
						case 20:
							// fmin.s fmax.s
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvf) op = rv_op_fmin_s; break;
								case 1: if (rvf) op = rv_op_fmax_s; break;
							}
							break;
						case 21:
							// fmin.d fmax.d
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvd) op = rv_op_fmin_d; break;
								case 1: if (rvd) op = rv_op_fmax_d; break;
							}
							break;
						case 23:
							// fmin.q fmax.q
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvq) op = rv_op_fmin_q; break;
								case 1: if (rvq) op = rv_op_fmax_q; break;
							}
							break;
						case 32:
							// fcvt.s.d fcvt.s.q
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 1: if (rvd) op = rv_op_fcvt_s_d; break;
								case 3: if (rvq) op = rv_op_fcvt_s_q; break;
							}
							break;
						case 33:
							// fcvt.d.s fcvt.d.q
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvd) op = rv_op_fcvt_d_s; break;
								case 3: if (rvq) op = rv_op_fcvt_d_q; break;
							}
							break;
						case 35:
							// fcvt.q.s fcvt.q.d
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvq) op = rv_op_fcvt_q_s; break;
								case 1: if (rvq) op = rv_op_fcvt_q_d; break;
							}
							break;
						case 44:
							// fsqrt.s
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvf) op = rv_op_fsqrt_s; break;
							}
							break;
						case 45:
							// fsqrt.d
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvd) op = rv_op_fsqrt_d; break;
							}
							break;
						case 47:
							// fsqrt.q
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvq) op = rv_op_fsqrt_q; break;
							}
							break;
						case 80:
							// fle.s flt.s feq.s
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvf) op = rv_op_fle_s; break;
								case 1: if (rvf) op = rv_op_flt_s; break;
								case 2: if (rvf) op = rv_op_feq_s; break;
							}
							break;
						case 81:
							// fle.d flt.d feq.d
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvd) op = rv_op_fle_d; break;
								case 1: if (rvd) op = rv_op_flt_d; break;
								case 2: if (rvd) op = rv_op_feq_d; break;
							}
							break;
						case 83:
							// fle.q flt.q feq.q
							switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
								case 0: if (rvq) op = rv_op_fle_q; break;
								case 1: if (rvq) op = rv_op_flt_q; break;
								case 2: if (rvq) op = rv_op_feq_q; break;
							}
							break;
						case 96:
							// fcvt.w.s fcvt.wu.s fcvt.l.s fcvt.lu.s
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvf) op = rv_op_fcvt_w_s; break;
								case 1: if (rvf) op = rv_op_fcvt_wu_s; break;
								case 2: if (rvf) op = rv_op_fcvt_l_s; break;
								case 3: if (rvf) op = rv_op_fcvt_lu_s; break;
							}
							break;
						case 97:
							// fcvt.w.d fcvt.wu.d fcvt.l.d fcvt.lu.d
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvd) op = rv_op_fcvt_w_d; break;
								case 1: if (rvd) op = rv_op_fcvt_wu_d; break;
								case 2: if (rvd) op = rv_op_fcvt_l_d; break;
								case 3: if (rvd) op = rv_op_fcvt_lu_d; break;
							}
							break;
						case 99:
							// fcvt.w.q fcvt.wu.q fcvt.l.q fcvt.lu.q
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvq) op = rv_op_fcvt_w_q; break;
								case 1: if (rvq) op = rv_op_fcvt_wu_q; break;
								case 2: if (rvq) op = rv_op_fcvt_l_q; break;
								case 3: if (rvq) op = rv_op_fcvt_lu_q; break;
							}
							break;
						case 104:
							// fcvt.s.w fcvt.s.wu fcvt.s.l fcvt.s.lu
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvf) op = rv_op_fcvt_s_w; break;
								case 1: if (rvf) op = rv_op_fcvt_s_wu; break;
								case 2: if (rvf) op = rv_op_fcvt_s_l; break;
								case 3: if (rvf) op = rv_op_fcvt_s_lu; break;
							}
							break;
						case 105:
							// fcvt.d.w fcvt.d.wu fcvt.d.l fcvt.d.lu
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvd) op = rv_op_fcvt_d_w; break;
								case 1: if (rvd) op = rv_op_fcvt_d_wu; break;
								case 2: if (rvd) op = rv_op_fcvt_d_l; break;
								case 3: if (rvd) op = rv_op_fcvt_d_lu; break;
							}
							break;
						case 107:
							// fcvt.q.w fcvt.q.wu fcvt.q.l fcvt.q.lu
							switch (((inst >> 20) & 0b11111) /* inst[24:20] */) {
								case 0: if (rvq) op = rv_op_fcvt_q_w; break;
								case 1: if (rvq) op = rv_op_fcvt_q_wu; break;
								case 2: if (rvq) op = rv_op_fcvt_q_l; break;
								case 3: if (rvq) op = rv_op_fcvt_q_lu; break;
							}
							break;
						case 112:
							// fmv.x.s fclass.s
							switch (((inst >> 17) & 0b11111000) | ((inst >> 12) & 0b00000111) /* inst[24:20|14:12] */) {
								case 0: if (rvf) op = rv_op_fmv_x_s; break;
								case 1: if (rvf) op = rv_op_fclass_s; break;
							}
							break;
						case 113:
							// fclass.d fmv.x.d
							switch (((inst >> 17) & 0b11111000) | ((inst >> 12) & 0b00000111) /* inst[24:20|14:12] */) {
								case 0: if (rvd) op = rv_op_fmv_x_d; break;
								case 1: if (rvd) op = rv_op_fclass_d; break;
							}
							break;
						case 115:
							// fclass.q fmv.x.q
							switch (((inst >> 17) & 0b11111000) | ((inst >> 12) & 0b00000111) /* inst[24:20|14:12] */) {
								case 0: if (rvq) op = rv_op_fmv_x_q; break;
								case 1: if (rvq) op = rv_op_fclass_q; break;
							}
							break;
						case 120:
							// fmv.s.x
							switch (((inst >> 17) & 0b11111000) | ((inst >> 12) & 0b00000111) /* inst[24:20|14:12] */) {
								case 0: if (rvf) op = rv_op_fmv_s_x; break;
							}
							break;
						case 121:
							// fmv.d.x
							switch (((inst >> 17) & 0b11111000) | ((inst >> 12) & 0b00000111) /* inst[24:20|14:12] */) {
								case 0: if (rvd) op = rv_op_fmv_d_x; break;
							}
							break;
						case 123:
							// fmv.q.x
							switch (((inst >> 17) & 0b11111000) | ((inst >> 12) & 0b00000111) /* inst[24:20|14:12] */) {
								case 0: if (rvq) op = rv_op_fmv_q_x; break;
							}
							break;
					}
					break;
				case 22:
					// addid sllid srlid sraid
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0: if (rvi && rv128) op = rv_op_addid; break;
						case 1:
							// sllid
							switch (((inst >> 26) & 0b111111) /* inst[31:26] */) {
								case 0: if (rvi && rv128) op = rv_op_sllid; break;
							}
							break;
						case 5:
							// srlid sraid
							switch (((inst >> 26) & 0b111111) /* inst[31:26] */) {
								case 0: if (rvi && rv128) op = rv_op_srlid; break;
								case 16: if (rvi && rv128) op = rv_op_sraid; break;
							}
							break;
					}
					break;
				case 24:
					// beq bne blt bge bltu bgeu
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0: if (rvi) op = rv_op_beq; break;
						case 1: if (rvi) op = rv_op_bne; break;
						case 4: if (rvi) op = rv_op_blt; break;
						case 5: if (rvi) op = rv_op_bge; break;
						case 6: if (rvi) op = rv_op_bltu; break;
						case 7: if (rvi) op = rv_op_bgeu; break;
					}
					break;
				case 25:
					// jalr
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0: if (rvi) op = rv_op_jalr; break;
					}
					break;
				case 27: if (rvi) op = rv_op_jal; break;
				case 28:
					// ecall ebreak uret sret hret mret dret sfence.vm wfi csrrw csrrs csrrc ...
					switch (((inst >> 12) & 0b111) /* inst[14:12] */) {
						case 0:
							// ecall ebreak uret sret hret mret dret sfence.vm wfi
							switch (((inst >> 15) & 0b11111111111100000) | ((inst >> 7) & 0b00000000000011111) /* inst[31:20|11:7] */) {
								case 0:
									// ecall
									switch (((inst >> 15) & 0b11111) /* inst[19:15] */) {
										case 0: if (rvs) op = rv_op_ecall; break;
									}
									break;
								case 32:
									// ebreak
									switch (((inst >> 15) & 0b11111) /* inst[19:15] */) {
										case 0: if (rvs) op = rv_op_ebreak; break;
									}
									break;
								case 64:
									// uret
									switch (((inst >> 15) & 0b11111) /* inst[19:15] */) {
										case 0: if (rvs) op = rv_op_uret; break;
									}
									break;
								case 8256:
									// sret
									switch (((inst >> 15) & 0b11111) /* inst[19:15] */) {
										case 0: if (rvs) op = rv_op_sret; break;
									}
									break;
								case 8320: if (rvs) op = rv_op_sfence_vm; break;
								case 8352:
									// wfi
									switch (((inst >> 15) & 0b11111) /* inst[19:15] */) {
										case 0: if (rvs) op = rv_op_wfi; break;
									}
									break;
								case 16448:
									// hret
									switch (((inst >> 15) & 0b11111) /* inst[19:15] */) {
										case 0: if (rvs) op = rv_op_hret; break;
									}
									break;
								case 24640:
									// mret
									switch (((inst >> 15) & 0b11111) /* inst[19:15] */) {
										case 0: if (rvs) op = rv_op_mret; break;
									}
									break;
								case 63040:
									// dret
									switch (((inst >> 15) & 0b11111) /* inst[19:15] */) {
										case 0: if (rvs) op = rv_op_dret; break;
									}
									break;
							}
							break;
						case 1: if (rvs) op = rv_op_csrrw; break;
						case 2: if (rvs) op = rv_op_csrrs; break;
						case 3: if (rvs) op = rv_op_csrrc; break;
						case 5: if (rvs) op = rv_op_csrrwi; break;
						case 6: if (rvs) op = rv_op_csrrsi; break;
						case 7: if (rvs) op = rv_op_csrrci; break;
					}
					break;
				case 30:
					// addd subd slld srld srad muld divd divud remd remud
					switch (((inst >> 22) & 0b1111111000) | ((inst >> 12) & 0b0000000111) /* inst[31:25|14:12] */) {
						case 0: if (rvi && rv128) op = rv_op_addd; break;
						case 1: if (rvi && rv128) op = rv_op_slld; break;
						case 5: if (rvi && rv128) op = rv_op_srld; break;
						case 8: if (rvm && rv128) op = rv_op_muld; break;
						case 12: if (rvm && rv128) op = rv_op_divd; break;
						case 13: if (rvm && rv128) op = rv_op_divud; break;
						case 14: if (rvm && rv128) op = rv_op_remd; break;
						case 15: if (rvm && rv128) op = rv_op_remud; break;
						case 256: if (rvi && rv128) op = rv_op_subd; break;
						case 261: if (rvi && rv128) op = rv_op_srad; break;
					}
					break;
			}
			break;
	}
	return op;
}

/* Decode Instruction Type */

template <typename T>
inline void decode_inst_type(T &dec, riscv::inst_t inst)
{
	dec.codec = rv_inst_codec[dec.op];
	switch (dec.codec) {
		case rv_codec_none:             riscv::decode_none(dec, inst);                     break;
		case rv_codec_u:                riscv::decode_u(dec, inst);                        break;
		case rv_codec_uj:               riscv::decode_uj(dec, inst);                       break;
		case rv_codec_i:                riscv::decode_i(dec, inst);                        break;
		case rv_codec_i_sh5:            riscv::decode_i_sh5(dec, inst);                    break;
		case rv_codec_i_sh6:            riscv::decode_i_sh6(dec, inst);                    break;
		case rv_codec_i_sh7:            riscv::decode_i_sh7(dec, inst);                    break;
		case rv_codec_i_csr:            riscv::decode_i_csr(dec, inst);                    break;
		case rv_codec_s:                riscv::decode_s(dec, inst);                        break;
		case rv_codec_sb:               riscv::decode_sb(dec, inst);                       break;
		case rv_codec_r:                riscv::decode_r(dec, inst);                        break;
		case rv_codec_r_m:              riscv::decode_r_m(dec, inst);                      break;
		case rv_codec_r4_m:             riscv::decode_r4_m(dec, inst);                     break;
		case rv_codec_r_a:              riscv::decode_r_a(dec, inst);                      break;
		case rv_codec_r_l:              riscv::decode_r_l(dec, inst);                      break;
		case rv_codec_r_f:              riscv::decode_r_f(dec, inst);                      break;
		case rv_codec_cb:               riscv::decode_cb(dec, inst);                       break;
		case rv_codec_cb_imm:           riscv::decode_cb_imm(dec, inst);                   break;
		case rv_codec_cb_sh5:           riscv::decode_cb_sh5(dec, inst);                   break;
		case rv_codec_cb_sh6:           riscv::decode_cb_sh6(dec, inst);                   break;
		case rv_codec_ci:               riscv::decode_ci(dec, inst);                       break;
		case rv_codec_ci_sh5:           riscv::decode_ci_sh5(dec, inst);                   break;
		case rv_codec_ci_sh6:           riscv::decode_ci_sh6(dec, inst);                   break;
		case rv_codec_ci_16sp:          riscv::decode_ci_16sp(dec, inst);                  break;
		case rv_codec_ci_lwsp:          riscv::decode_ci_lwsp(dec, inst);                  break;
		case rv_codec_ci_ldsp:          riscv::decode_ci_ldsp(dec, inst);                  break;
		case rv_codec_ci_lqsp:          riscv::decode_ci_lqsp(dec, inst);                  break;
		case rv_codec_ci_li:            riscv::decode_ci_li(dec, inst);                    break;
		case rv_codec_ci_lui:           riscv::decode_ci_lui(dec, inst);                   break;
		case rv_codec_ci_none:          riscv::decode_ci_none(dec, inst);                  break;
		case rv_codec_ciw_4spn:         riscv::decode_ciw_4spn(dec, inst);                 break;
		case rv_codec_cj:               riscv::decode_cj(dec, inst);                       break;
		case rv_codec_cj_jal:           riscv::decode_cj_jal(dec, inst);                   break;
		case rv_codec_cl_lw:            riscv::decode_cl_lw(dec, inst);                    break;
		case rv_codec_cl_ld:            riscv::decode_cl_ld(dec, inst);                    break;
		case rv_codec_cl_lq:            riscv::decode_cl_lq(dec, inst);                    break;
		case rv_codec_cr:               riscv::decode_cr(dec, inst);                       break;
		case rv_codec_cr_mv:            riscv::decode_cr_mv(dec, inst);                    break;
		case rv_codec_cr_jalr:          riscv::decode_cr_jalr(dec, inst);                  break;
		case rv_codec_cr_jr:            riscv::decode_cr_jr(dec, inst);                    break;
		case rv_codec_cs:               riscv::decode_cs(dec, inst);                       break;
		case rv_codec_cs_sw:            riscv::decode_cs_sw(dec, inst);                    break;
		case rv_codec_cs_sd:            riscv::decode_cs_sd(dec, inst);                    break;
		case rv_codec_cs_sq:            riscv::decode_cs_sq(dec, inst);                    break;
		case rv_codec_css_swsp:         riscv::decode_css_swsp(dec, inst);                 break;
		case rv_codec_css_sdsp:         riscv::decode_css_sdsp(dec, inst);                 break;
		case rv_codec_css_sqsp:         riscv::decode_css_sqsp(dec, inst);                 break;
	};
}

/* Encode Instruction */

template <typename T>
inline riscv::inst_t encode_inst(T &dec)
{
	dec.codec = rv_inst_codec[dec.op];
	riscv::inst_t inst = rv_inst_match[dec.op];
	switch (dec.codec) {
		case rv_codec_none:             return inst |= riscv::encode_none(dec);            break;
		case rv_codec_u:                return inst |= riscv::encode_u(dec);               break;
		case rv_codec_uj:               return inst |= riscv::encode_uj(dec);              break;
		case rv_codec_i:                return inst |= riscv::encode_i(dec);               break;
		case rv_codec_i_sh5:            return inst |= riscv::encode_i_sh5(dec);           break;
		case rv_codec_i_sh6:            return inst |= riscv::encode_i_sh6(dec);           break;
		case rv_codec_i_sh7:            return inst |= riscv::encode_i_sh7(dec);           break;
		case rv_codec_i_csr:            return inst |= riscv::encode_i_csr(dec);           break;
		case rv_codec_s:                return inst |= riscv::encode_s(dec);               break;
		case rv_codec_sb:               return inst |= riscv::encode_sb(dec);              break;
		case rv_codec_r:                return inst |= riscv::encode_r(dec);               break;
		case rv_codec_r_m:              return inst |= riscv::encode_r_m(dec);             break;
		case rv_codec_r4_m:             return inst |= riscv::encode_r4_m(dec);            break;
		case rv_codec_r_a:              return inst |= riscv::encode_r_a(dec);             break;
		case rv_codec_r_l:              return inst |= riscv::encode_r_l(dec);             break;
		case rv_codec_r_f:              return inst |= riscv::encode_r_f(dec);             break;
		case rv_codec_cb:               return inst |= riscv::encode_cb(dec);              break;
		case rv_codec_cb_imm:           return inst |= riscv::encode_cb_imm(dec);          break;
		case rv_codec_cb_sh5:           return inst |= riscv::encode_cb_sh5(dec);          break;
		case rv_codec_cb_sh6:           return inst |= riscv::encode_cb_sh6(dec);          break;
		case rv_codec_ci:               return inst |= riscv::encode_ci(dec);              break;
		case rv_codec_ci_sh5:           return inst |= riscv::encode_ci_sh5(dec);          break;
		case rv_codec_ci_sh6:           return inst |= riscv::encode_ci_sh6(dec);          break;
		case rv_codec_ci_16sp:          return inst |= riscv::encode_ci_16sp(dec);         break;
		case rv_codec_ci_lwsp:          return inst |= riscv::encode_ci_lwsp(dec);         break;
		case rv_codec_ci_ldsp:          return inst |= riscv::encode_ci_ldsp(dec);         break;
		case rv_codec_ci_lqsp:          return inst |= riscv::encode_ci_lqsp(dec);         break;
		case rv_codec_ci_li:            return inst |= riscv::encode_ci_li(dec);           break;
		case rv_codec_ci_lui:           return inst |= riscv::encode_ci_lui(dec);          break;
		case rv_codec_ci_none:          return inst |= riscv::encode_ci_none(dec);         break;
		case rv_codec_ciw_4spn:         return inst |= riscv::encode_ciw_4spn(dec);        break;
		case rv_codec_cj:               return inst |= riscv::encode_cj(dec);              break;
		case rv_codec_cj_jal:           return inst |= riscv::encode_cj_jal(dec);          break;
		case rv_codec_cl_lw:            return inst |= riscv::encode_cl_lw(dec);           break;
		case rv_codec_cl_ld:            return inst |= riscv::encode_cl_ld(dec);           break;
		case rv_codec_cl_lq:            return inst |= riscv::encode_cl_lq(dec);           break;
		case rv_codec_cr:               return inst |= riscv::encode_cr(dec);              break;
		case rv_codec_cr_mv:            return inst |= riscv::encode_cr_mv(dec);           break;
		case rv_codec_cr_jalr:          return inst |= riscv::encode_cr_jalr(dec);         break;
		case rv_codec_cr_jr:            return inst |= riscv::encode_cr_jr(dec);           break;
		case rv_codec_cs:               return inst |= riscv::encode_cs(dec);              break;
		case rv_codec_cs_sw:            return inst |= riscv::encode_cs_sw(dec);           break;
		case rv_codec_cs_sd:            return inst |= riscv::encode_cs_sd(dec);           break;
		case rv_codec_cs_sq:            return inst |= riscv::encode_cs_sq(dec);           break;
		case rv_codec_css_swsp:         return inst |= riscv::encode_css_swsp(dec);        break;
		case rv_codec_css_sdsp:         return inst |= riscv::encode_css_sdsp(dec);        break;
		case rv_codec_css_sqsp:         return inst |= riscv::encode_css_sqsp(dec);        break;
	};
	return inst;
}

#endif
