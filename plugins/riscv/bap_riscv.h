//
//  disasm.h
//

#ifndef rv_disasm_h
#define rv_disasm_h

namespace riscv {

	struct disasm : decode
	{
		addr_t pc;
		inst_t inst;

		disasm() : decode(), pc(0), inst(0) {}
	};

	enum rva {
		rva_none,
		rva_abs,
		rva_pcrel
	};

	struct rvx {
		rv_op op1;
		rv_op op2;
		rva addr;
	};

	// instruction pair constraints
	const rvx rvx_constraints[] = {
		{ rv_op_lui,     rv_op_addi,     rva_abs   },
		{ rv_op_auipc,   rv_op_addi,     rva_pcrel },
		{ rv_op_auipc,   rv_op_jalr,     rva_pcrel },
		{ rv_op_auipc,   rv_op_ld,       rva_pcrel },
		{ rv_op_auipc,   rv_op_lb,       rva_pcrel },
		{ rv_op_auipc,   rv_op_lh,       rva_pcrel },
		{ rv_op_auipc,   rv_op_lw,       rva_pcrel },
		{ rv_op_auipc,   rv_op_lbu,      rva_pcrel },
		{ rv_op_auipc,   rv_op_lhu,      rva_pcrel },
		{ rv_op_auipc,   rv_op_lwu,      rva_pcrel },
		{ rv_op_auipc,   rv_op_flw,      rva_pcrel },
		{ rv_op_auipc,   rv_op_fld,      rva_pcrel },
		{ rv_op_auipc,   rv_op_sd,       rva_pcrel },
		{ rv_op_auipc,   rv_op_sb,       rva_pcrel },
		{ rv_op_auipc,   rv_op_sh,       rva_pcrel },
		{ rv_op_auipc,   rv_op_sw,       rva_pcrel },
		{ rv_op_auipc,   rv_op_fsw,      rva_pcrel },
		{ rv_op_auipc,   rv_op_fsd,      rva_pcrel },
		{ rv_op_illegal, rv_op_illegal,  rva_none  },
	};

	// instruction buffer length
	const size_t rvx_instruction_buffer_len = 16;

	// decode pc relative address
	template <typename T>
	bool decode_pcrel(T &dec, addr_t &addr, addr_t pc, addr_t pc_bias)
	{
		switch (dec.codec) {
			case rv_codec_uj:
			case rv_codec_sb:
				addr = pc - pc_bias + dec.imm;
				return true;
			default:
				return false;
		}
		return false;
	}

	// decode address using instruction pair constraints
	template <typename T>
	bool decode_pairs(T &dec, addr_t &addr, std::deque<T> &dec_hist, addr_t pc_bias)
	{
		const rvx* rvxi = rvx_constraints;
		while(rvxi->addr != rva_none) {
			if (rvxi->op2 == dec.op) {
				for (auto li = dec_hist.rbegin(); li != dec_hist.rend(); li++) {
					if (rvxi->op1 != li->op && dec.rs1 == li->rd) break; // break: another primitive encountered
					if (rvxi->op1 != li->op || dec.rs1 != li->rd) continue; // continue: not the right pair
					switch (rvxi->addr) {
						case rva_abs:
							addr = li->imm + dec.imm;
							return true;
						case rva_pcrel:
							addr = li->pc - pc_bias + li->imm + dec.imm;
							return true;
						case rva_none:
						default:
							continue;
					}
					break;
				}
			}
			rvxi++;
		}
		return false;
	}

	// decode address for loads and stores from the global pointer
	template <typename T>
	bool deocde_gprel(T &dec, addr_t &addr, addr_t gp)
	{
		if (!gp || dec.rs1 != rv_ireg_gp) return false;
		switch (dec.op) {
			case rv_op_addi:
			case rv_op_lb:
			case rv_op_lh:
			case rv_op_lw:
			case rv_op_ld:
			case rv_op_lbu:
			case rv_op_lhu:
			case rv_op_lwu:
			case rv_op_flw:
			case rv_op_fld:
			case rv_op_sb:
			case rv_op_sh:
			case rv_op_sw:
			case rv_op_sd:
			case rv_op_fsw:
			case rv_op_fsd:
				addr = intptr_t(gp + dec.imm);
				return true;
			default:
				break;
		}
		return false;
	}

	typedef std::function<const char*(addr_t, bool nearest)> symbol_name_fn;
	typedef std::function<const char*(const char *type)> symbol_colorize_fn;

	const char* null_symbol_lookup(addr_t, bool nearest);
	const char* null_symbol_colorize(const char *type);

	template <typename T>
	std::string disasm_inst_simple(T &dec)
	{
		std::string args;
		const char *fmt = rv_inst_format[dec.op];
		while (*fmt) {
			switch (*fmt) {
				case 'O': args += rv_inst_name_sym[dec.op]; break;
				case '(': args += "("; break;
				case ',': args += ", "; break;
				case ')': args += ")"; break;
				case '0': args += rv_ireg_name_sym[dec.rd]; break;
				case '1': args += rv_ireg_name_sym[dec.rs1]; break;
				case '2': args += rv_ireg_name_sym[dec.rs2]; break;
				case '3': args += rv_freg_name_sym[dec.rd]; break;
				case '4': args += rv_freg_name_sym[dec.rs1]; break;
				case '5': args += rv_freg_name_sym[dec.rs2]; break;
				case '6': args += rv_freg_name_sym[dec.rs3]; break;
				case '7': args += format_string("%d", dec.rs1); break;
				case 'i': args += format_string("%d", dec.imm); break;
				case 'o': args += format_string("pc %c %td",
					intptr_t(dec.imm) < 0 ? '-' : '+',
					intptr_t(dec.imm) < 0 ? -intptr_t(dec.imm) : intptr_t(dec.imm)); break;
				case 'c': {
					const char * csr_name = rv_csr_name_sym[dec.imm & 0xfff];
					if (csr_name) args += format_string("%s", csr_name);
					else args += format_string("0x%03x", dec.imm & 0xfff);
					break;
				}
				case 'r':
					switch(dec.rm) {
						case rv_rm_rne: args += "rne"; break;
						case rv_rm_rtz: args += "rtz"; break;
						case rv_rm_rdn: args += "rdn"; break;
						case rv_rm_rup: args += "rup"; break;
						case rv_rm_rmm: args += "rmm"; break;
						case rv_rm_dyn: args += "dyn"; break;
						default:           args += "inv"; break;
					}
					break;
				case 'p':
					if (dec.pred & rv_fence_i) args += "i";
					if (dec.pred & rv_fence_o) args += "o";
					if (dec.pred & rv_fence_r) args += "r";
					if (dec.pred & rv_fence_w) args += "w";
					break;
				case 's':
					if (dec.succ & rv_fence_i) args += "i";
					if (dec.succ & rv_fence_o) args += "o";
					if (dec.succ & rv_fence_r) args += "r";
					if (dec.succ & rv_fence_w) args += "w";
					break;
				case '\t': while (args.length() < 12) args += " "; break;
				case 'A': if (dec.aq) args += ".aq"; break;
				case 'R': if (dec.rl) args += ".rl"; break;
				default:
					break;
			}
			fmt++;
		}
		return args;
	}

	void disasm_inst_print(disasm &dec, std::deque<disasm> &dec_hist,
		addr_t pc, addr_t pc_bias, addr_t gp,
		symbol_name_fn symlookup = null_symbol_lookup,
		symbol_colorize_fn colorize = null_symbol_colorize);

}

#endif
