#include <cstdio>
#include <cstring>
#include <cstdarg>
#include <cassert>
#include <map>
#include <algorithm>
#include <functional>
#include <vector>
#include <deque>
#include <string>

#include "bap_riscv.h"

#include "types.h"
#include "host-endian.h"
#include "meta.h"
#include "codec.h"
#include "strings.h"
#include "disasm.h"

using namespace riscv;

const char* riscv::null_symbol_lookup(addr_t, bool nearest) { return nullptr; }
const char* riscv::null_symbol_colorize(const char *type) { return ""; }

static const bap_disasm_insn_p_type supported[] = {
    is_true,
    is_invalid,
    is_return,
    is_call,
    is_barrier,
    is_terminator,
    is_branch,
    is_indirect_branch,
    is_conditional_branch,
    is_unconditional_branch,
    may_affect_control_flow,
    may_load,
    may_store
};

class riscv_disassembler : public disassembler_interface {
	disasm dec;
	inst_t rvinst;
    insn current;
	shared_ptr<memory> mem;

	riscv_disassembler(int debug_level)
		: debug_level(debug_level), current(invalid_insn({0,0})) {}

public:
    static result<riscv_disassembler>
    create(const char *name, const char *cpu, int debug_level) {
        std::string error;

		shared_ptr<riscv_disassembler> self(new riscv_disassembler());
		// TODO: Add proper initialization
        return {self, {0} };
    }

	table insn_table() const {
		return {rv_inst_name_sym, ARRAY_SIZE(rv_inst_name_sym)};
	}

	table reg_table() const {
		return {rv_ireg_name_sym, ARRAY_SIZE(rv_ireg_name_sym)};
	}

	void set_memory(memory m) {
		mem = m;
	}

    void step(uint64_t pc) {
        auto base = mem->base;
		int pc_offset = 0;

        if (pc < base) {
            current = invalid_insn(location{0,1});
        } else if (pc > base + mem->loc.len + 1) {
            auto off = mem->loc.len - 1;
            current = invalid_insn(location{off,1});
        } else {
            uint64_t size = 0;
            int off = pc - mem->base;
            int len = mem->loc.len - off;

			if (len > 0) {
				rvinst = inst_fetch(addr, &pc_offset)
			}
			// TODO: apply PC offset here
			size = pc_offset;
			// TODO: process errors here

            location loc = {
                static_cast<int>(pc - base),
                static_cast<int>(size)
            };

            current = valid_insn(loc);
            if (debug_level > 1) {
			    std::cerr << "read: '" << get_asm() << "'\n";
            }
        }
    }

    insn get_insn() const {
        return current;
    }

	std::string get_asm() const {
		if (current.code != 0) {
			disasm_inst_simple(dec);
		} else {
			return "";
		}
	}

    // invalid instruction doesn't satisfy any predicate except is_invalid.
    bool satisfies(bap_disasm_insn_p_type p) const {
        auto current_is_invalid = current.code == 0;
        if (p == is_invalid) {
            return current_is_invalid;
        } else if (current_is_invalid) {
            return false;
        } else if (p == is_true) {
            return true;
        } else {
            auto d = ins_info->get(current.code);
            if (p == may_affect_control_flow) {
                return d.mayAffectControlFlow(mcinst, *reg_info);
            } else if (auto check = fun_of_pred(p)) {
                return check(d);
            } else {
                return false;
            }
        }
    }

    bool supports(bap_disasm_insn_p_type p) const {
        for (auto q : supported) {
            if (p == q)
                return true;
        }
        return false;
    }

private:
    insn valid_insn(location loc) const {
        insn ins;

		// read instruction here
		// loop through operands here...
		if (dec.imm) {
			// then imm operand
			op.type = bap_disasm_op_imm;
            op.imm_val = dec.imm;
			ins.ops.push_back(op);
		}
		if (dec.rd) {
			// then register operand, etc
			op.type = bap_disasm_op_reg;
            op.reg_val = {code, (int)(rv_ireg_name_sym[dec.rd] - rv_ireg_name_sym[0])};
			ins.ops.push_back(op);
		}
		if (dec.rs1) {
			// then register operand, etc
			op.type = bap_disasm_op_reg;
            op.reg_val = {code, (int)(rv_ireg_name_sym[dec.rs1] - rv_ireg_name_sym[0])};
			ins.ops.push_back(op);
		}
		if (dec.rs2) {
			// then register operand, etc
			op.type = bap_disasm_op_reg;
            op.reg_val = {code, int(rv_ireg_name_sym[dec.rs2] - rv_ireg_name_sym[0])};
			ins.ops.push_back(op);
		}
		if (dec.rs3) { // Only for floating point instructions
			// then register operand, etc
			op.type = bap_disasm_op_reg;
            op.reg_val = {code, (int)(rv_freg_name_sym[dec.rs3] - rv_freg_name_sym[0])};
			ins.ops.push_back(op);
		}
		// TODO: Add support for floating point!

		riscv::decode_inst(dec, inst);
        ins.code = dec.op;
        ins.name = rv_inst_name_sym[dec.op];
        ins.loc = loc;
        return ins;
    }

    insn invalid_insn(location loc) const {
        return {0, 0L, loc};
    }

    reg create_reg(unsigned code) const {
        return {(int)code, (int) (reg_info->getName(code - reg_tab.data)};
    }
};

struct create_riscv_disassembler : disasm_factory {
    result<disassembler_interface>
    create(const char *triple, const char *cpu, int debug_level) {
        auto riscv = riscv_disassembler::create(triple, cpu, debug_level);
        result<disassembler_interface> r;
        r.dis = riscv.dis;
        if (!r.dis)
            r.err = riscv.err;
        return r;
    }
};

}

int disasm_riscv_init() {
    auto f = std::make_shared<bap::create_riscv_disassembler>();
    return bap::register_disassembler("riscv", f);
}
