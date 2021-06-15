#include <ghidra/loadimage.hh>
#include <ghidra/sleigh.hh>
#include <ghidra/emulate.hh>

#include "ghidra_disasm.hpp"

#include "disasm.hpp"

#include <iostream>
#include <memory>
#include <algorithm>
#include <map>
#include <vector>

class Loader : public LoadImage {
    bap::memory mem;
public:
    Loader() : LoadImage("nofile"), mem() {}

    explicit Loader(const bap::memory &mem) : LoadImage("nofile"), mem(mem) {}

    virtual void loadFill(uint1 *dst, int4 len, const Address &addr) {
        int4 off = offset(addr);
        if (off < mem.loc.len && off + len <= mem.loc.len) {
            std::copy_n(mem.data + off, len, dst);
        } else {
            std::fill_n(dst, len, 0);
            if (off < mem.loc.len) {
                std::copy(mem.data + off, mem.data + mem.loc.len, dst);
            }
        }
    }

    virtual void adjustVma(long adjust) {
        mem.base += adjust;
    }

    virtual std::string getArchType() const {return "bap";};

    void set_memory(const bap::memory &next_mem) {
        mem = next_mem;
    }

    int offset(const Address &addr) const {
        return addr.getOffset() - mem.base + mem.loc.off;
    }

    int address(const uint64_t offset) const {
        return mem.base + (offset - mem.loc.off);
    }

    bool is_mapped(uint64_t addr) const {
        return addr >= mem.base && addr - mem.base < mem.loc.len;
    }
};

bap::table table_from_string(const std::string &data) {
    bap::table res;
    res.data = data.c_str();
    res.size = data.length();
    return res;
}

class OpcodesTable {
    std::string opnames;
    std::map<OpCode,int> offsets;
public:
    OpcodesTable() {
        std::stringstream ss;
        int offset = 0;
        for (int i = 0; i < CPUI_MAX; i++) {
            OpCode op = static_cast<OpCode>(i);
            std::string name = get_opname(op);
            ss << name << '\000';
            offsets[op] = offset;
            offset += name.length() + 1;
        }
        opnames = ss.str();
    }

    bap::table table() const {
        return table_from_string(opnames);
    }

    int intern(OpCode op) const {
        return offsets.at(op);
    }
};

class RegistersTable {
    std::string regnames;
    std::map<VarnodeData,int> offsets;
public:
    RegistersTable() : regnames(), offsets() {}

    void populate_registers(const Translate& translator) {
        offsets = {};
        std::map<VarnodeData,std::string> registers;
        int offset = 0;
        std::stringstream ss;
        translator.getAllRegisters(registers);
        for (const auto& elt : registers) {
            VarnodeData node = elt.first;
            std::string name = elt.second;
            ss << name << '\000';
            offsets[node] = offset;
            offset += name.length() + 1;
        }
        regnames = ss.str();
    }

    bap::table table() const {
        return table_from_string(regnames);
    }

    bap::reg create_reg(const VarnodeData &node) const{
        bap::reg reg;
        if (node.space->getType() == IPTR_INTERNAL) {
            reg.code = node.offset;
            reg.name = -1;
        } else {
            auto pos = offsets.find(node);
            if (pos != offsets.end()) {
                reg.code = pos->second;
                reg.name = pos->second;
            } else {
                reg.code = 0;
                reg.name = 0;
            }
        }
        return reg;
    }
};


class AssemblyBuilder : public AssemblyEmit {
    std::string data;
public:
    void dump(const Address&, const std::string &mnem, const std::string &ops) {
        data = mnem + " " + ops;
    }

    std::string result() const {
        return data;
    }
};

class InstructionBuilder : public PcodeEmit {
    const Loader &loader;
    const RegistersTable &registers;
    const OpcodesTable &opcodes;
    bap::insn insn;
public:
    InstructionBuilder(const Loader &loader_,
                       const RegistersTable &registers_,
                       const OpcodesTable &opcodes_) :
        loader(loader_), registers(registers_), opcodes(opcodes_) {}

    virtual void dump(const Address &addr,
                      OpCode opcode,
                      VarnodeData *outvar,
                      VarnodeData *invars,
                      int4 number_of_inputs) {
        insn.code = opcode;
        insn.name = opcodes.intern(opcode);
        insn.loc.off = loader.offset(addr);
        if (outvar != nullptr) {
            insn.ops.push_back(operand(*outvar));
        }

        for (int i = 0; i < number_of_inputs; i++) {
            insn.ops.push_back(operand(invars[i]));
        }
    }

    bap::insn result(int len) {
        insn.loc.len = len;
        return insn;
    }

private:
    bap::operand operand(const VarnodeData &node) const {
        bap::operand result = {};
        if (node.space->getType() == IPTR_CONSTANT) {
            result.type = bap_disasm_op_imm;
            result.imm_val = node.offset;
        } else {
            result.type = bap_disasm_op_reg;
            result.reg_val = registers.create_reg(node);
        }
        return result;
    }
};

class Disassembler : public bap::disassembler_interface {
    Loader loader;
    DocumentStorage specification;
    ContextInternal context;
    Sleigh translator;
    OpcodesTable opcodes;
    RegistersTable regs;
    bap::insn current;

public:
    explicit Disassembler(const std::string &slafile)
        : translator(&loader, &context), current() {
        Document *doc = specification.openDocument(slafile);
        specification.registerTag(doc->getRoot());
        translator.initialize(specification);
        regs.populate_registers(translator);
    }

    virtual void set_memory(bap::memory mem) {
        loader.set_memory(mem);
    }

    virtual bap::table insn_table() const {
        return opcodes.table();
    }

    virtual bap::table reg_table() const {
        return regs.table();
    }

    virtual void step(uint64_t pc) {
        current = {};
        if (loader.is_mapped(pc)) {
            InstructionBuilder builder(loader, regs, opcodes);
            Address addr(translator.getDefaultCodeSpace(), pc);
            int length = translator.oneInstruction(builder, addr);
            current = builder.result(length);
        }
    }

    virtual bap::insn get_insn() const {
        return current;
    }

    virtual std::string get_asm() const {
        uint64_t pc = loader.address(current.loc.off);
        if (loader.is_mapped(pc)) {
            AssemblyBuilder builder;
            Address addr(translator.getDefaultCodeSpace(), pc);
            translator.printAssembly(builder,addr);
            return builder.result();
        }
        return "#undefined";
    }

    virtual bool satisfies(bap_disasm_insn_p_type p) const {
        bool current_is_invalid = current.code == 0;
        if (current_is_invalid) {
            return p == is_invalid;
        } else {
            OpCode op = static_cast<OpCode>(current.code);
            switch (p) {
            case is_true: return true;
            case is_return: return op == CPUI_RETURN;
            case is_call: return
                    op == CPUI_CALL ||
                    op == CPUI_CALLIND ||
                    op == CPUI_CALLOTHER;
            case is_barrier: return
                    op == CPUI_BRANCH ||
                    op == CPUI_BRANCHIND;
            case is_terminator: return satisfies(is_branch);
            case is_branch: return
                    op == CPUI_BRANCH ||
                    op == CPUI_CBRANCH ||
                    op == CPUI_BRANCHIND;
            case is_indirect_branch: return
                    op == CPUI_BRANCHIND;
            case is_conditional_branch: return
                    op == CPUI_CBRANCH;
            case is_unconditional_branch: return
                    op == CPUI_BRANCH ||
                    op == CPUI_BRANCHIND;
            case may_affect_control_flow: return
                    satisfies(is_branch) ||
                    satisfies(is_call);
            case may_store: return op == CPUI_STORE;
            case may_load: return op == CPUI_LOAD;
            }
        }
    }

    virtual bool supports(bap_disasm_insn_p_type) const {
        return true;
    }
};

const std::string testsla = "/usr/share/ghidra/Ghidra/Processors/x86/data/languages/x86.sla";


struct Factory : bap::disasm_factory {
    bap::result<bap::disassembler_interface>
    create(const char *triple, const char *cpu, int debug_level) {
        bap::result<bap::disassembler_interface> r;
        auto dis = std::make_shared<Disassembler>(testsla);
        r.dis = dis;
        r.ok = 0;
        return r;
    }
};


int disasm_ghidra_init () {
    register_disassembler("ghidra", std::make_shared<Factory>());
    return 0;
}
