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

enum CoreOpCode {
    CORE_SEQ = CPUI_MAX,
    CORE_MAX
};

class OpcodesTable {
    std::string opnames;
    std::map<OpCode,int> cpui_offsets;
    std::map<CoreOpCode, int> core_offsets;

public:
    OpcodesTable() {
        std::stringstream ss;
        for (int i = 0; i < CPUI_MAX; i++) {
            OpCode op = static_cast<OpCode>(i);
            cpui_offsets[op] = ss.tellp();
            ss << "pcode:" << get_opname(op) << '\000';
        }
        core_offsets[CORE_SEQ] = ss.tellp();
        ss << "core:seq" << '\000';
        opnames = ss.str();
    }

    bap::table table() const {
        return table_from_string(opnames);
    }

    int intern(int op) const {
        if (op > 0 && op < CPUI_MAX) {
            return cpui_offsets.at(static_cast<OpCode>(op));
        } else if (op > 0 && op < CORE_MAX) {
            return core_offsets.at(static_cast<CoreOpCode>(op));
        } else {
            return 0;
        }
    }

};

class RegistersTable {
    std::string regnames;
    std::map<VarnodeData,int> offsets;
    std::set<int> known_spaces;
public:
    RegistersTable() : regnames(), offsets() {}

    void populate_registers(const Translate& translator) {
        offsets = {};
        std::map<VarnodeData,std::string> registers;
        std::stringstream ss;
        ss << "Nil" << '\000';
        translator.getAllRegisters(registers);
        for (const auto& elt : registers) {
            VarnodeData node = elt.first;
            std::string name = elt.second;
            known_spaces.insert(node.space->getIndex());
            offsets[node] = ss.tellp();
            ss << name << '\000';
        }
        regnames = ss.str();
    }

    bap::table table() const {
        return table_from_string(regnames);
    }

    bap::reg create_reg(const VarnodeData &node) const{
        bap::reg reg = {};
        if (is_virtual(node)) {
            reg.code = -node.offset;
            reg.name = node.space->getShortcut();
        } else {
            auto pos = offsets.find(node);
            if (pos != offsets.end()) {
                reg.code = pos->second;
                reg.name = pos->second;
            }
        }
        return reg;
    }
private:
    bool is_virtual(const VarnodeData &node) const {
        return known_spaces.find(node.space->getIndex()) ==
            known_spaces.end();
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
    const AddrSpaceManager &spaces;
    std::vector<bap::insn> insns;
public:
    InstructionBuilder(const Loader &loader_,
                       const RegistersTable &registers_,
                       const OpcodesTable &opcodes_,
                       const AddrSpaceManager &spaces_) :
        loader(loader_), registers(registers_),
        opcodes(opcodes_), spaces(spaces_) {}

    virtual void dump(const Address &addr,
                      OpCode opcode,
                      VarnodeData *outvar,
                      VarnodeData *invars,
                      int4 number_of_inputs) {
        insns.push_back(bap::insn());
        int p = insns.size() - 1;
        insns[p].code = opcode;
        insns[p].name = opcodes.intern(opcode);
        insns[p].loc.off = loader.offset(addr);
        if (outvar != nullptr) {
            insns[p].ops.push_back(operand(*outvar));
        }
        for (int i = 0; i < number_of_inputs; i++) {
            insns[p].ops.push_back(operand(invars[i]));
        }
    }

    void reset() {
        insns.clear();
    }

    bap::insn result(int len) {
        if (insns.size() == 0) {
            return bap::insn();
        } else if (insns.size() == 1) {
            insns[0].loc.len = len;
            return insns[0];
        } else {
            bap::insn insn;
            insn.code = CORE_SEQ;
            insn.name = opcodes.intern(CORE_SEQ);
            for (int i = 0; i < insns.size(); i++) {
                bap::operand op;
                op.type = bap_disasm_op_insn;
                op.sub_val = &insns[i];
                insns[i].loc.len = len;
                insn.ops.push_back(op);
            }
            insn.loc.off = insns[0].loc.off;
            insn.loc.len = len;
            return insn;
        }
    }

private:
    bap::operand operand(const VarnodeData &node) const {
        bap::operand result = {};
        if (is_constant(node.space) || is_address(node.space)) {
            result.type = bap_disasm_op_imm;
            result.imm_val = node.offset;
        } else {
            result.type = bap_disasm_op_reg;
            result.reg_val = registers.create_reg(node);
        }
        return result;
    }

    bool is_address(const AddrSpace *space) const {
        return is_code(space) || is_data(space);
    }

    bool is_data(const AddrSpace *space) const {
        return spaces.getDefaultDataSpace() == space;
    }

    bool is_code(const AddrSpace *space) const {
        return spaces.getDefaultCodeSpace() == space;
    }

    bool is_constant(const AddrSpace *space) const {
        return space->getType() == IPTR_CONSTANT;
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
    InstructionBuilder builder;

public:
    explicit Disassembler(const std::string &slafile)
        : translator(&loader, &context)
        , current()
        , builder(loader, regs, opcodes, translator) {
        Document *doc = specification.openDocument(slafile);
        specification.registerTag(doc->getRoot());
        translator.initialize(specification);
        context.setVariableDefault("addrsize", 1);
        context.setVariableDefault("opsize", 1);
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
        builder.reset();
        if (loader.is_mapped(pc)) {
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
