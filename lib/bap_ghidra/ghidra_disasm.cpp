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

enum ExtraOpCode {
    CORE_SEQ = CPUI_MAX,        // a sequence of subinstructions
    EXTRA_GOTO,                 // intra-instruction branch
    EXTRA_CGOTO,                // intra-instruction cbranch
    EXTRA_MAX
};


std::string get_extra_opname(ExtraOpCode op) {
    switch (op) {
    case CORE_SEQ: return "core:seq";
    case EXTRA_GOTO: return "pcode-extra:GOTO";
    case EXTRA_CGOTO: return "pcode-extra:CGOTO";
    }
}

class OpcodesTable {
    std::string opnames;
    std::map<OpCode,int> cpui_offsets;
    std::map<ExtraOpCode,int> extra_offsets;
    // user-defined opcodes have a separate indexing
    std::map<int,int> user_offsets;

public:
    void populate_opcodes(const Translate &translator) {
        std::stringstream ss;
        for (int i = 0; i < CPUI_MAX; i++) {
            OpCode op = static_cast<OpCode>(i);
            cpui_offsets[op] = ss.tellp();
            ss << "pcode:" << get_opname(op) << '\000';
        }

        for (int i = CPUI_MAX; i < EXTRA_MAX; i++) {
            ExtraOpCode op = static_cast<ExtraOpCode>(i);
            extra_offsets[op] = ss.tellp();
            ss << get_extra_opname(op) << '\000';
        }

        std::vector<string> userops;
        translator.getUserOpNames(userops);
        for (int op = 0; op < userops.size(); op++) {
            user_offsets[op] = ss.tellp();
            ss << userops[op] << '\000';
        }
        opnames = ss.str();
    }

    bap::table table() const {
        return table_from_string(opnames);
    }

    int intern(int op) const {
        if (op > 0 && op < CPUI_MAX) {
            return cpui_offsets.at(static_cast<OpCode>(op));
        } else if (op >= CPUI_MAX && op < EXTRA_MAX) {
            return extra_offsets.at(static_cast<ExtraOpCode>(op));
        } else {
            return 0;
        }
    }

    int intern_user(int op) const {
        return user_offsets.at(op);
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
        if (opcode == CPUI_CALLOTHER) {
            int opcode = invars[0].offset;
            insns[p].code = opcode;
            insns[p].name = opcodes.intern_user(opcode);
            number_of_inputs -= 1;
            invars += 1;
        } else {
            int newcode = extended_opcode(opcode, invars);
            insns[p].code = newcode;
            insns[p].name = opcodes.intern(newcode);
        }

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
            insn.name = opcodes.intern(insn.code);
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

    // we represent overloaded instructions, such as
    // BRANCH and CBRANCH with explicit opcodes,
    // GOTO and CGOTO correspondingly.
    int extended_opcode(OpCode op, VarnodeData *ivars) {
        if ((op == CPUI_BRANCH || op == CPUI_CBRANCH) &&
            is_constant(ivars[0].space)) {
            return op == CPUI_BRANCH ? EXTRA_GOTO : EXTRA_CGOTO;
        } else {
            return op;
        }
    }
};

bool matches(const bap::insn &insn, bap_disasm_insn_p_type p) {
    if (insn.code == 0) {
        return p == is_invalid;
    } else {
        std::vector<bap::operand> ops = insn.ops;
        bool sat = std::any_of(ops.begin(), ops.end(), [p](bap::operand oper) {
            return
                oper.type == bap_disasm_op_insn &&
                matches(*oper.sub_val, p);
        });

        OpCode op = static_cast<OpCode>(insn.code);
        switch (p) {
        case is_true:
            sat = true;
        case may_store:
            sat |= op == CPUI_STORE;
            break;
        case may_load:
            sat |= op == CPUI_LOAD;
            break;
        case is_conditional_branch:
            sat |= op == CPUI_CBRANCH;
            break;
        case may_affect_control_flow:
        case is_terminator:
        case is_branch:
            sat |= op == CPUI_CBRANCH;
        case is_barrier:
        case is_unconditional_branch:
            sat |= op == CPUI_BRANCH;
        case is_call:
            sat |= p != is_barrier && (op == CPUI_CALL || op == CPUI_CALLIND);
        case is_indirect_branch:
            sat |= p != is_call && op == CPUI_BRANCHIND || op == CPUI_CALLIND;
        case is_return:
            sat |= p != is_call && op == CPUI_RETURN;
        }
        return sat;
    }
 }

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
        opcodes.populate_opcodes(translator);
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
        return matches(current, p);
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
