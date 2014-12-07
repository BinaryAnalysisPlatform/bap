#include <llvm/ADT/OwningPtr.h>
#include <llvm/MC/MCAsmInfo.h>
#include <llvm/MC/MCContext.h>
#include <llvm/MC/MCDisassembler.h>
#include <llvm/MC/MCInstPrinter.h>
#include <llvm/MC/MCInstrInfo.h>
#include <llvm/MC/MCRegisterInfo.h>
#include <llvm/Support/DataTypes.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/MemoryObject.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetInstrInfo.h>


#include <cstring>
#include <iostream>

#include "disasm.hpp"
#include "llvm_disasm.h"

template <typename T>
using shared_ptr = std::shared_ptr<T>;

namespace bap {


void initialize_llvm() {
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllDisassemblers();
}

using pred_fun = std::function<bool(const llvm::MCInstrDesc&)>;

class MemoryObject : public llvm::MemoryObject {
    memory mem;
public:
    MemoryObject(memory mem) : mem(mem) {}

    uint64_t getBase() const {
        return mem.base;
    }

    uint64_t getExtent() const {
        return mem.loc.len;
    }

    int readByte(uint64_t pc, uint8_t *ptr) const {
        int offset = pc - getBase();
        if (offset < 0 || offset >= getExtent())
            return -1;
        *ptr = mem.data[mem.loc.off + offset];
        return 0;
    }

    int readBytes(uint64_t addr, uint64_t size, uint8_t *buf) const {
        int offset = addr - getBase();
        if (offset + size > getExtent() || offset < 0)
            return -1;

        const char *ptr = &mem.data[mem.loc.off + offset];
        memcpy(buf, ptr, size);
        return 0;
    }
};


class llvm_disassembler;

static void output_error(const char *triple, const char *cpu,
                         std::string error, std::string tail="") {
    std::cerr
        << "llvm_disasm: failed to create llvm_disassmbler for:\n"
        << "    triple = " << triple << "\n"
        << "    cpu = " << cpu << "\n"
        << "Error: " << error
        << (tail == "" ? "" : "\nAdditional information:\n")
        << tail
        << std::endl;
}

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
    may_affect_control_flow
};

class llvm_disassembler : public disassembler_interface {
    shared_ptr<const llvm::MCRegisterInfo>  reg_info;
    shared_ptr<const llvm::MCInstrInfo>     ins_info;
    shared_ptr<const llvm::MCSubtargetInfo> sub_info;
    shared_ptr<const llvm::MCAsmInfo>       asm_info;
    shared_ptr<const llvm::MCContext>      ctx;
    shared_ptr<llvm::MCDisassembler>       dis;
    shared_ptr<const llvm::MemoryObject>   mem;
    shared_ptr<llvm::MCInstPrinter>       printer;
    const int debug_level;
    table ins_tab, reg_tab;
    llvm::MCInst mcinst;
    insn current;


    llvm_disassembler(int debug_level)
        : debug_level(debug_level), current(invalid_insn({0,0})) {}

public:
    static result<llvm_disassembler>
    create(const char *triple, const char *cpu, int debug_level) {
        std::string error;

        // returned value is not allocted
        const llvm::Target *target =
            llvm::TargetRegistry::lookupTarget(triple, error);

        if (!target) {
            if (debug_level > 0)
                output_error(triple, cpu, "target not found", error);
            return {NULL, bap_disasm_unsupported_target};
        }

        // target's createMC* functions allocates a new instance each time:
        // cf., Target/X86/MCTargetDesc/X86MCTargetDesc.cpp

        shared_ptr<const llvm::MCRegisterInfo>
            reg_info(target->createMCRegInfo(triple));

        if (!reg_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain registers information");
            return {nullptr, bap_disasm_unsupported_target};
        }

        const shared_ptr<const llvm::MCInstrInfo>
            ins_info(target->createMCInstrInfo());

        if (!ins_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain instructions information");
            return {nullptr, bap_disasm_unsupported_target};
        }

        shared_ptr<const llvm::MCSubtargetInfo>
            sub_info(target->createMCSubtargetInfo(triple, cpu, ""));

        if (!sub_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain subtarget information");
            return {nullptr, bap_disasm_unsupported_target};
        }

        shared_ptr<const llvm::MCAsmInfo>
            asm_info(target->createMCAsmInfo(*reg_info, triple));

        if (!asm_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain assembler information");
            return {nullptr, bap_disasm_unsupported_target};
        }

        shared_ptr<llvm::MCContext> ctx
            (new llvm::MCContext(&*asm_info, &*reg_info, 0));

        if (!ctx) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to create disassembly context");
            return {nullptr, bap_disasm_unsupported_target};
        }


        llvm::OwningPtr<llvm::MCRelocationInfo>
            rel_info(target->createMCRelocationInfo(triple, *ctx));

        if (!rel_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain relocation information");
            return {nullptr, bap_disasm_unsupported_target};
        }

        llvm::OwningPtr<llvm::MCSymbolizer>
            symbolizer(target->createMCSymbolizer(
                           triple,
                           nullptr, // getOpInfo
                           nullptr, // SymbolLookUp
                           nullptr, // DisInfo
                           &*ctx, rel_info.take()));

        if (!symbolizer) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to create symbolizer");
            return {nullptr, bap_disasm_unsupported_target};
        }

        shared_ptr<llvm::MCInstPrinter>
            printer (target->createMCInstPrinter
                     (asm_info->getAssemblerDialect(),
                      *asm_info, *ins_info, *reg_info, *sub_info));

        if (!printer) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to create instruction printer");
            return {nullptr, bap_disasm_unsupported_target};
        }
        /* Make the default for immediates to be in hex */
        printer->setPrintImmHex(true);
          
        shared_ptr<llvm::MCDisassembler>
            dis(target->createMCDisassembler(*sub_info));

        if (!dis) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to create the disassembler");
            return {nullptr, bap_disasm_unsupported_target};
        }

        dis->setSymbolizer(symbolizer);
        dis->setupForSymbolicDisassembly(
            nullptr, // getOpInfo
            nullptr, // SymbolLookUp
            nullptr, // DisInfo,
            &*ctx, rel_info);

        shared_ptr<llvm_disassembler> self(new llvm_disassembler(debug_level));
        self->printer  = printer;
        self->reg_info = reg_info;
        self->ins_info = ins_info;
        self->sub_info = sub_info;
        self->asm_info = asm_info;
        self->ctx = ctx;
        self->dis = dis;
        self->ins_tab = self->create_table(ins_info->getNumOpcodes(), ins_info);
        self->reg_tab = self->create_table(reg_info->getNumRegs(), reg_info);
        return {self, 0};
    }


    table insn_table() const {
        return ins_tab;
    }

    table reg_table() const {
        return reg_tab;
    }

    void set_memory(memory m) {
        mem.reset(new MemoryObject(m));
    }

    void step(int64_t pc) {
        mcinst.clear();
        uint64_t size = 0;
        auto status = dis->getInstruction
            (mcinst, size, *mem, pc,
             (debug_level > 2 ? llvm::errs() : llvm::nulls()),
             llvm::nulls());

        int off = (int)(pc - mem->getBase());

        if (off < mem->getExtent() && size == 0) {
            size += 1;
        }

        location loc = {off, (int)size};

        if (status == llvm::MCDisassembler::Success) {
            if (debug_level > 1) {
                std::cerr << "read: '" << get_asm() << "'\n";
            }
            current = valid_insn(loc);
        } else {
            if (debug_level > 0)
                std::cerr << "failed to decode insn at"
                          << " pc " << pc
                          << " offset " << off
                          << " skipping " << size << " bytes\n";
            current = invalid_insn(loc);
        }
    }

    insn get_insn() const {
        return current;
    }

    std::string get_asm() const {
        if (current.code != 0) {
            std::string data;
            llvm::raw_string_ostream stream(data);
            printer->printInst(&mcinst, stream, "");
            return stream.str();
        } else {
            return "";
        }
    }

    // invalid instruction doesn't satisfy any predicate except is_invalid.
    bool satisfies(bap_disasm_insn_p_type p) const {
        bool current_invalid = current.code == 0;
        if (p == is_invalid || current_invalid) {
            return (p == is_invalid) && current_invalid;
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

        for (int i = 0; i < mcinst.getNumOperands(); ++i) {
            const llvm::MCOperand &op = mcinst.getOperand(i);
            if (!op.isValid() || op.isExpr()) {
                if (debug_level > 0) {
                    std::cerr << "skipping instruction, because of invalid operand\n";
                }
                return invalid_insn(loc);
            }

            ins.ops.push_back(create_operand(op, loc));
        }

        ins.code = mcinst.getOpcode();
        ins.name = ins_info->getName(ins.code) - ins_tab.data;
        ins.loc = loc;
        return ins;
    }

    insn invalid_insn(location loc) const {
        return {0, 0L, loc};
    }

    operand create_operand(llvm::MCOperand mcop, location loc) const {
        using namespace llvm;
        operand op;

        if (mcop.isReg()) {
            op.type = bap_disasm_op_reg;
            op.reg_val = create_reg(mcop.getReg());
            return op;
        }
        if (mcop.isImm()) {
            op.type = bap_disasm_op_imm;
            op.imm_val = mcop.getImm();
            return op;
        }
        if (mcop.isFPImm()) {
            op.type = bap_disasm_op_fmm;
            op.fmm_val = mcop.getFPImm();
            return op;
        }
        if (mcop.isInst()) {
            std::cerr << "got subinst\n";
            abort();
        }
        abort();
    }

    reg create_reg(unsigned code) const {
        return {(int)code, (int) (reg_info->getName(code) - reg_tab.data)};
    }


    pred_fun fun_of_pred(bap_disasm_insn_p_type pred) const {
        using namespace llvm;
        switch (pred) {
        case is_return : return &MCInstrDesc::isReturn;
        case is_call   : return &MCInstrDesc::isCall;
        case is_barrier: return &MCInstrDesc::isBarrier;
        case is_terminator: return &MCInstrDesc::isTerminator;
        case is_branch: return &MCInstrDesc::isBranch;
        case is_indirect_branch : return &MCInstrDesc::isIndirectBranch;
        case is_conditional_branch : return &MCInstrDesc::isConditionalBranch;
        case is_unconditional_branch : return &MCInstrDesc::isUnconditionalBranch;
        default : return nullptr;
        }
    }


    template <typename Table>
    table create_table(int n, const Table &tab) const {
        // we can create our own table an copy all names to it but we
        // will just rely on a table, generated by tablegen.  Since,
        // we do not have an access to its size or pointer, we will
        // iterate over all instructions to find the first and the
        // last one pointers.  Since opcodes are remaped into indices,
        // we can't just take address of the lowest opcode insn, and
        // subtract it from the address of the highest one.
        assert(n > 0);
        const char *p = tab->getName(0);
        const char *q = p;
        for (int i = 0; i < n; i++) {
            const char *r = tab->getName(i);
            if (r < p)
                p = r;
            if (r > q)
                q = r;
        }

        std::size_t size = q - p + std::strlen(q);

        return {p, size};
    }

};

struct create_llvm_disassembler : disasm_factory {
    result<disassembler_interface>
    create(const char *triple, const char *cpu, int debug_level) {
        auto llvm = llvm_disassembler::create(triple, cpu, debug_level);
        result<disassembler_interface> r;
        r.dis = llvm.dis;
        if (!r.dis)
            r.err = llvm.err;
        return r;
    }
};

}


int bap_disasm_llvm_init() {
    auto f = std::make_shared<bap::create_llvm_disassembler>();
    bap::initialize_llvm();
    return bap::register_disassembler("llvm", f);
}
