#include <llvm/MC/MCAsmInfo.h>
#include <llvm/MC/MCContext.h>
#include <llvm/MC/MCInstPrinter.h>
#include <llvm/MC/MCInstrInfo.h>
#include <llvm/MC/MCRegisterInfo.h>
#include <llvm/Support/DataTypes.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/CommandLine.h>
#include <llvm-c/Target.h>

#if LLVM_VERSION_MAJOR >= 10
#include "llvm/MC/MCTargetOptions.h"
#endif

#include <cstring>
#include <cstdint>
#include <limits>
#include <typeinfo>
#include <iostream>

#include "disasm.hpp"
#include "llvm_disasm.h"

#include <llvm/MC/MCDisassembler/MCDisassembler.h>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/Triple.h>
#include <llvm/ADT/Twine.h>

template <typename T>
using smart_ptr = std::unique_ptr<T>;
template <class T>
typename std::remove_reference<T>::type&& move(smart_ptr<T>&& t) {
    return std::move(t);
}

template <typename T>
using shared_ptr = std::shared_ptr<T>;

namespace bap {

void initialize_llvm() {
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllDisassemblers();
}

using pred_fun = std::function<bool(const llvm::MCInstrDesc&)>;

bool ends_with(const std::string& str, const std::string &suffix) {
    auto n = str.length(), m = suffix.length();
    return n >= m && str.compare(n-m,m,suffix) == 0;
}


class MemoryObject {
    memory mem;
public:
    MemoryObject(memory mem) : mem(mem) {}

    uint64_t getBase() const {
        return mem.base;
    }

    uint64_t getExtent() {
        return mem.loc.len;
    }

    llvm::ArrayRef<uint8_t> view(uint64_t pc) {
        int off = pc - this->getBase();
        int len = this->getExtent() - off;
        return llvm::ArrayRef<uint8_t>((const uint8_t*)&mem.data[mem.loc.off+off], len);
    }
};

class llvm_disassembler;

static void output_error(std::string triple, const char *cpu,
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
    may_affect_control_flow,
    may_load,
    may_store
};


class llvm_disassembler : public disassembler_interface {
    shared_ptr<const llvm::MCRegisterInfo>  reg_info;
    shared_ptr<const llvm::MCInstrInfo>     ins_info;
    shared_ptr<const llvm::MCSubtargetInfo> sub_info;
    shared_ptr<const llvm::MCAsmInfo>       asm_info;
    shared_ptr<const llvm::MCContext>       ctx;
    shared_ptr<llvm::MCDisassembler>        dis;
    shared_ptr<llvm::MCInstPrinter>         printer;
    const int debug_level;
    table ins_tab, reg_tab;
    llvm::MCInst mcinst;
    insn current;
    std::vector<int> prefixes;
    shared_ptr<MemoryObject>                mem;

    llvm_disassembler(int debug_level)
        : debug_level(debug_level), current(invalid_insn({0,0})) {}

public:
    static result<llvm_disassembler>
    create(const char *name, const char *cpu, int debug_level) {
        std::string error;
        llvm::Triple t(llvm::Triple::normalize(name));
        std::string triple = t.getTriple();

        // returned value is not allocated
        const llvm::Target *target =
            llvm::TargetRegistry::lookupTarget(name,t,error);;

        if (!target) {
            target = llvm::TargetRegistry::lookupTarget("", t, error);
        }

        if (!target) {
            if (debug_level > 0)
                output_error(triple, cpu, "target not found", error);
            return { NULL, {bap_disasm_unsupported_target} };
        }

        // target's createMC* functions allocates a new instance each time:
        // cf., Target/X86/MCTargetDesc/X86MCTargetDesc.cpp

        shared_ptr<const llvm::MCRegisterInfo>
            reg_info(target->createMCRegInfo(triple));

        if (!reg_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain registers information");
            return {nullptr, {bap_disasm_unsupported_target} };
        }

        const shared_ptr<const llvm::MCInstrInfo>
            ins_info(target->createMCInstrInfo());

        if (!ins_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain instructions information");
            return {nullptr, {bap_disasm_unsupported_target} };
        }

        shared_ptr<const llvm::MCSubtargetInfo>
            sub_info(target->createMCSubtargetInfo(triple, cpu, ""));

        if (!sub_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain subtarget information");
            return {nullptr, {bap_disasm_unsupported_target} };
        }


#if LLVM_VERSION_MAJOR >= 10
        llvm::MCTargetOptions mcopts;
        shared_ptr<const llvm::MCAsmInfo>
            asm_info(target->createMCAsmInfo(*reg_info, triple, mcopts));
#else
        shared_ptr<const llvm::MCAsmInfo>
            asm_info(target->createMCAsmInfo(*reg_info, triple));
#endif

        if (!asm_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain assembler information");
            return {nullptr, { bap_disasm_unsupported_target} };
        }

        shared_ptr<llvm::MCContext> ctx
            (new llvm::MCContext(&*asm_info, &*reg_info, 0));

        if (!ctx) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to create disassembly context");
            return {nullptr, {bap_disasm_unsupported_target} };
        }

        smart_ptr<llvm::MCRelocationInfo>
            rel_info(target->createMCRelocationInfo(triple, *ctx));

        if (!rel_info) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to obtain relocation information");
            return {nullptr, {bap_disasm_unsupported_target} };
        }

        smart_ptr<llvm::MCSymbolizer>
            symbolizer(target->createMCSymbolizer(
                           triple,
                           nullptr, // getOpInfo
                           nullptr, // SymbolLookUp
                           nullptr, // DisInfo
                           &*ctx, move(rel_info)));

        if (!symbolizer) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to create symbolizer");
            return {nullptr, {bap_disasm_unsupported_target} };
        }

        shared_ptr<llvm::MCInstPrinter>
            printer (target->createMCInstPrinter
                     (t, asm_info->getAssemblerDialect(), *asm_info, *ins_info, *reg_info));

        if (!printer) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to create instruction printer");
            return {nullptr, {bap_disasm_unsupported_target} };
        }
        /* Make the default for immediates to be in hex */
        printer->setPrintImmHex(true);

        shared_ptr<llvm::MCDisassembler>
            dis(target->createMCDisassembler(*sub_info, *ctx));

        if (!dis) {
            if (debug_level > 0)
                output_error(triple, cpu, "failed to create the disassembler");
            return {nullptr, {bap_disasm_unsupported_target} };
        }

        dis->setSymbolizer(move(symbolizer));

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
        self->init_prefixes();
        return {self, {0} };
    }


    table insn_table() const {
        return ins_tab;
    }

    table reg_table() const {
        return reg_tab;
    }

    //! this member function will not be needed anymore
    void set_memory(memory m) {
        mem.reset(new MemoryObject(m));
    }

    bool is_prefix() const {
        return std::binary_search(prefixes.begin(),
                                  prefixes.end(),
                                  current.code);
    }

    llvm::ArrayRef<uint8_t> view(uint64_t pc) {
        return mem->view(pc);
    }

    void step(uint64_t pc) {
        mcinst.clear();
        auto base = mem->getBase();

        if (pc < base) {
            current = invalid_insn(location{0,1});
        } else if (pc > base + mem->getExtent()) {
            auto off = static_cast<int>(mem->getExtent() - 1);
            current = invalid_insn(location{off,1});
        } else {
            uint64_t size = 0;
            int off = pc - mem->getBase();
            int len = mem->getExtent() - off;

            auto status = llvm::MCDisassembler::Fail;
            if (len > 0) {
#if LLVM_VERSION_MAJOR >= 10
                status = dis->getInstruction
                    (mcinst, size, view(pc), pc, llvm::nulls());
#else
                 status = dis->getInstruction
                    (mcinst, size, view(pc), pc,
                     (debug_level > 2 ? llvm::errs() : llvm::nulls()),
                     llvm::nulls());
#endif
            }

            location loc = {
                static_cast<int>(pc - base),
                static_cast<int>(size)
            };

            if (status == llvm::MCDisassembler::Fail) {
                if (debug_level > 0)
                    std::cerr << "failed to decode insn at"
                              << " pc " << pc
                              << " offset " << loc.off
                              << " skipping " << loc.len << " bytes\n";
                current = invalid_insn(loc);
            } else {
                current = valid_insn(loc);
                if (debug_level > 1) {
                    std::cerr << "read: '" << get_asm() << "'\n";
                }
                if (is_prefix() && size != 0) {
                    step(pc+size);

                    // a standalone prefix is not a valid instruction
                    if (current.loc.len == 0) {
                        current = invalid_insn(loc);
                    }

                    // a prefix to invalid instruction is invalid instruction
                    if (current.code != 0) {
                        location ext = {loc.off, loc.len + current.loc.len};
                        current = valid_insn(ext);
                    }
                }
            }
        }
    }

    insn get_insn() const {
        return current;
    }

    std::string get_asm() const {
        if (current.code != 0) {
            std::string data;
            llvm::raw_string_ostream stream(data);
#if LLVM_VERSION_MAJOR >= 10
            auto pc = mem->getBase() + current.loc.off;
            printer->printInst(&mcinst, pc, "", *sub_info, stream);
#else
            printer->printInst(&mcinst, stream, "", *sub_info);
#endif
            return stream.str();
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

    const char *get_name(const char *x) const { return x; }
    const char *get_name(const llvm::StringRef &x) const { return x.data(); }

    template <typename Table>
    const char* get_name(const Table &tab, int code) const {
        return get_name(tab.getName(code));
    }

    insn valid_insn(location loc) const {
        insn ins;

        for (std::size_t i = 0; i < mcinst.getNumOperands(); ++i) {
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
        ins.name = get_name(*ins_info, ins.code) - ins_tab.data;
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
        case may_load : return &MCInstrDesc::mayLoad;
        case may_store : return &MCInstrDesc::mayStore;
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
        const char *p = get_name(*tab, 0);
        const char *q = p;
        for (int i = 0; i < n; i++) {
            const char *r = get_name(*tab, i);
            if (r < p)
                p = r;
            if (r > q)
                q = r;
        }

        std::size_t size = q - p + std::strlen(q);

        return {p, size};
    }

    void init_prefixes() {
        for (std::size_t i = 0; i < ins_info->getNumOpcodes(); i++) {
            if (ends_with(std::string(ins_info->getName(i)), "_PREFIX")) {
                prefixes.push_back(i);
            }
        }
    }
};

// See https://github.com/BinaryAnalysisPlatform/bap/issues/1081
// for details
bool is_error_prone_arch(const char *triple) {
    if (LLVM_VERSION_MAJOR < 8)
        return std::string(triple) == "aarch64";
    else
        return false;
}


struct create_llvm_disassembler : disasm_factory {
    result<disassembler_interface>
    create(const char *triple, const char *cpu, int debug_level) {
        result<disassembler_interface> r;
        if (is_error_prone_arch(triple)) {
            if (debug_level > 0)
                output_error(triple, cpu, "unsupported target", "consider to update to llvm version >= 8.0");
            r.err = bap_disasm_unsupported_target;
        } else {
            auto llvm = llvm_disassembler::create(triple, cpu, debug_level);
            r.dis = llvm.dis;
            if (!r.dis)
                r.err = llvm.err;
        }
        return r;
    }
};

}

int disasm_llvm_init() {
    llvm::cl::ParseEnvironmentOptions("bap", "BAP_LLVM_OPTIONS");
    bap::initialize_llvm();
    auto f = std::make_shared<bap::create_llvm_disassembler>();
    return bap::register_disassembler("llvm", f);
}
