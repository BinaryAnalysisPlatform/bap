#include <vector>
#include <memory>
#include <set>
#include <cstdint>

extern "C" {
    #include "disasm.h"
}

namespace bap {
struct table;
struct memory;
struct insn;
struct location;
struct operand;



// An interface that should be implemented by a backend.
//
// A disassembler is handled as a very primitive automaton.  It has a
// state - current instruction. After each step automaton changes its
// state to another instruction.
//
// The instruction can be invalid, or can even point to a memory, that
// is not currently under control of the automaton.
//
// If it is impossible to disassemble the instruction, then the state
// of the disassembler is an invalid instruction. The location of the
// invalid instruction points to the memory, that was "consumed" in
// the process of disassembling.
//
// The initial state of the disassembler should be invalid instruction.
//
// If current instruction is invalid, then results of all other calls
// to disassembler, that involves the instruction are undefined.
struct disassembler_interface {
    // disassemble one instruction, starting from address \a pc.
    virtual void step(uint64_t pc) = 0;

    // directs disassembler to a specified memory region
    virtual void set_memory(memory) = 0;

    // table, containing all instruction names
    // each name is a null-terminated string.
    virtual table insn_table() const = 0;

    // table, containing all instruction names
    // each name is a null-terminated string.
    virtual table reg_table() const = 0;

    // returns last disassembled instruction
    virtual insn get_insn() const = 0;

    // returns a disassembly string of a current instruction.
    virtual std::string get_asm() const = 0;

    // true if insn satisifes predicate \a p.
    virtual bool satisfies(bap_disasm_insn_p_type p) const = 0;

    // returns a set of predicates supported by this disassmbler.
    virtual bool supports(bap_disasm_insn_p_type p) const = 0;
};

struct reg {
    int code;
    int name;
};

typedef int64_t imm;
typedef double  fmm;

struct operand {
    bap_disasm_op_type type;

    union {
        reg reg_val;
        imm imm_val;
        fmm fmm_val;
        insn *sub_val;
    };
};


struct location {
    int off;
    int len;
};

struct insn {
    int code;
    int name;
    location loc;
    std::vector<operand> ops;
};


struct memory {
    const char *data;
    uint64_t     base;
    location    loc;
};

struct table {
    const char *data;
    std::size_t size;
};


template <typename T>
struct result {
    std::shared_ptr<T> dis;
    union {
        int ok;
        bap_disasm_error err;
    };
};

// a first class contstructor for disassemblers.
struct disasm_factory {
    virtual result<disassembler_interface>
    create(const char *triple, const char *cpu, int debug_level) = 0;

    // an extended constructor that accepts a sequence of target attributes
    //
    // The default implementation just ignores it.
    virtual result<disassembler_interface>
    create(const char *triple, const char *cpu, const char *attrs, int debug_level) {
        return create(triple, cpu, debug_level);
    }
};


// registers new disassembler under the name. Returns 0 if ok,
// and -1 if the name is already taken.
int register_disassembler(std::string name, std::shared_ptr<disasm_factory> f);

}
