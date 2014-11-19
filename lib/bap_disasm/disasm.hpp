#include <vector>
#include <memory>

extern "C" {
    #include "disasm.h"
}


struct table;
struct memory;
struct predicate;
struct insn;

struct disasm_base {
    virtual table insn_table() = 0;
    virtual table reg_table() = 0;

    virtual std::shared_ptr<predicate>
    create_predicate(bap_disasm_insn_p_type) = 0;

    virtual void set_memory(memory) = 0;

    virtual void append_asm_to_string(std::string &s, const insn&) = 0;

    virtual insn get_insn(int64_t pc) = 0;
};


struct location {
    int off;
    int len;
};

struct memory {
    const char *data;
    int64_t     base;
    location    loc;
};

struct reg {
    int code;
    int name;
};

typedef int64_t imm;
typedef double  fmm;
class insn;

struct operand {
    bap_disasm_op_type type;
    union {
        reg reg_val;
        imm imm_val;
        fmm fmm_val;
        insn *insn_val;
    };
};

struct insn {
    int code;
    int name;
    location loc;
    std::vector<operand> ops;
};

struct predicate {
    virtual bool satisfies(const insn& ins) = 0;
};

struct table {
    const char *data;
    std::size_t size;
};
