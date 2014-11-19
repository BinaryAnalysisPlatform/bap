#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <cassert>

#include "disasm.hpp"

namespace bap {

using string = std::string;

template <typename T>
using vector = std::vector<T>;

template <typename T>
using shared_ptr = std::shared_ptr<T>;


struct disasm_factory {
    virtual shared_ptr<disasm_base>
    create(const char *backend, const char *cpu) = 0;
};


template <typename T>
T operand_value(operand) { assert(false);}

template <>
reg operand_value<reg>(operand op) {
    assert(op.type == bap_disasm_op_reg);
    return op.reg_val;
}

template <>
imm operand_value<imm>(operand op) {
    assert(op.type == bap_disasm_op_imm);
    return op.imm_val;
}

template <>
fmm operand_value<fmm>(operand op) {
    assert(op.type == bap_disasm_op_fmm);
    return op.fmm_val;
}

template <>
insn * operand_value<insn *>(operand op) {
    assert(op.type == bap_disasm_op_insn);
    return op.insn_val;
}


static std::map<string, shared_ptr<disasm_factory>> backends;

struct unknown_backend : std::exception {};


class disassembler {
    using predicates = vector<shared_ptr<predicate>>;
    using pred = predicates::value_type;
    using subkey = std::pair<int,int>;
    predicates preds;
    shared_ptr<disasm_base> dis;
    vector<insn> insns;
    vector<insn> sub_insns;
    vector<int>  asm_offs;
    std::map<subkey,int> submap;
    vector<int>  sub_asm_offs;
    std::string asms;
    int64_t base;
    int off;

public:
    disassembler(const char *name, const char *triple, const char *cpu)
    : base(0L), off(0) {
        auto factory = backends.find(name);

        if (factory != backends.end()) {
            factory->second->create(triple, cpu);
        } else {
            throw unknown_backend();
        }
    }

    void run() {
        while (1) {
            bool finished = step();
            if (finished)
                return;
        };
    }

    void set_memory(int64_t base, const char *data, int off, int len) {
        dis->set_memory({data, base, {off, len}});
    }

    void push_pred(bap_disasm_insn_p_type p) {
        auto pred = dis->create_predicate(p);
        if (pred) {
            preds.push_back(pred);
        }
    }

    void clear_preds() {
        preds.clear();
    }

    void clear_insns() {
        insns.clear();
        sub_insns.clear();
        asm_offs.clear();
        sub_asm_offs.clear();
        asms.clear();
    }

    int queue_size() const {
        return insns.size();
    }

    table insn_table() const {
        return dis->insn_table();
    }

    table reg_table() const {
        return dis->reg_table();
    }

    table asm_table() const {
        return { &asms[0], asms.size()};
    }

    bool is_supported(bap_disasm_insn_p_type p) {
        return dis->create_predicate(p) != NULL;
    }

    bool is_satisfied(bap_disasm_insn_p_type pre, const insn& insn) {
        auto p = dis->create_predicate(pre);
        return p && p->satisfies(insn);
    }

    void set_offset(int new_off) {
        off = new_off;
    }

    int offset() const {
        return off;
    }

    const insn& nth_insn(int i) const {
        return get(i, insns, sub_insns);
    }

    const int asm_offset(int i) const {
        return get(i, asm_offs, sub_asm_offs);
    }

    template <typename OpVal>
    OpVal oper_value(int i, int j) const {
        auto insn = nth_insn(i);
        assert(j >= 0 && j < insn.ops.size());
        return operand_value<OpVal>(insn.ops[j]);
    }

    int oper_insn(int i, int j) const {
        auto r = submap.find(std::make_pair(i,j));
        assert(r != submap.end());
        return -(r->second + 1);
    }

private:
    template <typename T>
    const T& get(int i, const vector<T> &cont, const vector<T> &sub) const {
        if (i < 0) {
            i = -i + 1;
            assert(i < sub_insns.size());
            return sub[i];
        } else {
            assert(i < insns.size());
            return cont[i];
        }
    }

    void update_asms(const insn& ins, vector<insn> &set, vector<int> &offs) {
        set.push_back(ins);
        offs.push_back(asms.size());
        dis->append_asm_to_string(asms, ins);
        asms.push_back('\x00');
    }

    bool step() {
        auto insn = dis->get_insn(base + off);
        off = insn.loc.off + insn.loc.len;
        update_asms(insn, insns, asm_offs);

        int insn_no = insns.size() - 1;
        int op_no = 0;
        std::for_each(insn.ops.begin(), insn.ops.end(), [&](operand op){
                if (op.type == bap_disasm_op_insn) {
                    update_asms(insn, sub_insns, sub_asm_offs);
                    int sub_no = sub_insns.size() - 1;
                    subkey key = {insn_no, op_no};
                    submap.insert(std::make_pair(key, sub_no));
                }
                op_no++;
            });

        return std::any_of(preds.begin(), preds.end(), [insn](pred p) {
                return p->satisfies(insn);
            });
    }
};

vector<shared_ptr<disassembler>> disassemblers;

}


using namespace bap;

bap_disasm_type bap_disasm_create(const char *backend,
                                  const char *triple,
                                  const char *cpu) try {
    int id = -1;
    auto dis = std::make_shared<disassembler>(backend, triple, cpu);

    for (int i = 0; i < disassemblers.size(); i++) {
        if (disassemblers[i] == NULL) {
            disassemblers[i] = dis;
            return i;
        }
    }
    id = disassemblers.size();
    disassemblers.push_back(dis);
    return id;
} catch (unknown_backend) {
    return BAP_DISASM_NO_SUCH_BACKEND;
} catch (std::exception) {
    return BAP_DISASM_UNKNOWN_ERROR;
}

static shared_ptr<disassembler> get(int d) {
    assert(d >= 0 && d < disassemblers.size());
}


void bap_disasm_set_memory(int d, int64_t base, const char *data, int off, int len) {
    get(d)->set_memory(base, data, off, len);
}

const char *bap_disasm_insn_table_ptr(int d) {
    return get(d)->insn_table().data;
}

int bap_disasm_insn_table_size(int d) {
    return get(d)->insn_table().size;
}


const char *bap_disasm_reg_table_ptr(int d) {
    return get(d)->reg_table().data;
}

int bap_disasm_reg_table_size(int d) {
    return get(d)->reg_table().size;
}

const char *bap_disasm_queue_asm_table_ptr(int d) {
    return get(d)->asm_table().data;
}

int bap_disasm_queue_asm_table_size(int d) {
    return get(d)->asm_table().size;
}

void bap_disasm_predicates_clear(int d) {
    get(d)->clear_preds();
}

void bap_disasm_predicates_clear(int d, bap_disasm_insn_p_type p) {
    get(d)->push_pred(p);
}


int bap_disasm_predicate_is_supported(int d, bap_disasm_insn_p_type p) {
    return get(d)->is_supported(p);
}

void bap_disasm_set_offset(int d, int off) {
    get(d)->set_offset(off);
}

int bap_disasm_get_offset(int d) {
    return get(d)->offset();
}

void bap_disasm_run(int d) {
    get(d)->run();
}

void bap_disasm_insns_clear(int d) {
    get(d)->clear_insns();
}

int bap_disasm_insns_size(int d) {
    return get(d)->queue_size();
}

static const insn &get_insn(int d, int i) {
    return get(d)->nth_insn(i);
}

int bap_disasm_insn_size(int d, int i) {
    return get_insn(d,i).loc.len;
}

int bap_disasm_insn_offset(int d, int i) {
    return get_insn(d,i).loc.off;
}

int bap_disasm_insn_name(int d, int i) {
    return get_insn(d,i).name;
}

int bap_disasm_insn_code(int d, int i) {
    return get_insn(d,i).code;
}


int bap_disasm_isn_asm(int d, int i) {
    return get(d)->asm_offset(i);
}

int bap_disasm_insn_satisfies(int d, int i, bap_disasm_insn_p_type p) {
    return get(i)->is_satisfied(p, get_insn(d,i));
}

int bap_disasm_insn_ops_size(int d, int i) {
    return get_insn(d,i).ops.size();
}

int bap_disasm_insn_op_reg_name(int d, int i, int op) {
    return get(d)->oper_value<reg>(i,op).name;
}

int bap_disasm_insn_op_reg_code(int d, int i, int op) {
    return get(d)->oper_value<reg>(i,op).code;
}


imm bap_disasm_insn_op_imm_value(int d, int i, int op) {
    return get(d)->oper_value<imm>(i,op);
}

fmm bap_disasm_insn_op_fmm_value(int d, int i, int op) {
    return get(d)->oper_value<fmm>(i,op);
}

int bap_disasm_insn_op_insn_value(int d, int i, int op) {
    // assumption that sub instructions cannot have sub instructions fails here:
    assert(i >= 0);
    return get(i)->oper_insn(i,op);
}
