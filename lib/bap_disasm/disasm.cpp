#undef NDEBUG
#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <iterator>
#include <cassert>
#include <cstring>

// debug REMOVE
#include <iostream>

#include "disasm.hpp"

namespace bap {

using string = std::string;

template <typename T>
using vector = std::vector<T>;

template <typename T>
using shared_ptr = std::shared_ptr<T>;

static std::map<string, shared_ptr<disasm_factory>> backends;

static auto all_predicates = {
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
    may_store,
    may_load
};

int register_disassembler(string name, shared_ptr<disasm_factory> f) {
    if (backends.find(name) != backends.end())
        return -1;
    backends[name] = f;
    return 0;
}


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

class disassembler {
    using predicates = std::vector<bap_disasm_insn_p_type>;
    using pred = predicates::value_type;
    using subkey = std::pair<int,int>;

    shared_ptr<disassembler_interface> dis;
    predicates supported_predicates;
    predicates preds;
    vector<insn> insns;
    vector<string>  asms;
    string asm_cache;
    vector<predicates> insn_preds;
    int64_t base;
    int off;
    bool store_preds, store_asms;


    disassembler(shared_ptr<disassembler_interface> dis)
        : dis(dis)
        , base(0L)
        , off(0)
        , store_preds(false)
        , store_asms(false) {
        for (auto p : all_predicates) {
            if (dis->supports(p))
                supported_predicates.push_back(p);
        }
        // it should be already sorted, but nothing worse if will do it twice.
        sort(supported_predicates.begin(), supported_predicates.end());
    }

public:
    static result<disassembler>
    create(const char *name, const char *triple, const char *cpu, int debug_level) {
        if (auto factory = backends[name]) {
            auto result = factory->create(triple, cpu, debug_level);
            if (!result.dis) {
                return {nullptr, result.err};
            } else {
                auto dis = shared_ptr<disassembler>(new disassembler(result.dis));
                return {dis, 0};
            }
        } else
            return {nullptr, bap_disasm_no_such_backend};
    }

    void run() {
        while (1) {
            bool finished = step();
            if (finished)
                return;
        };
    }

    void set_memory(int64_t addr, const char *data, int offset, int length) {
        this->base = addr;
        this->off  = 0;
        dis->set_memory({data, addr, {offset, length}});
    }

    void enable_store_preds(bool enable) {
        store_preds = enable;
        if (enable == false)
            insn_preds.clear();
    }

    void enable_store_asms(bool enable) {
        store_asms = enable;
        if (enable == false)
            asms.clear();
    }

    void push_pred(bap_disasm_insn_p_type p) {
        preds.push_back(p);
        sort(preds.begin(), preds.end());
    }

    void clear_preds() {
        preds.clear();
    }

    void clear_insns() {
        insns.clear();
        asms.clear();
        insn_preds.clear();
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

    bool supports(bap_disasm_insn_p_type p) const {
        return dis->supports(p);
    }

    bool satisfies(bap_disasm_insn_p_type p, int n) {
        if (p == is_invalid) {
            return (insns[n].code == 0);
        }

        if (store_preds) {
            assert(n >= 0 && n < insn_preds.size());
            auto beg = insn_preds[n].begin(), end = insn_preds[n].end();
            return std::binary_search(beg, end, p);
        } else {
            assert (n == queue_size() - 1);
            return dis->satisfies(p);
        }
    }

    string get_asm(int n) {
        if (store_asms) {
            assert(n >= 0 && n < asms.size());
            return asms[n];
        } else {
            assert(n == queue_size() - 1);
            if (asms.size() == 1)
                return asms[0];
            else {
                if (asm_cache.empty())
                    asm_cache = dis->get_asm();
                return asm_cache;
            }
        }
    }

    void set_offset(int new_off) {
        off = new_off;
    }

    int offset() const {
        return off;
    }

    const insn& nth_insn(int i) const {
        assert(i < insns.size());
        return insns[i];
    }

    template <typename OpVal>
    OpVal oper_value(int i, int j) const {
        auto insn = nth_insn(i);
        assert(j >= 0 && j < insn.ops.size());
        return operand_value<OpVal>(insn.ops[j]);
    }

    bap_disasm_op_type oper_type(int i, int j) const {
        auto insn = nth_insn(i);
        assert(j >= 0 && j < insn.ops.size());
        return insn.ops[j].type;
    }

private:
    bool step() {
        dis->step(base + off);
        auto insn = dis->get_insn();
        off = insn.loc.off + insn.loc.len;
        
        insns.push_back(insn);
        asm_cache.clear();

        if (store_asms) {
            asms.push_back(dis->get_asm());
        }

        if (store_preds) {
            predicates ps;
            for (auto p : supported_predicates) {
                if (dis->satisfies(p))
                    ps.push_back(p);
            }
            insn_preds.push_back(ps);
        }

        if (insn.loc.len == 0) {
            return true;
        } else if (preds.size() == 0) {
            return false;
        } else if (preds[0] == is_true) {
            return true;
        } else {
            return std::any_of(preds.begin(), preds.end(), [&](pred p) {
                    return dis->satisfies(p);
                });
        }
    }
};

static vector< shared_ptr<disassembler> > disassemblers;

}


using namespace bap;

bap_disasm_type bap_disasm_create(const char *backend,
                                  const char *triple,
                                  const char *cpu,
                                  int debug_level) {
    auto result = disassembler::create(backend, triple, cpu, debug_level);

    if (!result.dis)
        return result.err;

    for (int i = 0; i < disassemblers.size(); i++) {
        if (disassemblers[i] == nullptr) {
            disassemblers[i] = result.dis;
            return i;
        }
    }
    disassemblers.push_back(result.dis);
    return disassemblers.size() - 1;
}

void bap_disasm_delete(bap_disasm_type d) {
    assert(d >= 0 && d < disassemblers.size());
    disassemblers[d].reset();
}

int bap_disasm_backends_size() {
    return backends.size();
}

const char* bap_disasm_backend_name(int i) {
    assert(i >=0 && i < bap_disasm_backends_size());
    auto p = backends.cbegin();
    advance(p, i);
    return p->first.c_str();
}

static inline shared_ptr<disassembler> get(int d) {
    assert(d >= 0 && d < disassemblers.size());
    auto dis = disassemblers[d];
    assert(dis);
    return dis;
}


void bap_disasm_set_memory(int d, int64_t base, const char *data, int off, int len) {
    get(d)->set_memory(base, data, off, len);
}

void bap_disasm_store_predicates(int d, int v) {
    get(d)->enable_store_preds(v);
}

void bap_disasm_store_asm_strings(int d, int v) {
    get(d)->enable_store_asms(v);
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

void bap_disasm_predicates_clear(int d) {
    get(d)->clear_preds();
}

void bap_disasm_predicates_push(int d, bap_disasm_insn_p_type p) {
    get(d)->push_pred(p);
}


int bap_disasm_predicate_is_supported(int d, bap_disasm_insn_p_type p) {
    return get(d)->supports(p);
}

void bap_disasm_set_offset(int d, int off) {
    get(d)->set_offset(off);
}

int bap_disasm_offset(int d) {
    return get(d)->offset();
}

int bap_disasm_insn_asm_size(int d, int i) {
    return get(d)->get_asm(i).size();
}

void bap_disasm_insn_asm_copy(int d, int i, void *dst) {
    string s = get(d)->get_asm(i);
    std::memcpy(dst, &s[0], s.size());
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

static inline const insn &get_insn(int d, int i) {
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

int bap_disasm_insn_satisfies(int d, int i, bap_disasm_insn_p_type p) {
    return get(d)->satisfies(p, i);
}

int bap_disasm_insn_ops_size(int d, int i) {
    return get_insn(d,i).ops.size();
}

bap_disasm_op_type bap_disasm_insn_op_type(int d, int i, int j) {
    return get(d)->oper_type(i,j);
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
