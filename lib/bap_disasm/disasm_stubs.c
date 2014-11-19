#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>

#include "disasm.h"


/* noalloc */
value bap_disasm_create_stub(value backend,
                             value triple,
                             value cpu,
                             value debug_level) {
    int r = bap_disasm_create(String_val(backend),
                              String_val(triple),
                              String_val(cpu),
                              Int_val(debug_level));
    return Val_int(r);
}

/* noalloc */
value bap_disasm_delete_stub(value d) {
    bap_disasm_delete(Int_val(d));
    return Val_unit;
}

/* noalloc */
value bap_disasm_set_memory_stub(value d,
                                 value base,
                                 value data,
                                 value off,
                                 value len) {
    bap_disasm_set_memory(Int_val(d),
                          Int64_val(base),
                          (const char *)Caml_ba_data_val(data),
                          Int_val(off),
                          Int_val(len));
    return Val_unit;
}

/* noalloc */
value bap_disasm_store_predicates_stub(value d, value b) {
    bap_disasm_store_predicates(Int_val(d), Bool_val(b));
    return Val_unit;
}

/* noalloc */
value bap_disasm_store_asm_strings_stub(value d, value b) {
    bap_disasm_store_asm_strings(Int_val(d), Bool_val(b));
    return Val_unit;
}

/* alloc */
value bap_disasm_insn_table_stub(value d) {
    CAMLparam1(d);
    intnat dims[1];
    dims[0] = bap_disasm_insn_table_size(Int_val(d));
    CAMLreturn(caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                             (void *)bap_disasm_insn_table_ptr(Int_val(d)), dims));
}

/* alloc */
value bap_disasm_reg_table_stub(value d) {
    CAMLparam1(d);
    intnat dims[1];
    dims[0] = bap_disasm_reg_table_size(Int_val(d));
    CAMLreturn(caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                             (void *)bap_disasm_reg_table_ptr(Int_val(d)), dims));
}

/* noalloc */
value bap_disasm_predicates_clear_stub(value d) {
    bap_disasm_predicates_clear(Int_val(d));
    return Val_unit;
}

#define Pred_val(p) (bap_disasm_insn_p_type) Int_val(p)
/* noalloc */
value bap_disasm_predicates_push_stub(value d, value p) {
    bap_disasm_predicates_push(Int_val(d), Pred_val(p));
    return Val_unit;
}

/* noalloc */
value bap_disasm_predicate_is_supported_stub(value d, value p) {
    return Val_bool(bap_disasm_predicate_is_supported(Int_val(d), Pred_val(p)));
}


/* noalloc */
value bap_disasm_set_offset_stub(value d, value off) {
    bap_disasm_set_offset(Int_val(d), Int_val(off));
    return Val_unit;
}

/* noalloc */
value bap_disasm_run_stub(value d) {
    bap_disasm_run(Int_val(d));
    return Val_unit;
}

/* noalloc */
value bap_disasm_insns_clear_stub(value d) {
    bap_disasm_insns_clear(Int_val(d));
    return Val_unit;
}

/* noalloc */
value bap_disasm_insns_size_stub(value d) {
    return Val_int(bap_disasm_insns_size(Int_val(d)));
}


/* noalloc */
value bap_disasm_insn_size_stub(value d, value i) {
    return Val_int(bap_disasm_insn_size(Int_val(d), Int_val(i)));
}


/* noalloc */
value bap_disasm_insn_name_stub(value d, value i) {
    return Val_int(bap_disasm_insn_name(Int_val(d), Int_val(i)));
}

/* noalloc */
value bap_disasm_insn_code_stub(value d, value i) {
    return Val_int(bap_disasm_insn_code(Int_val(d), Int_val(i)));
}

/* noalloc */
value bap_disasm_insn_offset_stub(value d, value i) {
    return Val_int(bap_disasm_insn_offset(Int_val(d), Int_val(i)));
}

/* noalloc */
value bap_disasm_insn_asm_size_stub(value d, value i) {
    return Val_int(bap_disasm_insn_asm_size(Int_val(d), Int_val(i)));
}

/* noalloc */
value bap_disasm_insn_asm_copy_stub(value d, value i, value data) {
    bap_disasm_insn_asm_copy(Int_val(d), Int_val(i), String_val(data));
    return Val_unit;
}

/* noalloc */
value bap_disasm_insn_satisfies_stub(value d, value i, value p) {
    return Val_bool(bap_disasm_insn_satisfies(Int_val(d), Int_val(i), Pred_val(p)));
}

/* noalloc */
value bap_disasm_insn_ops_size_stub(value d, value i) {
    return Val_int(bap_disasm_insn_ops_size(Int_val(d), Int_val(i)));
}

/* noalloc */
value bap_disasm_insn_op_type_stub(value d, value i, value j) {
    return Val_int(bap_disasm_insn_op_type(Int_val(d), Int_val(i), Int_val(j)));
}

/* noalloc */
value bap_disasm_insn_op_reg_name_stub(value d, value i, value j) {
    return Val_int(bap_disasm_insn_op_reg_name(Int_val(d), Int_val(i), Int_val(j)));
}


/* noalloc */
value bap_disasm_insn_op_reg_code_stub(value d, value i, value j) {
    return Val_int(bap_disasm_insn_op_reg_code(Int_val(d), Int_val(i), Int_val(j)));
}

/* alloc */
value bap_disasm_insn_op_imm_value_stub(value d, value i, value j) {
    CAMLparam3(d,i,j);
    CAMLreturn(caml_copy_int64
               (bap_disasm_insn_op_imm_value(Int_val(d), Int_val(i), Int_val(j))));
}

/* noalloc */
value bap_disasm_insn_op_imm_small_value_stub(value d, value i, value j) {
    int64_t v = bap_disasm_insn_op_imm_value(Int_val(d), Int_val(i), Int_val(j));
    if (v < Min_long)
        v = Min_long;
    if (v > Max_long)
        v = Max_long;
    return Val_int(v);
}

/* alloc */
value bap_disasm_insn_op_fmm_value_stub(value d, value i, value j) {
    CAMLparam3(d,i,j);
    CAMLreturn(caml_copy_double
               (bap_disasm_insn_op_fmm_value(Int_val(d), Int_val(i), Int_val(j))));
}


/* noalloc */
value bap_disasm_insn_op_insn_value_stub(value d, value i, value j) {
    return Val_int(bap_disasm_insn_op_insn_value(Int_val(d), Int_val(i), Int_val(j)));
}
