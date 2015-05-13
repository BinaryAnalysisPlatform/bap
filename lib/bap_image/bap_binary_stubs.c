#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/compatibility.h>
#include <inttypes.h>
#include <stdbool.h>

#include "bap_binary_stubs.h"

void llvm_binary_fail(const char* message) {
    caml_failwith(message);
}


image* from_value(value v) {return *(image**) Data_custom_val(v);}

void finalize(value v) {CAMLparam1(v); free (from_value(v)); CAMLreturn0;}

value to_value_arc(const char* m) {
    CAMLparam0();
    CAMLlocal1(v);
    CAMLreturn(caml_copy_string(m));
}

value to_value_img(image* m) {
    CAMLparam0();
    CAMLlocal1(v);
    static struct custom_operations binary_ops = {
        "llvm.caml.llvm_image",
        finalize,
        custom_compare_default,
        custom_hash_default,
        custom_serialize_default,
        custom_deserialize_default
    };
    v = alloc_custom(&binary_ops, sizeof(image*), 0, 1);
    *((image**)(Data_custom_val(v))) = m;
    CAMLreturn(v);
}

value to_value_seg(const segment* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(7, 0);  
    Store_field (result, 0, caml_copy_string(seg_name(s)));
    Store_field (result, 1, caml_copy_int64(seg_offset(s)));
    Store_field (result, 2, caml_copy_int64(seg_addr(s)));
    Store_field (result, 3, caml_copy_int64(seg_size(s)));
    Store_field (result, 4, Val_bool(seg_readable(s)));
    Store_field (result, 5, Val_bool(seg_writable(s)));
    Store_field (result, 6, Val_bool(seg_executable(s)));
    CAMLreturn (result);
}

value to_value_sec(const section* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(3, 0);  
    Store_field (result, 0, caml_copy_string(sec_name(s)));
    Store_field (result, 1, caml_copy_int64(sec_addr(s)));
    Store_field (result, 2, caml_copy_int64(sec_size(s)));
    CAMLreturn (result);
}

value to_value_sym(const symbol* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(4, 0);  
    Store_field (result, 0, caml_copy_string(sym_name(s)));
    Store_field (result, 1, Val_int(sym_kind(s)));
    Store_field (result, 2, caml_copy_int64(sym_addr(s)));
    Store_field (result, 3, caml_copy_int64(sym_size(s)));
    CAMLreturn (result);

}

value to_value_secs(const image* data) {
    CAMLparam0();
    CAMLlocal2(result, cons);
    result = Val_emptylist;
    uint64_t size = secs_count(data) - 1;
    while ( size > 0) {
        cons = caml_alloc(2, 0);
        Store_field (cons, 0, to_value_sec(sec_from_index(data, size)));// head
        Store_field (cons, 1, result); // tail
        result = cons;
        --size;
    }
    CAMLreturn (result);
}

value to_value_segs(const image* data) {
    CAMLparam0();
    CAMLlocal2(result, cons);
    result = Val_emptylist;
    int size = segs_count(data) - 1;
    while (size > 0) {
        cons = caml_alloc(2, 0);
        Store_field (cons, 0, to_value_seg(seg_from_index(data, size)));// head
        Store_field (cons, 1, result); // tail
        result = cons;
        --size;
    }
    CAMLreturn (result);
}

value to_value_syms(const image* data) {
    CAMLparam0();
    CAMLlocal2(result, cons);
    result = Val_emptylist;
    int size = syms_count(data) - 1;
    while (size > 0) {
        cons = caml_alloc(2, 0);
        Store_field (cons, 0, to_value_sym(sym_from_index(data, size)));// head
        Store_field (cons, 1, result); // tail
        result = cons;
        --size;
    }
    CAMLreturn (result);
}

CAMLprim value llvm_binary_create_stub(value arg) {
    CAMLparam1(arg);
    const caml_ba_array* array = Caml_ba_array_val(arg);
    if (array->num_dims != 1)
        caml_invalid_argument("invalid bigarray dimension");
    image* obj =
        c_create((const char*)(array->data), array->dim[0]);
    CAMLreturn(to_value_img(obj));
}

CAMLprim value llvm_binary_arch_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(to_value_arc(c_arch(from_value(arg))));
}

CAMLprim value llvm_binary_entry_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(caml_copy_int64(c_entry(from_value(arg))));
}

CAMLprim value llvm_binary_segments_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(to_value_seg(c_segments(from_value(arg))));
}

CAMLprim value llvm_binary_symbols_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(to_value_sym(c_symbols(from_value(arg))));
}

CAMLprim value llvm_binary_sections_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(to_value_sec(c_sections(from_value(arg))));
}
