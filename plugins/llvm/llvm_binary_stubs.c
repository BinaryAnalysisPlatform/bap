#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/compatibility.h>
#include <inttypes.h>
#include <stdbool.h>

#include "llvm_binary_stubs.h"
#include "llvm_binary.h"

Noreturn void llvm_binary_fail(const char* message) {
    caml_failwith(message);
}

static const struct image* image_from_value(value v) {
    return *(const struct image**) Data_custom_val(v);
}

static void custom_finalize_image(value v) {
    CAMLparam1(v);
    image_destroy(image_from_value(v));
    CAMLreturn0;
}

static value image_to_value(const struct image* img) {
    CAMLparam0();
    CAMLlocal1(v);
    static struct custom_operations binary_ops = {
        "bap.llvm_loader",
        custom_finalize_image,
        custom_compare_default,
        custom_hash_default,
        custom_serialize_default,
        custom_deserialize_default
    };
    v = alloc_custom(&binary_ops, sizeof(struct image*), 0, 1);
    *((const struct image**)(Data_custom_val(v))) = img;
    CAMLreturn(v);
}

static value segment_to_value(const struct segment* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(7, 0);
    Store_field(result, 0, caml_copy_string(segment_name(s)));
    Store_field(result, 1, caml_copy_int64(segment_offset(s)));
    Store_field(result, 2, caml_copy_int64(segment_addr(s)));
    Store_field(result, 3, caml_copy_int64(segment_size(s)));
    Store_field(result, 4, Val_bool(segment_is_readable(s)));
    Store_field(result, 5, Val_bool(segment_is_writable(s)));
    Store_field(result, 6, Val_bool(segment_is_executable(s)));
    CAMLreturn(result);
}

static value section_to_value(const struct section* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(3, 0);
    Store_field(result, 0, caml_copy_string(section_name(s)));
    Store_field(result, 1, caml_copy_int64(section_addr(s)));
    Store_field(result, 2, caml_copy_int64(section_size(s)));
    CAMLreturn(result);
}

static value symbol_to_value(const struct symbol* s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(4, 0);
    Store_field(result, 0, caml_copy_string(symbol_name(s)));
    Store_field(result, 1, Val_int(symbol_kind(s)));
    Store_field(result, 2, caml_copy_int64(symbol_addr(s)));
    Store_field(result, 3, caml_copy_int64(symbol_size(s)));
    CAMLreturn(result);

}

static value image_sections_to_value(const struct image* img) {
    CAMLparam0();
    CAMLlocal2(result, cons);
    result = Val_emptylist;
    size_t i = image_section_count(img);
    while (i != 0) {
        cons = caml_alloc(2, 0);
        Store_field(cons, 0,
                    section_to_value(image_section_from_index(img, --i)));// head
        Store_field(cons, 1, result); // tail
        result = cons;
    }
    CAMLreturn (result);
}

static value image_segments_to_value(const struct image* img) {
    CAMLparam0();
    CAMLlocal2(result, cons);
    result = Val_emptylist;
    size_t i = image_segment_count(img);
    while (i != 0) {
        cons = caml_alloc(2, 0);
        Store_field(cons, 0,
                    segment_to_value(image_segment_from_index(img, --i)));// head
        Store_field(cons, 1, result); // tail
        result = cons;
    }
    CAMLreturn (result);
}

static value image_symbols_to_value(const struct image* img) {
    CAMLparam0();
    CAMLlocal2(result, cons);
    result = Val_emptylist;
    size_t i = image_symbol_count(img);
    while (i != 0) {
        cons = caml_alloc(2, 0);
        Store_field (cons, 0,
                     symbol_to_value(image_symbol_from_index(img, --i)));// head
        Store_field (cons, 1, result); // tail
        result = cons;
    }
    CAMLreturn (result);
}

CAMLprim value llvm_binary_create_stub(value arg) {
    CAMLparam1(arg);
    const struct caml_ba_array* array = Caml_ba_array_val(arg);
    if (array->num_dims != 1)
        caml_invalid_argument("invalid bigarray dimension");
    const struct image* obj =
        image_create((const char*)(array->data), array->dim[0]);
    CAMLreturn(image_to_value(obj));
}

CAMLprim value llvm_binary_arch_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(caml_copy_string(image_arch(image_from_value(arg))));
}

CAMLprim value llvm_binary_entry_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(caml_copy_int64(image_entry(image_from_value(arg))));
}

CAMLprim value llvm_binary_segments_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(image_segments_to_value(image_from_value(arg)));
}

CAMLprim value llvm_binary_symbols_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(image_symbols_to_value(image_from_value(arg)));
}

CAMLprim value llvm_binary_sections_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(image_sections_to_value(image_from_value(arg)));
}
