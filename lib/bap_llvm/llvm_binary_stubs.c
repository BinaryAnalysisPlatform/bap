#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/compatibility.h>
#include <caml/callback.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include "llvm_binary_stubs.h"
#include "llvm_binary.h"

#define None_val (Val_int(0))
#define Some_tag 0

void bap_notify_error(const char *msg) {
    printf("Llvm backend error: %s\n", msg);
}

void bap_notify_warning(const char *msg) {
    printf("Llvm backend warning: %s\n", msg);
}

static const struct image* image_from_value(value v) {
    return *(const struct image**) Data_custom_val(v);
}

static void custom_finalize_image(value v) {
    CAMLparam1(v);
    bap_llvm_image_destroy(image_from_value(v));
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

value bap_llvm_binary_create_stub(value arg) {
    CAMLparam1(arg);
    CAMLlocal1(result);
    const struct caml_ba_array* array = Caml_ba_array_val(arg);
    if (array->num_dims != 1) {
        bap_notify_error("invalid bigarray dimension");
        CAMLreturn(None_val);
    }
    if (!array->dim[0]) {
        bap_notify_error("Unexpected EOF");
        CAMLreturn(None_val);
    }
    const struct image* obj =
        bap_llvm_image_create((const char*)(array->data), array->dim[0]);
    if (!obj) {
        bap_notify_error("Bad file format");
        CAMLreturn(None_val);
    }
    result = caml_alloc(1, Some_tag);
    Store_field(result, 0, image_to_value(obj));
    CAMLreturn(result);
}


#define BAP_LLVM_STUB(name, to_value)                                       \
    value bap_llvm_binary_##name##_stub(value img) {                        \
        CAMLparam1(img);                                                    \
        CAMLreturn(to_value(bap_llvm_image_##name(image_from_value(img)))); \
    }

#define BAP_LLVM_STUB_I(name, to_value)                                                 \
    value bap_llvm_binary_##name##_stub(value img, value i) {                           \
        CAMLparam2(img, i);                                                             \
        CAMLreturn(to_value(bap_llvm_image_##name(image_from_value(img), Int_val(i)))); \
    }

BAP_LLVM_STUB(arch, caml_copy_string)
BAP_LLVM_STUB(entry, caml_copy_int64)
BAP_LLVM_STUB(segments_number, Val_int) /* noalloc */
BAP_LLVM_STUB(sections_number, Val_int) /* noalloc */
BAP_LLVM_STUB(symbols_number, Val_int)  /* noalloc */

BAP_LLVM_STUB_I(segment_name, caml_copy_string)
BAP_LLVM_STUB_I(segment_addr, caml_copy_int64)
BAP_LLVM_STUB_I(segment_size, caml_copy_int64)
BAP_LLVM_STUB_I(segment_offset, caml_copy_int64)
BAP_LLVM_STUB_I(segment_is_writable, Val_bool)   /* noalloc */
BAP_LLVM_STUB_I(segment_is_readable, Val_bool)   /* noalloc */
BAP_LLVM_STUB_I(segment_is_executable, Val_bool) /* noalloc */

BAP_LLVM_STUB_I(symbol_name, caml_copy_string)
BAP_LLVM_STUB_I(symbol_addr, caml_copy_int64)
BAP_LLVM_STUB_I(symbol_size, caml_copy_int64)
BAP_LLVM_STUB_I(symbol_is_fun, Val_bool)   /* noalloc */
BAP_LLVM_STUB_I(symbol_is_debug, Val_bool) /* noalloc */

BAP_LLVM_STUB_I(section_name, caml_copy_string)
BAP_LLVM_STUB_I(section_addr, caml_copy_int64)
BAP_LLVM_STUB_I(section_size, caml_copy_int64)
