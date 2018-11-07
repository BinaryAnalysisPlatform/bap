#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/compatibility.h>
#include <caml/callback.h>

#include "llvm_loader.h"
#include "llvm_loader_stubs.h"

static void failn(int n) {
    caml_raise_with_arg(*caml_named_value("Llvm_loader_fail"), Val_int(n));
}

static void loader_fail(const struct bap_llvm_loader *loader, int n) {
    bap_llvm_loader_destroy(loader);
    failn(n);
}

value bap_llvm_load_stub(value arg) {
    CAMLparam1(arg);
    CAMLlocal1(result);
    const struct caml_ba_array* array = Caml_ba_array_val(arg);
    if ((!array->dim[0]) || (array->num_dims != 1))
        failn(2);
    const struct bap_llvm_loader *loader =
        bap_llvm_loader_create((const char*)(array->data), array->dim[0]);
    if (bap_llvm_file_not_supported(loader))
        loader_fail(loader, 2);
    if (bap_llvm_loader_failed(loader))
        loader_fail(loader, 1);
    const char *d = bap_llvm_loader_data(loader);
    result = caml_copy_string(d);
    bap_llvm_loader_destroy(loader);
    CAMLreturn(result);
}
