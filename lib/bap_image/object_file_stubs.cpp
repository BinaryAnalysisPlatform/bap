extern "C" {
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
}

#include "object_file_stubs.h"

CAMLprim value object_file_create_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value object_file_arch_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value object_file_symbols_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value object_file_sections_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}


