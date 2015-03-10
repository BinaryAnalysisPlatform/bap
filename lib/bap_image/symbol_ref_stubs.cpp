extern "C" {
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
}

#include "symbol_ref_stubs.h"

CAMLprim value symbol_ref_name_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value symbol_ref_address_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value symbol_ref_is_function_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value symbol_ref_is_debug_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value symbol_ref_size_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

