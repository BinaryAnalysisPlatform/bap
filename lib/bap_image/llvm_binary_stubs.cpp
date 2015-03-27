#include <string>

#include "llvm_binary_stubs.h"
#include "llvm_binary_stubs.hpp"

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
}

void llvm_binary_fail [[ noreturn ]](const char* message) {
    ::caml_failwith(message);
}

namespace impl {

using namespace llvm;
using namespace llvm::object;

Binary* from_value (::value v) {
    return *reinterpret_cast<Binary**>(Data_custom_val(v));
}

::value to_value (Binary* b) {
    CAMLparam0();
    CAMLlocal1(v);
    static struct custom_operations binary_ops = {
        const_cast<char*>("llvm.caml.llvm_binary"),
        [](::value v) { CAMLparam1(v); delete from_value(v); CAMLreturn0;},
        custom_compare_default,
        custom_hash_default,
        custom_serialize_default,
        custom_deserialize_default
    };
    v = ::alloc_custom(&binary_ops, sizeof(Binary*), 0, 1);
    *reinterpret_cast<Binary**>(Data_custom_val(v)) = b;
    CAMLreturn(v);
}


::value to_value(Triple::ArchType arch) {
    CAMLparam0();
    CAMLlocal1(v);
    typedef Triple::ArchType AT;
    switch(arch) {
    case AT::arm: v = ::caml_hash_variant("arm"); break;
    case AT::aarch64: v = ::caml_hash_variant("aarch64"); break;
    case AT::hexagon: v = ::caml_hash_variant("hexagon"); break;
    case AT::mips: v = ::caml_hash_variant("mips"); break;
    case AT::mipsel: v = ::caml_hash_variant("mipsel"); break;
    case AT::mips64: v = ::caml_hash_variant("mips64"); break;
    case AT::mips64el: v = ::caml_hash_variant("mips64el"); break;
    case AT::ppc: v = ::caml_hash_variant("ppc"); break;
    case AT::ppc64: v = ::caml_hash_variant("ppc64"); break;
    case AT::ppc64le: v = ::caml_hash_variant("ppc64le"); break;
    case AT::r600: v = ::caml_hash_variant("r600"); break;
    case AT::sparc: v = ::caml_hash_variant("sparc"); break;
    case AT::sparcv9: v = ::caml_hash_variant("sparcv9"); break;
    case AT::systemz: v = ::caml_hash_variant("systemz"); break;
    case AT::thumb: v = ::caml_hash_variant("thumb"); break;
    case AT::x86: v = ::caml_hash_variant("x86"); break;
    case AT::x86_64: v = ::caml_hash_variant("x86_64"); break;
    case AT::xcore: v = ::caml_hash_variant("xcore"); break;
    case AT::nvptx: v = ::caml_hash_variant("nvptx"); break;
    case AT::nvptx64: v = ::caml_hash_variant("nvptx64"); break;
    default:
        std::string message("Unknown arch: ");
        message += std::to_string(static_cast<int>(arch));
        ::caml_failwith(message.c_str());
    }
    CAMLreturn(v);
}


::value to_value(const seg::segment& s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(7, 0);  
    Store_field (result, 0, caml_copy_string(s.name().c_str()));
    Store_field (result, 1, caml_copy_int64(s.offset()));
    Store_field (result, 2, caml_copy_int64(s.addr()));
    Store_field (result, 3, caml_copy_int64(s.size()));
    Store_field (result, 4, Val_bool(s.is_readable()));
    Store_field (result, 5, Val_bool(s.is_writable()));
    Store_field (result, 6, Val_bool(s.is_executable()));
    CAMLreturn(result);
}

::value to_value(const sym::symbol& s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(4, 0);  
    Store_field (result, 0, caml_copy_string(s.name().c_str()));
    Store_field (result, 1, Val_int(s.kind()));
    Store_field (result, 2, caml_copy_int64(s.addr()));
    Store_field (result, 3, caml_copy_int64(s.size()));
    CAMLreturn(result);
}

::value to_value(const sec::section& s) {
    CAMLparam0();
    CAMLlocal1(result);
    result = caml_alloc(3, 0);  
    Store_field (result, 0, caml_copy_string(s.name().c_str()));
    Store_field (result, 1, caml_copy_int64(s.addr()));
    Store_field (result, 2, caml_copy_int64(s.size()));
    CAMLreturn(result);
}

template <typename T>
::value to_value(const std::vector<T>& data) {
    CAMLparam0();
    CAMLlocal2(result, cons);
    result = Val_emptylist;
    auto begin = data.rbegin();
    auto end = data.rend();
    while (begin != end) {
        cons = caml_alloc(2, 0);
        Store_field(cons, 0, to_value(*begin));  // head
        Store_field(cons, 1, result); // tail
        result = cons;
        ++begin;
    }
    CAMLreturn(result);
}


} //namespace impl

CAMLprim value llvm_binary_create_stub(value arg) {
    CAMLparam1(arg);
    const caml_ba_array* array = Caml_ba_array_val(arg);
    if (array->num_dims != 1)
        ::caml_invalid_argument("invalid bigarray dimension");
    binary::Binary* obj =
        binary::llvm_binary_create(reinterpret_cast<const char*>(array->data),
                                   array->dim[0]);
    CAMLreturn(impl::to_value(obj));
}

CAMLprim value llvm_binary_arch_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(impl::to_value(binary::llvm_binary_arch(impl::from_value(arg))));
}

CAMLprim value llvm_binary_entry_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(::caml_copy_int64(binary::llvm_binary_entry(impl::from_value(arg))));
}

CAMLprim value llvm_binary_segments_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(impl::to_value(binary::llvm_binary_segments(impl::from_value(arg))));
}

CAMLprim value llvm_binary_symbols_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(impl::to_value(binary::llvm_binary_symbols(impl::from_value(arg))));
}

CAMLprim value llvm_binary_sections_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(impl::to_value(binary::llvm_binary_sections(impl::from_value(arg))));
}

