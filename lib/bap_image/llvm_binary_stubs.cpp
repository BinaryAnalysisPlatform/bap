#include <memory>
#include <string>

#include "llvm_binary_stubs.h"
#include "llvm_binary_stubs.hpp"

#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Object/Binary.h>
#include <llvm/Support/system_error.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Object/Archive.h>
#include <llvm/Object/ObjectFile.h>

#include "llvm_segment.hpp"

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
}

namespace impl {

using namespace llvm;
using namespace llvm::object;

Binary* from_value (::value v) {
    return *reinterpret_cast<Binary**>(Data_custom_val(v));
}

::value to_value (Binary* b) {
    static struct custom_operations binary_ops = {
        const_cast<char*>("llvm.caml.llvm_binary"),
        [](::value v) { delete from_value(v); },
        custom_compare_default,
        custom_hash_default,
        custom_serialize_default,
        custom_deserialize_default
    };
    ::value v = ::alloc_custom(&binary_ops, sizeof(Binary*), 0, 1);
    *reinterpret_cast<Binary**>(Data_custom_val(v)) = b;
    return v;
}

inline void fail_with_error(const error_code& ec) {
    ::caml_failwith(ec.message().c_str());
}

Binary* llvm_binary_create(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBuffer* buff(MemoryBuffer::getMemBufferCopy(data_ref, "binary"));
    OwningPtr<object::Binary> binary;
    if (error_code ec = createBinary(buff, binary))
        fail_with_error(ec);
    return binary.take();
}

Triple::ArchType llvm_binary_arch(Binary* binary) {
    return utils::create_extractor(binary)->arch();
}

::value to_value(Triple::ArchType arch) {
    typedef Triple::ArchType AT;
    switch(arch) {
    case AT::arm: return ::caml_hash_variant("arm");
    case AT::aarch64: return ::caml_hash_variant("aarch64");
    case AT::hexagon: return ::caml_hash_variant("hexagon");
    case AT::mips: return ::caml_hash_variant("mips");
    case AT::mipsel: return ::caml_hash_variant("mipsel");
    case AT::mips64: return ::caml_hash_variant("mips64");
    case AT::mips64el: return ::caml_hash_variant("mips64el");
    case AT::ppc: return ::caml_hash_variant("ppc");
    case AT::ppc64: return ::caml_hash_variant("ppc64");
    case AT::ppc64le: return ::caml_hash_variant("ppc64le");
    case AT::r600: return ::caml_hash_variant("r600");
    case AT::sparc: return ::caml_hash_variant("sparc");
    case AT::sparcv9: return ::caml_hash_variant("sparcv9");
    case AT::systemz: return ::caml_hash_variant("systemz");
    case AT::thumb: return ::caml_hash_variant("thumb");
    case AT::x86: return ::caml_hash_variant("x86");
    case AT::x86_64: return ::caml_hash_variant("x86_64");
    case AT::xcore: return ::caml_hash_variant("xcore");
    case AT::nvptx: return ::caml_hash_variant("nvptx");
    case AT::nvptx64: return ::caml_hash_variant("nvptx64");
    default:
        std::string message("Unknown arch: ");
        message += std::to_string(static_cast<int>(arch));
        ::caml_failwith(message.c_str());
    }
}

uint64_t llvm_binary_entry(Binary* binary) {
    return utils::create_extractor(binary)->entry();
}

::value segment_to_value(segment s) {
    CAMLlocal1(result);
    result = caml_alloc(8, 0);  
    Store_field (result, 0, caml_copy_string(s.name.data()));
    Store_field (result, 1, caml_copy_int64(s.address));
    Store_field (result, 2, caml_copy_int64(s.offset));
    Store_field (result, 3, Val_int(s.length));
    Store_field (result, 4, Val_int(s.bitwidth));
    Store_field (result, 5, Val_bool(s.is_readable));
    Store_field (result, 6, Val_bool(s.is_writable));
    Store_field (result, 7, Val_bool(s.is_executable));
    return result;
}

} //namespace llvm_binary

CAMLprim value llvm_binary_create_stub(value arg) {
    CAMLparam1(arg);
    const caml_ba_array* array = Caml_ba_array_val(arg);
    if (array->num_dims != 1)
        ::caml_invalid_argument("invalid bigarray dimension");
    impl::Binary* obj =
        impl::llvm_binary_create(reinterpret_cast<const char*>(array->data),
                                 array->dim[0]);
    CAMLreturn(impl::to_value(obj));
}

CAMLprim value llvm_binary_arch_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(impl::to_value(impl::llvm_binary_arch(impl::from_value(arg))));
}

CAMLprim value llvm_binary_entry_stub(value arg) {
    CAMLparam1(arg);
    CAMLreturn(::caml_copy_int64(impl::llvm_binary_entry(impl::from_value(arg))));
}

CAMLprim value llvm_binary_symbols_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value llvm_binary_sections_stub(value arg) {
    CAMLparam1(arg);

    CAMLreturn(Val_unit);
}

CAMLprim value llvm_binary_segments_stub(value bin_val) {
    CAMLparam1(bin_val);
    CAMLlocal2(result, cons);
    result = Val_emptylist;
    impl::Binary* bin = impl::from_value(bin_val);
    std::vector<segment> s = get_segments(bin);
    int length = s.size();
    for (int i = length - 1; i >= 0; i--) {
        cons = caml_alloc(2, 0);
        Store_field(cons, 0, impl::segment_to_value(s.at(i)));  // head
        Store_field(cons, 1, result);                           // tail
        result = cons;
    }
    CAMLreturn(result);
}
