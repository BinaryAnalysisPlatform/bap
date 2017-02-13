#include "caml/mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

    value bap_llvm_binary_create_stub(value);
    value bap_llvm_binary_arch_stub(value);
    value bap_llvm_binary_entry_stub(value);

    value bap_llvm_binary_segments_number_stub(value);
    value bap_llvm_binary_sections_number_stub(value);
    value bap_llvm_binary_symbols_number_stub(value);

    value bap_llvm_binary_segment_name_stub(value, value);
    value bap_llvm_binary_segment_addr_stub(value, value);
    value bap_llvm_binary_segment_size_stub(value, value);
    value bap_llvm_binary_segment_offset_stub(value, value);
    value bap_llvm_binary_segment_is_writable_stub(value, value);
    value bap_llvm_binary_segment_is_readable_stub(value, value);
    value bap_llvm_binary_segment_is_executable_stub(value, value);

    value bap_llvm_binary_symbol_name_stub(value, value);
    value bap_llvm_binary_symbol_addr_stub(value, value);
    value bap_llvm_binary_symbol_size_stub(value, value);
    value bap_llvm_binary_symbol_is_fun_stub(value, value);
    value bap_llvm_binary_symbol_is_debug_stub(value, value);

    value bap_llvm_binary_section_name_stub(value, value);
    value bap_llvm_binary_section_addr_stub(value, value);
    value bap_llvm_binary_section_size_stub(value, value);

#ifdef __cplusplus
}
#endif
