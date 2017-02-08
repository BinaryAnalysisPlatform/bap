#ifndef LLVM_BINARY_H
#define LLVM_BINARY_H

struct image;

const struct image* bap_llvm_image_create(const char*, size_t);
void bap_llvm_image_destroy(const struct image*);
const char* bap_llvm_image_arch(const struct image*);
uint64_t bap_llvm_image_entry(const struct image*);

size_t bap_llvm_image_segments_number(const struct image*);
size_t bap_llvm_image_sections_number(const struct image*);
size_t bap_llvm_image_symbols_number(const struct image*);

const char* bap_llvm_image_segment_name(const struct image*, size_t);
uint64_t bap_llvm_image_segment_offset(const struct image*, size_t);
uint64_t bap_llvm_image_segment_addr(const struct image*, size_t);
uint64_t bap_llvm_image_segment_size(const struct image*, size_t);
bool bap_llvm_image_segment_is_readable(const struct image*, size_t);
bool bap_llvm_image_segment_is_writable(const struct image*, size_t);
bool bap_llvm_image_segment_is_executable(const struct image*, size_t);

const char* bap_llvm_image_symbol_name(const struct image*, size_t);
bool bap_llvm_image_symbol_is_fun(const struct image*, size_t);
bool bap_llvm_image_symbol_is_debug(const struct image*, size_t);
uint64_t bap_llvm_image_symbol_addr(const struct image*, size_t);
uint64_t bap_llvm_image_symbol_size(const struct image*, size_t);

const char* bap_llvm_image_section_name(const struct image*, size_t);
uint64_t bap_llvm_image_section_addr(const struct image*, size_t);
uint64_t bap_llvm_image_section_size(const struct image*, size_t);

#endif //LLVM_BINARY_H
