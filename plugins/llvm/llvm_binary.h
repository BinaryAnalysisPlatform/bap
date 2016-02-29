#ifndef LLVM_BINARY_H
#define LLVM_BINARY_H

struct image;
struct section;
struct symbol;
struct segment;

const struct image* image_create(const char*, size_t);
void image_destroy(const struct image*);
const char* image_arch(const struct image*);
uint64_t image_entry(const struct image*);
size_t image_segment_count(const struct image*);
size_t image_section_count(const struct image*);
size_t image_symbol_count(const struct image*);
const struct section* image_section_from_index(const struct image*, size_t);
const struct symbol* image_symbol_from_index(const struct image*, size_t);
const struct segment* image_segment_from_index(const struct image*, size_t);


const char* segment_name(const struct segment*);
uint64_t segment_offset(const struct segment*);
uint64_t segment_addr(const struct segment*);
uint64_t segment_size(const struct segment*);
bool segment_is_readable(const struct segment*);
bool segment_is_writable(const struct segment*);
bool segment_is_executable(const struct segment*);

const char* section_name(const struct section*);
uint64_t section_addr(const struct section*);
uint64_t section_size(const struct section*);

const char* symbol_name(const struct symbol*);
int symbol_kind(const struct symbol*);
uint64_t symbol_addr(const struct symbol*);
uint64_t symbol_size(const struct symbol*);


#endif //LLVM_BINARY_H
