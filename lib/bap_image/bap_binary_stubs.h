#ifndef BAP_BINARY_STUBS_H
#define BAP_BINARY_STUBS_H

#include <caml/mlvalues.h>
#include "bap_binary.h"

typedef struct caml_ba_array caml_ba_array;

const section* c_sections(const image*);
const segment* c_segments(const image*);
const symbol*  c_symbols(const image*);
const char*    c_arch(const image*);
uint64_t       c_entry(const image*);
image*         c_create(const char*, size_t);

const char* seg_name(const segment*);
uint64_t seg_offset(const segment*);
uint64_t seg_addr(const segment*);
uint64_t seg_size(const segment*);
bool seg_readable(const segment*);
bool seg_writable(const segment*);
bool seg_executable(const segment*);

const char* sec_name(const section*);
uint64_t sec_addr(const section*);
uint64_t sec_size(const section*);

const char* sym_name(const symbol*);
int sym_kind(const symbol*);
uint64_t sym_addr(const symbol*);
uint64_t sym_size(const symbol*);

uint64_t segs_count(const image*);
uint64_t secs_count(const image*);
uint64_t syms_count(const image*);
const section* sec_from_index(const image*, uint64_t);
const symbol* sym_from_index(const image*, uint64_t);
const segment* seg_from_index(const image*, uint64_t);


value llvm_binary_create_stub(value);
value llvm_binary_arch_stub(value);
value llvm_binary_entry_stub(value);
value llvm_binary_segments_stub(value);
value llvm_binary_symbols_stub(value);
value llvm_binary_sections_stub(value);

image* from_value(value);

value to_value_img(image*);
value to_value_sec(const section*);
value to_value_seg(const segment*);
value to_value_sym(const symbol*);
value to_value_arc(const char*);
value to_value_secs(const image*);
value to_value_segs(const image*);
value to_value_syms(const image*);

#endif //BAP_BINARY_STUBS_H
