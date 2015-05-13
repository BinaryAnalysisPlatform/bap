#include "bap_binary.hpp"

extern "C" {

#include "bap_binary.h"

uint64_t c_entry(const img::image* m) {return m->entry();}

const char* c_arch(const img::image* m) {
    return (llvm::Triple::getArchTypeName(m->arch()));
}

const segment* c_segments(const img::image* m) {
    return ((const segment*) &(m->segments())[0]);
}

const symbol* c_symbols(const img::image* m) {
    return ((const symbol*) &(m->symbols())[0]);
}

const section* c_sections(const img::image* m) {
    return ((const section*) &(m->sections())[0]);
}

image* c_create(const char* data, size_t size) {
    return (image*) img::create(data, size);
}

const char* seg_name(const seg::segment* s) {return s->name().c_str();}

uint64_t seg_offset(const seg::segment* s) {return s->offset();}

uint64_t seg_addr(const seg::segment* s) {return s->addr();}

uint64_t seg_size(const seg::segment* s) {return s->size();}

bool seg_readable(const seg::segment* s) {return s->is_readable();}

bool seg_writable(const seg::segment* s) {return s->is_writable();}

bool seg_executable(const seg::segment* s) {return s->is_executable();}

const char* sec_name(const sec::section* s) {return s->name().c_str();}

uint64_t sec_addr(const sec::section* s) {return s->addr();}

uint64_t sec_size(const sec::section* s) {return s->size();}

const char* sym_name(const sym::symbol* s) {return s->name().c_str();}

uint64_t sym_addr(const sym::symbol* s) {return s->addr();}
    
uint64_t sym_size(const sym::symbol* s) {return s->size();}

int sym_kind(const sym::symbol* s) {return s->kind();}

uint64_t segs_count(const img::image* m) {
    return img::count_elements(m->segments());
}

uint64_t secs_count(const img::image* m) {
    return img::count_elements(m->sections());
}

uint64_t syms_count(const img::image* m) {
    return img::count_elements(m->symbols());
}

const section* sec_from_index(const img::image* m, uint64_t i) {
    return (const section*)(&m->sections()[i]);
}

const segment* seg_from_index(const img::image* m, uint64_t i) {
    return (const segment*)(&m->segments()[i]);
}

const symbol* sym_from_index(const img::image* m, uint64_t i) {
    return (const symbol*)(&m->symbols()[i]);
}
}
