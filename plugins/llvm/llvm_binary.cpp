#include "llvm_binary.hpp"

extern "C" {
    const img::image* image_create(const char* data, size_t size) {
        return img::create(data, size);
    }

    void image_destroy(const img::image* m) {
        delete m;
    }

    const char* image_arch(const img::image* m) {
        return m->arch().c_str();
    }

    uint64_t image_entry(const img::image* m) {
        return m->entry();
    }

    size_t image_segment_count(const img::image* m) {
        return m->segments().size();
    }

    size_t image_section_count(const img::image* m) {
        return m->sections().size();
    }

    size_t image_symbol_count(const img::image* m) {
        return m->symbols().size();
    }

    const sec::section* image_section_from_index(const img::image* m,
                                                 size_t i) {
        return &m->sections()[i];
    }

    const sym::symbol* image_symbol_from_index(const img::image* m,
                                      size_t i) {
        return &m->symbols()[i];
    }

    const seg::segment* image_segment_from_index(const img::image* m,
                                                 size_t i) {
        return &m->segments()[i];
    }


    const char* segment_name(const seg::segment* s) {
        return s->name.c_str();
    }

    uint64_t segment_offset(const seg::segment* s) {
        return s->offset;
    }

    uint64_t segment_addr(const seg::segment* s) {
        return s->addr;
    }

    uint64_t segment_size(const seg::segment* s) {
        return s->size;
    }

    bool segment_is_readable(const seg::segment* s) {
        return s->is_readable;
    }

    bool segment_is_writable(const seg::segment* s) {
        return s->is_writable;
    }

    bool segment_is_executable(const seg::segment* s) {
        return s->is_executable;
    }

    const char* section_name(const sec::section* s) {
        return s->name.c_str();
    }

    uint64_t section_addr(const sec::section* s) {
        return s->addr;
    }

    uint64_t section_size(const sec::section* s) {
        return s->size;
    }

    const char* symbol_name(const sym::symbol* s) {
        return s->name.c_str();
    }

    int symbol_kind(const sym::symbol* s) {
        return s->kind;
    }

    uint64_t symbol_addr(const sym::symbol* s) {
        return s->addr;
    }

    uint64_t symbol_size(const sym::symbol* s) {
        return s->size;
    }

}
