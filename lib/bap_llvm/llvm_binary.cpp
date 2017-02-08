#include <stdio.h>
#include "llvm_binary.hpp"

extern "C" {

#define BAP_LLVM_IMAGE_PROPERTY(return_type, name, field, property)  \
    return_type bap_llvm_image_##name(const img::image* m) {         \
        return m->field.property;                                    \
    }

#define BAP_LLVM_IMAGE_PROPERTY_I(return_type, field, property)                       \
    return_type bap_llvm_image_##field##_##property(const img::image* m, size_t i) {  \
        assert(i < m->field##s.size());                                               \
        return m->field##s[i].property;                                               \
    }

#define BAP_LLVM_IMAGE_PROPERTY_I_NAME(return_type, field, property)                 \
    return_type bap_llvm_image_##field##_##property(const img::image* m, size_t i) { \
        assert(i < m->field##s.size());                                              \
        return m->field##s[i].property.c_str();                                      \
    }

    const img::image* bap_llvm_image_create(const char* data, size_t size) { return img::create(data, size); }
    void bap_llvm_image_destroy(const img::image* m) { delete m; }
    const char* bap_llvm_image_arch(const img::image* m) { return m->arch.c_str(); }
    uint64_t bap_llvm_image_entry(const img::image* m) { return m->entry; }

    BAP_LLVM_IMAGE_PROPERTY(size_t, segments_number, segments, size())
    BAP_LLVM_IMAGE_PROPERTY(size_t, sections_number, sections, size())
    BAP_LLVM_IMAGE_PROPERTY(size_t, symbols_number, symbols, size())

    BAP_LLVM_IMAGE_PROPERTY_I_NAME(const char *, segment, name)
    BAP_LLVM_IMAGE_PROPERTY_I(uint64_t,  segment, offset)
    BAP_LLVM_IMAGE_PROPERTY_I(uint64_t, segment, addr)
    BAP_LLVM_IMAGE_PROPERTY_I(uint64_t, segment, size)
    BAP_LLVM_IMAGE_PROPERTY_I(bool, segment, is_readable)
    BAP_LLVM_IMAGE_PROPERTY_I(bool, segment, is_writable)
    BAP_LLVM_IMAGE_PROPERTY_I(bool, segment, is_executable)

    BAP_LLVM_IMAGE_PROPERTY_I_NAME(const char *, section, name)
    BAP_LLVM_IMAGE_PROPERTY_I(uint64_t, section, addr)
    BAP_LLVM_IMAGE_PROPERTY_I(uint64_t, section, size)

    BAP_LLVM_IMAGE_PROPERTY_I_NAME(const char *, symbol, name)
    BAP_LLVM_IMAGE_PROPERTY_I(uint64_t, symbol, addr)
    BAP_LLVM_IMAGE_PROPERTY_I(uint64_t, symbol, size)
    BAP_LLVM_IMAGE_PROPERTY_I(bool, symbol, is_fun)
    BAP_LLVM_IMAGE_PROPERTY_I(bool, symbol, is_debug)

}
