#include <stdio.h>
#include "llvm_loader.hpp"

extern "C" {

    const loader::bap_llvm_loader * bap_llvm_loader_create(const char* data, size_t size) {
        return loader::create(data, size);
    }

    const char * bap_llvm_loader_data(const loader::bap_llvm_loader *loader) {
        return loader->get()->c_str();
    }

    bool bap_llvm_loader_failed(const loader::bap_llvm_loader *loader) {
        return loader->has_error();
    }

    bool bap_llvm_file_not_supported(const loader::bap_llvm_loader *loader) {
        return loader::file_not_supported(loader);
    }

    void bap_llvm_loader_destroy(const loader::bap_llvm_loader *loader) {
        loader::destroy(loader);
    }
}
