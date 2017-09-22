#ifndef LLVM_LOADER_H
#define LLVM_LOADER_H

#include <stdbool.h>

struct bap_llvm_loader;

const struct bap_llvm_loader* bap_llvm_loader_create(const char*, size_t);
const char* bap_llvm_loader_data(const struct bap_llvm_loader*);
bool bap_llvm_loader_failed(const struct bap_llvm_loader*);
bool bap_llvm_file_not_supported(const struct bap_llvm_loader*);
void bap_llvm_loader_destroy(const struct bap_llvm_loader*);

#endif // LLVM_LOADER_H
