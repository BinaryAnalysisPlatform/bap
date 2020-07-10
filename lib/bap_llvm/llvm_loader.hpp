#ifndef LLVM_LOADER_HPP
#define LLVM_LOADER_HPP

#include <iostream>

#include "llvm_error_or.hpp"
#include "llvm_coff_loader.hpp"
#include "llvm_elf_loader.hpp"
#include "llvm_macho_loader.hpp"

static std::string scheme =
    "(declare arch (name str))\n"
    "(declare subarch (name str))\n"
    "(declare vendor (name str))\n"
    "(declare system (name str))\n"
    "(declare abi (name str))\n"
    "(declare bits (size int))\n"
    "(declare is-little-endian (flag bool))\n"
    "(declare code-entry (name str) (off int) (size int))\n"
    "(declare default-base-address (addr int))\n"
    "(declare entry (relative-addr int))\n"
    "(declare file-type (name str))\n"
    "(declare function (off int) (name str))\n"
    "(declare macho-symbol (name str) (value int))\n"
    "(declare plt-entry (name str) (relative-addr int) (size int) (off int))\n"
    "(declare program-header-flags (name str) (ld bool) (r bool) (w bool) (x bool))\n"
    "(declare program-header (name str) (off int) (size int))\n"
    "(declare ref-external (rel-off int) (name str))\n"
    "(declare ref-internal (sym-off int) (rel-off int))\n"
    "(declare relocatable (flag bool))\n"
    "(declare section-entry (name str) (relative-addr int) (size int) (off int))\n"
    "(declare section-flags (name str) (r bool) (w bool) (x bool))\n"
    "(declare segment-command-flags (name str) (r bool) (w bool) (x bool))\n"
    "(declare segment-command (name str) (off int) (size int))\n"
    "(declare symbol-entry (name str) (relative-addr int) (size int) (off int))\n"
    "(declare virtual-program-header (name str) (relative-addr int) (size int))\n"
    "(declare virtual-section-header (name str) (relative-addr int) (size int))\n"
    "(declare virtual-segment-command (name str) (relative-addr int) (size int))\n";


namespace loader {

using namespace llvm;
using namespace llvm::object;

#if LLVM_VERSION_MAJOR >= 4

error_or<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (!binary)
        return failure(toString(binary.takeError()));
    error_or<object::Binary> v(binary->release());
    return v;
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

error_or<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (auto ec = binary.getError())
        return failure(ec.message());
    error_or<object::Binary> v(binary->release());
    return v;
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4
error_or<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBuffer* buff(MemoryBuffer::getMemBufferCopy(data_ref, "binary"));
    OwningPtr<object::Binary> bin;
    if (error_code ec = createBinary(buff, bin))
        return failure(ec.message());
    return error_or<object::Binary>(bin.take());
}

#else
#error LLVM version is not supported
#endif

error_or<std::string> unsupported_filetype() { return success(std::string("")); }

template <typename T>
error_or<std::string> load_base(ogre_doc &s, const object::Binary *binary) {
    if (auto bin = llvm::dyn_cast<T>(binary))
        return load(s, *bin);
    else
        return unsupported_filetype();
}

error_or<std::string> load_elf(ogre_doc &s, const object::Binary *binary) {
    if (isa<ELF32LEObjectFile>(*binary))
        return load_base<ELF32LEObjectFile>(s, binary);
    else if (isa<ELF32BEObjectFile>(*binary))
        return load_base<ELF32BEObjectFile>(s, binary);
    else if (isa<ELF64LEObjectFile>(*binary))
        return load_base<ELF64LEObjectFile>(s, binary);
    else if (isa<ELF64BEObjectFile>(*binary))
        return load_base<ELF64BEObjectFile>(s, binary);
    else
        return unsupported_filetype();
}

error_or<std::string> load_coff(ogre_doc &s,const object::Binary *binary, const char * pdb_path) {
     if (auto bin = llvm::dyn_cast<COFFObjectFile>(binary))
         return load(s, *bin, pdb_path);
    else
        return unsupported_filetype();
}

error_or<std::string> load_macho(ogre_doc &s, const object::Binary *binary) {
    return load_base<MachOObjectFile>(s, binary);
}

template <typename T>
void verbose_fails(const error_or<T> &loaded) {
    if(const char* env_p = std::getenv("BAP_DEBUG")) {
        if (std::string(env_p) == "1" || std::string(env_p) == "true")
            if (!loaded)
                std::cerr << "ogre llvm loader error: " << loaded.message() << std::endl;
        for (auto w : loaded.warnings())
            std::cerr << "ogre llvm loader warning: " << w << std::endl;
    }
}

void emit_common_header(ogre_doc &s, const object::ObjectFile *obj) {
    s.raw_entry(scheme);
    auto target = obj->makeTriple();
    s.entry("arch") << Triple::getArchTypeName(target.getArch());
    s.entry("subarch") << prim::string_of_subarch(target.getSubArch());
    s.entry("vendor") << target.getVendorName();
    s.entry("system") << target.getOSName();
    s.entry("abi") << prim::string_of_abi(target.getEnvironment());
    s.entry("bits") << (obj->getBytesInAddress() * 8);
    s.entry("is-little-endian") << target.isLittleEndian();
}

error_or<std::string> load(const char* data, std::size_t size, const char * pdb_path) {
    error_or<object::Binary> result = get_binary(data, size);
    if (!result) { verbose_fails(result); return unsupported_filetype(); }
    auto object = llvm::dyn_cast<object::ObjectFile>(result.get());
    if (!object) return unsupported_filetype();
    ogre_doc s;
    emit_common_header(s, object);
    if (object->isCOFF())   return load_coff(s, object, pdb_path);
    else if (object->isELF())    return load_elf(s, object);
    else if (object->isMachO())  return load_macho(s, object);
    else return unsupported_filetype();
}

typedef error_or<std::string> bap_llvm_loader;

const bap_llvm_loader * create(const char* data, std::size_t size, const char *pdb_path) {
    error_or<std::string> loaded = load(data, size, pdb_path);
    verbose_fails(loaded);
    return new bap_llvm_loader(std::move(loaded));
}

bool file_not_supported(const bap_llvm_loader * loader) {
    return (!loader->has_error() && (*loader)->size() == 0);
}

void destroy(const bap_llvm_loader *loader) { delete loader; }

} // namespace loader

#endif // LLVM_LOADER_HPP
