#ifndef LLVM_LOADER_HPP
#define LLVM_LOADER_HPP

#include <iostream>

#include "llvm_error_or.hpp"
#include "llvm_coff_loader.hpp"
#include "llvm_elf_loader.hpp"
#include "llvm_macho_loader.hpp"

static std::string scheme =
    "(declare abi (name str))\n"
    "(declare arch (name str))\n"
    "(declare bits (size int))\n"
    "(declare is-little-endian (flag bool))\n"
    "(declare subarch (name str))\n"
    "(declare system (name str))\n"
    "(declare vendor (name str))\n"
    "(declare llvm:code-entry (name str) (off int) (size int))\n"
    "(declare llvm:base-address (addr int))\n"
    "(declare llvm:entry-point (addr int))\n"
    "(declare llvm:file-type (name str))\n"
    "(declare llvm:macho-symbol (name str) (value int))\n"
    "(declare llvm:elf-program-header-flags (name str) (ld bool) (r bool) (w bool) (x bool))\n"
    "(declare llvm:elf-program-header (name str) (off int) (size int))\n"
    "(declare llvm:name-reference (at int) (name str))\n"
    "(declare llvm:relocation (at int) (addr int))\n"
    "(declare llvm:section-entry (name str) (addr int) (size int) (off int))\n"
    "(declare llvm:section-flags (name str) (r bool) (w bool) (x bool))\n"
    "(declare llvm:segment-command-flags (name str) (r bool) (w bool) (x bool))\n"
    "(declare llvm:segment-command (name str) (off int) (size int))\n"
    "(declare llvm:symbol-entry (name str) (addr int) (size int) (off int) (value int))\n"
    "(declare llvm:elf-virtual-program-header (name str) (addr int) (size int))\n"
    "(declare llvm:coff-virtual-section-header (name str) (addr int) (size int))\n"
    "(declare llvm:virtual-segment-command (name str) (addr int) (size int))\n";


namespace loader {

using namespace llvm;
using namespace llvm::object;

error_or<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (!binary)
        return failure(toString(binary.takeError()));
    error_or<object::Binary> v(binary->release());
    return v;
}

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
