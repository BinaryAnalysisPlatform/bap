#ifndef BAP_LLVM_BINARY_STUBS_HPP
#define BAP_LLVM_BINARY_STUBS_HPP

#include <memory>
#include <stdexcept>
#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/Archive.h>
extern "C" {
#include <caml/fail.h>
}


namespace utils {
using namespace llvm;
using namespace llvm::object;

//Extracto. Extracts require values from binary
struct extractor_base {
    virtual uint64_t entry() const = 0;
    virtual Triple::ArchType arch() const = 0;
    virtual ~extractor_base() {}
};

template <typename T>
struct extractor_objfile : extractor_base {
    explicit extractor_objfile(const T* obj) : obj_(obj) {}
    Triple::ArchType arch() const {
        return static_cast<Triple::ArchType>(obj_->getArch());
    }
protected:
    const T *obj_;
};

template <typename T>
struct extractor;

//ELF extractor
template <typename ELFT>
struct extractor< ELFObjectFile<ELFT> > : extractor_objfile< ELFObjectFile<ELFT> > {
    explicit extractor(const ELFObjectFile<ELFT> *obj)
        : extractor_objfile< ELFObjectFile<ELFT> >(obj) {}

    uint64_t entry() const {
        return this -> obj_ -> getELFFile() -> getHeader() -> e_entry;
    }
};

//MachO extractor
template <>
struct extractor<MachOObjectFile> : extractor_objfile<MachOObjectFile> {
    explicit extractor(const MachOObjectFile *obj)
        : extractor_objfile<MachOObjectFile>(obj) {}
    uint64_t entry() const { return 42; };
};

//COFF extractor
template <>
struct extractor<COFFObjectFile> : extractor_objfile<COFFObjectFile> {
    explicit extractor(const COFFObjectFile *obj)
        : extractor_objfile<COFFObjectFile>(obj) {}
    uint64_t entry() const { return 42; };
};

template <typename T>
std::shared_ptr<extractor_base> create_extractor(const ObjectFile* obj) {
    if (const T* ptr = dyn_cast<T>(obj))
        return std::make_shared< extractor<T> >(ptr);
    ::caml_invalid_argument("Unrecognized object format");
}

std::shared_ptr<extractor_base> create_extractor_elf(const ObjectFile* obj) {
    if (const ELF32LEObjectFile *elf = dyn_cast<ELF32LEObjectFile>(obj))
        return create_extractor<ELF32LEObjectFile>(elf);

    if (const ELF32BEObjectFile *elf = dyn_cast<ELF32BEObjectFile>(obj))
        return create_extractor<ELF32BEObjectFile>(elf);

    if (const ELF64LEObjectFile *elf = dyn_cast<ELF64LEObjectFile>(obj))
        return create_extractor<ELF64LEObjectFile>(elf);

    if (const ELF64BEObjectFile *elf = dyn_cast<ELF64BEObjectFile>(obj))
        return create_extractor<ELF64BEObjectFile>(elf);
    ::caml_invalid_argument("Unrecognized ELF format");
}

std::shared_ptr<extractor_base> create_extractor(const ObjectFile* obj) {
    if (obj->isCOFF())
        return create_extractor<COFFObjectFile>(obj);
    if (obj->isELF())
        return create_extractor_elf(obj);
    if (obj->isMachO())
        return create_extractor<MachOObjectFile>(obj);
    ::caml_invalid_argument("Unrecognized object format");            
}

std::shared_ptr<extractor_base> create_extractor(const Archive* arch) {
    ::caml_failwith("Archive loading unimplemented");
}

std::shared_ptr<extractor_base> create_extractor(const Binary* binary) {
    if (const Archive *arch = dyn_cast<Archive>(binary))
        return create_extractor(arch);
    if (const ObjectFile *obj = dyn_cast<ObjectFile>(binary))
        return create_extractor(obj);
    ::caml_invalid_argument("Unrecognized binary format");
}
} //namespace elf_utils

#endif //BAP_LLVM_BINARY_STUBS_HPP
