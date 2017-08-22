#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8          \
    || LLVM_VERSION_MAJOR == 4 && LLVM_VERSION_MINOR == 0

#ifndef LLVM_BINARY_38_HPP
#define LLVM_BINARY_38_HPP

#include <memory>
#include <numeric>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <tuple>

#if LLVM_VERSION_MAJOR == 4 && LLVM_VERSION_MINOR == 0
#include <llvm/Support/Error.h>
#endif

#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/Archive.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Config/llvm-config.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Object/SymbolSize.h>

#include "llvm_error_or.hpp"

using std::error_code;

namespace {
using namespace llvm;
using namespace llvm::object;

iterator_range<MachOObjectFile::load_command_iterator> load_commands(const MachOObjectFile &obj) {
    return obj.load_commands();
}

error_or<uint64_t> getImageBase(const COFFObjectFile &obj) {
    return success(obj.getImageBase());
}

} // namespace

namespace seg {
using namespace llvm;
using namespace llvm::object;

template <typename ELFT>
const typename ELFFile<ELFT>::Elf_Phdr* elf_header_begin(const ELFFile<ELFT> *elf) {
    return elf->program_header_begin();
}

template <typename ELFT>
const typename ELFFile<ELFT>::Elf_Phdr* elf_header_end(const ELFFile<ELFT> *elf) {
    return elf->program_header_end();
}

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

typedef SymbolRef::Type kind_type;
typedef std::vector<std::pair<SymbolRef, uint64_t>> symbol_sizes;

template <typename T>
error_or<T> of_llvm_error_or(const llvm::ErrorOr<T> &e) {
    if (!e)
        return failure(e.getError().message());
    return success(e.get());
}

#if LLVM_VERSION_MAJOR == 4 && LLVM_VERSION_MINOR == 0
template <typename T>
error_or<T> of_llvm_error_or(llvm::Expected<T> &e) {
    if (!e) {
        return failure(llvm::toString(e.takeError()));
    }
    return success(e.get());
}
#endif

error_or<std::string> get_name(const SymbolRef &sym) {
    auto er_name = sym.getName();
    auto e = of_llvm_error_or(er_name);
    return map_value<std::string>(e, [](const StringRef &x){return x.str();});
}

error_or<uint64_t> get_addr(const SymbolRef &sym, const ObjectFile &) {
    auto er_addr = sym.getAddress();
    return of_llvm_error_or(er_addr);
}

error_or<uint64_t> get_addr(const SymbolRef &sym, const COFFObjectFile &obj) {
    auto er_addr = sym.getAddress();
    return of_llvm_error_or(er_addr);
}

error_or<kind_type> get_kind(const SymbolRef &sym) {
    auto er_type = sym.getType();
    return success(er_type);
}

error_or<symbol_sizes> getSymbolSizes(const ObjectFile &obj) {
    return success(computeSymbolSizes(obj));
}

template <typename ELFT>
error_or<symbol_sizes> getSymbolSizes(const ELFObjectFile<ELFT> &obj) {
    typedef typename ELFFile<ELFT>::Elf_Shdr sec_hdr;

    symbol_sizes syms;
    for (auto sym : obj.symbols())
        syms.push_back({sym, sym.getSize()});

    auto sections = obj.getELFFile()->sections();
    bool is_dyn = std::any_of(sections.begin(), sections.end(),
                              [](const sec_hdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });

    if (!syms.size() && !is_dyn)
        return success(symbol_sizes());

    if (is_dyn)  // we aren't able to rely on iterators because of bug in llvm
        for (auto sym : obj.getDynamicSymbolIterators())
            syms.push_back({sym, sym.getSize()});

    return success(syms);
}

error_or<symbol_sizes> getSymbolSizes(const COFFObjectFile& obj) {
    symbol_sizes sizes;
    for (symbol_iterator it : obj.symbols()) {
        auto sym = obj.getCOFFSymbol(*it);
        const coff_section *sec = nullptr;
        if (sym.getSectionNumber() == COFF::IMAGE_SYM_UNDEFINED)
            continue;
        if (error_code ec = obj.getSection(sym.getSectionNumber(), sec))
            return failure(ec.message());
        if (!sec) continue;

        uint64_t size = (sec->VirtualAddress + sec->SizeOfRawData) - sym.getValue();

        for (symbol_iterator it : obj.symbols()) {
            auto next = obj.getCOFFSymbol(*it);
            if (next.getSectionNumber() == sym.getSectionNumber()) {
                auto new_size = next.getValue() > sym.getValue() ?
                    next.getValue() - sym.getValue() : size;
                size = new_size < size ? new_size : size;
            }
        }

        sizes.push_back(std::make_pair(*it, size));
    }
    return success(std::move(sizes));
}

} //namespace sym

namespace sec {
using namespace llvm;
using namespace llvm::object;

typedef std::tuple<std::string, uint64_t, uint64_t> section_descr;

error_or<std::string> getName(const SectionRef &sec) {
    StringRef name;
    if (error_code ec = sec.getName(name))
        return failure(ec.message());
    return success(name.str());
}

std::string getName(const coff_section &s) { return s.Name;           }
uint64_t    getAddr(const SectionRef &sec) { return sec.getAddress(); }
uint64_t    getSize(const SectionRef &sec) { return sec.getSize();    }
uint64_t    getAddr(const coff_section &s) { return s.VirtualAddress; }
uint64_t    getSize(const coff_section &s) { return s.SizeOfRawData;  }

error_or<section_descr> get_section(const SectionRef &sec) {
    auto name = getName(sec);
    if (!name) return name;
    auto d = std::make_tuple(*name, getAddr(sec), getSize(sec));
    return std::move(success(d) << name.warnings());
}

section_descr get_section(const coff_section &s) {
    return std::make_tuple(getName(s), getAddr(s), getSize(s));
}

error_or<ObjectFile::section_iterator_range> sections_range(const ObjectFile &obj) {
    return success(obj.sections());
}

const coff_section* getCOFFSection(const COFFObjectFile &obj, section_iterator it) {
    return obj.getCOFFSection(*it);
}

} //namespace sec

namespace img {
using namespace llvm;
using namespace llvm::object;

bool is_main(const MachOObjectFile::LoadCommandInfo &info) {
    return (info.C.cmd == MachO::LoadCommandType::LC_MAIN);
}

error_or<uint64_t> image_entry_macho(const MachOObjectFile& obj) {
    auto it =
        std::find_if(obj.begin_load_commands(), obj.end_load_commands(), is_main);
    if (it == obj.end_load_commands())
        return failure("LC_MAIN not found, binary version < 10.8");
    const MachO::entry_point_command *entry_cmd =
        reinterpret_cast<const MachO::entry_point_command*>(it->Ptr);
    return success(entry_cmd->entryoff);
}

error_or<uint64_t> image_entry_coff(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (error_code ec = obj.getPE32Header(hdr))
            return failure(ec.message());
        if (!hdr) return failure("PE header not found");
        return error_or<uint64_t>(hdr->AddressOfEntryPoint + hdr->ImageBase);
    } else {
        const pe32plus_header *hdr = 0;
        if (error_code ec = obj.getPE32PlusHeader(hdr))
            return failure(ec.message());
        if (!hdr) return failure("PE+ header not found");
        return error_or<uint64_t>(hdr->AddressOfEntryPoint + hdr->ImageBase);
    }
}

error_or<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (error_code ec = binary.getError())
        return failure(ec.message());
    error_or<object::Binary> v(binary->release());
    return v;
}

} //namespace img

#endif //LLVM_BINARY_38_HPP
#endif // LLVM=3.8
