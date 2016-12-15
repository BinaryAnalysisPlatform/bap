#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

#ifndef LLVM_BINARY_38_HPP
#define LLVM_BINARY_38_HPP

#include <memory>
#include <numeric>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>

#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/Archive.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Config/llvm-config.h>

#include <llvm/ADT/iterator_range.h>
#include <llvm/Object/SymbolSize.h>

using std::move;
using std::error_code;
using std::distance;

// TODO: move llvm_binary_fail function out of LLVM version specific files
extern "C" void llvm_binary_fail(const char*) LLVM_ATTRIBUTE_NORETURN ;

LLVM_ATTRIBUTE_NORETURN void llvm_binary_fail (const error_code ec) {
    llvm_binary_fail(ec.message().c_str());
}

namespace llvm { namespace object {

template <typename T>
content_iterator<T>& operator++(content_iterator<T>& a) {
    error_code ec;
    a.increment(ec);
    if(ec) llvm_binary_fail(ec);
    return a;
}

}} //namespace llvm::object

namespace {
using namespace llvm;
using namespace llvm::object;

template <typename T>
T value_or_default(const llvm::ErrorOr<T> &e, T def=T()) {
    if (e) return e.get();
    return def;
}

iterator_range<MachOObjectFile::load_command_iterator> load_commands(const MachOObjectFile &obj) {
    return obj.load_commands();
}

uint64_t getImageBase(const COFFObjectFile &obj) {
    return obj.getImageBase();
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

ObjectFile::section_iterator_range sections(const COFFObjectFile &obj) {
    return obj.sections();
}

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

typedef SymbolRef::Type kind_type;

std::string name_or_default(const SymbolRef &sym) {
    return value_or_default(sym.getName()).str();
}

uint64_t addr_or_default(const SymbolRef &sym) {
    return value_or_default(sym.getAddress());
}

uint64_t addr_or_default(const SymbolRef &sym, const COFFObjectFile &obj) {
    return value_or_default(sym.getAddress());
}

kind_type get_type(const SymbolRef &sym) {
    return sym.getType();
}

std::vector<std::pair<SymbolRef, uint64_t>> getSymbolSizes(const ObjectFile &obj) {
    return computeSymbolSizes(obj);
}

template <typename ELFT>
std::vector<std::pair<SymbolRef, uint64_t>> getSymbolSizes(const ELFObjectFile<ELFT> &obj) {
    typedef typename ELFFile<ELFT>::Elf_Shdr sec_hdr;
    
    std::size_t sym_count = std::distance(obj.symbol_begin(), obj.symbol_end());
    auto sections = obj.getELFFile()->sections();   
    bool is_dyn = std::any_of(sections.begin(), sections.end(),
                              [](const sec_hdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });
    if (!sym_count && !is_dyn)
        return std::vector<std::pair<SymbolRef, uint64_t>>();
    return computeSymbolSizes(obj);
}

std::vector<std::pair<SymbolRef, uint64_t>> getSymbolSizes(const COFFObjectFile& obj) {
    std::vector<std::pair<SymbolRef, uint64_t>> symbol_sizes;
    for (auto it = obj.symbol_begin(); it != obj.symbol_end(); ++it) {
        auto sym = obj.getCOFFSymbol(*it);

        const coff_section *sec = nullptr;
        if (sym.getSectionNumber() == COFF::IMAGE_SYM_UNDEFINED)
            continue;

        if (error_code ec = obj.getSection(sym.getSectionNumber(), sec))
            llvm_binary_fail(ec);

        if (!sec) continue;

        uint64_t size = (sec->VirtualAddress + sec->SizeOfRawData) - sym.getValue();

        for (auto it = obj.symbol_begin(); it != obj.symbol_end(); ++it) {
            auto next = obj.getCOFFSymbol(*it);
            if (next.getSectionNumber() == sym.getSectionNumber()) {
                auto new_size = next.getValue() > sym.getValue() ?
                    next.getValue() - sym.getValue() : size;
                size = new_size < size ? new_size : size;
            }
        }

        symbol_sizes.push_back(std::make_pair(*it, size));
    }
    return symbol_sizes;
}

} //namespace sym

namespace sec {
using namespace llvm;
using namespace llvm::object;

std::string getName(const SectionRef &sec) {
    StringRef name;
    if (error_code ec = sec.getName(name))
        llvm_binary_fail(ec);

    return name.str();
}

uint64_t getAddr(const SectionRef &sec) {
    return sec.getAddress();
}

uint64_t getSize(const SectionRef &sec) {
    return sec.getSize();
}

std::string getName(const coff_section &s) {
    return s.Name;
}

uint64_t getAddr(const coff_section &s) {
    return s.VirtualAddress;
}

uint64_t getSize(const coff_section &s) {
    return s.SizeOfRawData;
}

section_iterator begin_sections(const ObjectFile &obj) {
    return obj.sections().begin();
}

section_iterator end_sections(const ObjectFile &obj) {
    return obj.sections().end();
}

ObjectFile::section_iterator_range obj_sections(const ObjectFile &obj) {
    return obj.sections();
}

const coff_section*
getCOFFSection(const COFFObjectFile &obj, section_iterator it) {
    return obj.getCOFFSection(*it);
}

} //namespace sec

namespace img {
using namespace llvm;
using namespace llvm::object;

uint64_t image_entry_macho(const MachOObjectFile& obj) {
    typedef MachOObjectFile::LoadCommandInfo command_info;
    auto it =
        std::find_if(obj.begin_load_commands(), obj.end_load_commands(),
                     [](const command_info &info){
                         return
                         info.C.cmd == MachO::LoadCommandType::LC_MAIN;});
    if (it != obj.end_load_commands()) {
        const MachO::entry_point_command *entry_cmd =
            reinterpret_cast<const MachO::entry_point_command*>(it->Ptr);
        return entry_cmd->entryoff;
    } else {
        llvm_binary_fail("LC_MAIN not found, binary version < 10.8");
    }
}

uint64_t image_entry_coff(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (error_code ec = obj.getPE32Header(hdr))
	    llvm_binary_fail(ec);
        if (!hdr)
            llvm_binary_fail("PE header not found");
        return hdr->AddressOfEntryPoint + hdr->ImageBase;
    } else {
        const pe32plus_header *hdr = 0;
        if (error_code ec = obj.getPE32PlusHeader(hdr))
            llvm_binary_fail(ec);
        if (!hdr)
            llvm_binary_fail("PE+ header not found");
        return hdr->AddressOfEntryPoint + hdr->ImageBase;
    }
}

std::unique_ptr<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (error_code ec = binary.getError()) {
        std::cerr << ec.message() << std::endl;
        return NULL;
    }
    return move(*binary);
}

} //namespace img

using std::distance;

#endif //LLVM_BINARY_38_HPP
#endif // LLVM=3.8
