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

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
#include <llvm/ADT/iterator_range.h>
#include <llvm/Object/SymbolSize.h>
#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4
#include <llvm/ADT/OwningPtr.h>
#else
#error LLVM version not supported.
#endif

using std::move;

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
using std::error_code;
using std::distance;
#else
using llvm::error_code;
#endif

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

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
template <typename T>
T value_or_default(const llvm::ErrorOr<T> &e, T def=T()) {
    if (e) return e.get();
    return def;
}
#else

#endif

} // namespace

namespace seg {
using namespace llvm;
using namespace llvm::object;

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
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

iterator_range<MachOObjectFile::load_command_iterator> load_commands(const MachOObjectFile &obj) {
    return obj.load_commands();
}

ObjectFile::section_iterator_range sections(const COFFObjectFile &obj) {
    return obj.sections();
}
#else

#endif

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
std::vector<segment> read(const COFFObjectFile& obj) {
    std::vector<segment> segments;
    uint64_t image_base = obj.getImageBase();
    for (auto it : sections(obj)) {
        const coff_section *s = obj.getCOFFSection(it);
        uint64_t c = static_cast<uint64_t>(s->Characteristics);
        if ( c & COFF::IMAGE_SCN_CNT_CODE ||
             c & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA ||
             c & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA )
            segments.push_back(make_segment(*s, image_base));
    }
    return segments;
}
#else

#endif

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
symbol make_symbol(const SymbolRef& sym, uint64_t size) {
    auto name = value_or_default(sym.getName()).str();
    auto addr = value_or_default(sym.getAddress());
    return symbol{name, sym.getType(), addr, size}; 
}

std::vector<symbol> read(const ObjectFile& obj) {
    std::vector<symbol> symbols;
    auto symbol_sizes = computeSymbolSizes(obj);
    for (auto s : symbol_sizes) 
	symbols.push_back(make_symbol(s.first, s.second));
    return symbols;
}

std::vector<symbol> read(const COFFObjectFile& obj) {
    std::vector<symbol> symbols;
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
        
        symbols.push_back(make_symbol(*it, size));
    }
    return symbols;
}
#else

#endif

} //namespace sym

namespace sec {
using namespace llvm;
using namespace llvm::object;

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
using namespace llvm;
using namespace llvm::object;

section make_section(const SectionRef &sec) {
    StringRef name;
    if (error_code ec = sec.getName(name))
        llvm_binary_fail(ec);

    return section{name.str(), sec.getAddress(), sec.getSize()};
}

section make_section(const coff_section &s, const uint64_t image_base) {
    return section{s.Name, s.VirtualAddress + image_base, s.SizeOfRawData};
}

section_iterator begin_sections(const ObjectFile &obj) {
    return obj.sections().begin();
}

section_iterator end_sections(const ObjectFile &obj) {
    return obj.sections().end();
}

std::vector<section> read(const COFFObjectFile &obj) {
    auto size = distance(begin_sections(obj), end_sections(obj));
    std::vector<section> sections;
    uint64_t image_base = obj.getImageBase();
    sections.reserve(size);
    std::transform(begin_sections(obj),
                   end_sections(obj),
                   std::back_inserter(sections),
                   [&obj, image_base](const SectionRef& s) { return make_section(*obj.getCOFFSection(s), image_base); });
    return sections;
}
#else

#endif

} //namespace sec

namespace img {
using namespace llvm;
using namespace llvm::object;

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
uint64_t image_entry(const MachOObjectFile& obj) {
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
#else

#endif

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
uint64_t image_entry(const COFFObjectFile& obj) {
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
#else

#endif

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
image* create(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (error_code ec = binary.getError()) {
	std::cerr << ec << "\n";
        return NULL;    
    }
    return create(move(*binary));
}
#else

#endif

} //namespace img

#endif //LLVM_BINARY_HPP
