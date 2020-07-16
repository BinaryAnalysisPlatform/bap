#include <iostream>

#include <llvm/ADT/Triple.h>

#include "llvm_primitives.hpp"

namespace prim {
using namespace llvm;
using namespace llvm::object;

// some cases are commented out because they are not supported
// by all versions of LLVM, we will later use a macro to enable
// them depending on the version.
std::string string_of_subarch(Triple::SubArchType sub) {
    switch (sub) {
    case Triple::NoSubArch: return "";
    // case Triple::ARMSubArch_v8_6a: return "v8.6-a";
    // case Triple::ARMSubArch_v8_5a: return "v8.5-a";
    // case Triple::ARMSubArch_v8_4a: return "v8.4-a";
    // case Triple::ARMSubArch_v8_3a: return "v8.3-a";
    // case Triple::ARMSubArch_v8_2a: return "v8.2-a";
    // case Triple::ARMSubArch_v8_1a: return "v8.1-a";
    case Triple::ARMSubArch_v8:    return "v8";
    // case Triple::ARMSubArch_v8r:   return "v8-r";
    // case Triple::ARMSubArch_v8m_baseline: return "v8-m.base";
    // case Triple::ARMSubArch_v8m_mainline: return "v8-m.main";
    // case Triple::ARMSubArch_v8_1m_mainline: return "v8.1-m.base",
    case Triple::ARMSubArch_v7: return "v7";
    case Triple::ARMSubArch_v7em: return "v7e-m";
    case Triple::ARMSubArch_v7m: return "v7-m";
    case Triple::ARMSubArch_v7s: return "v7s";
    case Triple::ARMSubArch_v7k: return "v7k";
    case Triple::ARMSubArch_v7ve: return "v7ve";
    case Triple::ARMSubArch_v6: return "v6";
    case Triple::ARMSubArch_v6m: return "v6-m";
    case Triple::ARMSubArch_v6k: return "v6k";
    case Triple::ARMSubArch_v6t2: return "v6t2";
    case Triple::ARMSubArch_v5: return "v5";
    case Triple::ARMSubArch_v5te: return "v5te";
    case Triple::ARMSubArch_v4t: return "v4t";
    case Triple::KalimbaSubArch_v5: return "v5";
    case Triple::KalimbaSubArch_v3: return "v3";
    case Triple::KalimbaSubArch_v4: return "v4";
    // case Triple::MipsSubArch_r6: return "r6";
    // case Triple::PPCSubArch_spe: return "spe";
    default: return "";
    }
}

// we need it because Triple::getEnvironmentName() is broken and
// returns the system instead.
std::string string_of_abi(Triple::EnvironmentType abi) {
    switch (abi) {
    case Triple::UnknownEnvironment: return "unknown";
    case Triple::GNU: return "gnu";
    case Triple::GNUABIN32: return "gnuabin32";
    case Triple::GNUABI64: return "gnuabi64";
    case Triple::GNUEABI: return "gnueabi";
    case Triple::GNUEABIHF: return "gnueabihf";
    case Triple::GNUX32: return "gnux32";
    case Triple::CODE16: return "code16";
    case Triple::EABI: return "eabi";
    case Triple::EABIHF: return "eabihf";
    case Triple::Android: return "android";
    case Triple::Musl: return "musl";
    case Triple::MuslEABI: return "musleabi";
    case Triple::MuslEABIHF: return "musleabihf";
    case Triple::MSVC: return "msvc";
    case Triple::Itanium: return "itanium";
    case Triple::Cygnus: return "cygnus";
    case Triple::CoreCLR: return "coreclr";
    case Triple::Simulator: return "simulator";
  //case Triple::MacABI: return "macabi";
    default: return "unknown";
    }
}

template <typename T>
std::string error_message(Expected<T> &e) {
    return toString(e.takeError());
}

error_or<SymbolRef::Type> symbol_type(const SymbolRef &s) {
    auto typ = s.getType();
    if (typ) return success(*typ);
    else return failure(error_message(typ));
}

#if LLVM_VERSION_MAJOR >= 10
error_or<std::string> section_name(const SectionRef &sec) {
    auto name = sec.getName();
    if (name) return success(name->str());
    else return failure(error_message(name));
}

error_or<section_iterator> relocated_section(const SectionRef &sec) {
    auto rel_sec = sec.getRelocatedSection();
    if (rel_sec) return success(*rel_sec);
    else return failure(error_message(rel_sec));
}

#else

error_or<std::string> section_name(const SectionRef &sec) {
    StringRef name;
    auto er = sec.getName(name);
    if (!er) return success(name.str());
    else return failure(er.message());
}

error_or<section_iterator> relocated_section(const SectionRef &sec) {
    return success(sec.getRelocatedSection());
}

#endif



const char* get_raw_data(const ObjectFile &obj) {
    return obj.getMemoryBufferRef().getBufferStart();
}

symbol_iterator begin_symbols(const ObjectFile &obj) { return obj.symbol_begin(); }
symbol_iterator end_symbols(const ObjectFile &obj) { return obj.symbol_end(); }
section_iterator begin_sections(const ObjectFile &obj) { return obj.section_begin(); }
section_iterator end_sections(const ObjectFile &obj) { return obj.section_end(); }

error_or<uint64_t> symbol_address(const SymbolRef &sym) {
    auto addr = sym.getAddress();
    if (addr) return success(*addr);
    else return failure(error_message(addr));
}

error_or<std::string> symbol_name(const SymbolRef &s) {
    auto name = s.getName();
    if (name) return success(name->str());
    else return failure(error_message(name));
}

error_or<section_iterator> symbol_section(const ObjectFile &obj, const SymbolRef &s) {
    auto sec = s.getSection();
    if (sec) return success(*sec);
    else return failure(error_message(sec));
}

uint64_t relocation_offset(const RelocationRef &rel) { return rel.getOffset(); }

std::vector<RelocationRef> relocations(const SectionRef &sec) {
    auto r = sec.relocations();
    return std::vector<RelocationRef>(r.begin(), r.end());
}

std::vector<SectionRef> sections(const ObjectFile &obj) {
    auto s = obj.sections();
    return std::vector<SectionRef>(s.begin(), s.end());
}

std::vector<SymbolRef> symbols(const ObjectFile &obj) {
    auto s = obj.symbols();
    return std::vector<SymbolRef>(s.begin(), s.end());
}

error_or<uint64_t> section_address(const SectionRef &sec) {
    return success(sec.getAddress());
}

error_or<uint64_t> section_size(const SectionRef &sec) {
    return success(sec.getSize());
}

//aimed for iteration over symbols, sections, relocations
template <typename T>
void next(content_iterator<T> &it, content_iterator<T> end) { ++it; }


typedef std::vector<std::pair<SymbolRef, uint64_t>> symbols_sizes;

//replace to computeSymbolSizes function, because sometimes it's
//results could be weird, e.g. symbol size eqauls to 18446744073709550526.
symbols_sizes get_symbols_sizes(const ObjectFile &obj) {
    symbols_sizes sizes;
    for (auto sym : symbols(obj)) {
        auto addr = symbol_address(sym);
        auto sect = symbol_section(obj, sym);
        if (!addr || !sect || *sect == end_sections(obj)) continue;
        auto sect_it = *sect;
        auto sect_addr = section_address(*sect_it);
        auto sect_size = section_size(*sect_it);
        if (!sect_addr || !sect_size) continue;
        uint64_t end = *sect_addr + *sect_size;
        if (end < *addr) continue;
        uint64_t size = end - *addr;
        for (auto next : symbols(obj)) {
            auto next_addr = symbol_address(next);
            auto next_sect = symbol_section(obj, next);
            if (!next_addr || !next_sect || *next_sect == end_sections(obj)) continue;
            if (*sect == *next_sect) {
                auto new_size = *next_addr > *addr ? *next_addr - *addr : size;
                size = new_size < size ? new_size : size;
            }
        }
        sizes.push_back(std::make_pair(sym, size));
    }
    return sizes;
}



#if LLVM_VERSION_MAJOR >= 10

error_or<pe32_header> get_pe32_header(const COFFObjectFile &obj) {
    const pe32_header *hdr = obj.getPE32Header();
    if (!hdr) { return failure("PE header not found"); }
    else return success(*hdr);
}

error_or<pe32plus_header> get_pe32plus_header(const COFFObjectFile &obj) {
    const pe32plus_header *hdr = obj.getPE32PlusHeader();
    if (!hdr) { return failure("PE+ header not found"); }
    else return success(*hdr);
}

#else

error_or<pe32_header> get_pe32_header(const COFFObjectFile &obj) {
    const pe32_header *hdr = 0;
    auto ec = obj.getPE32Header(hdr);
    if (ec) return failure(ec.message());
    else if (!hdr) { return failure("PE header not found"); }
    else return success(*hdr);
}

error_or<pe32plus_header> get_pe32plus_header(const COFFObjectFile &obj) {
    const pe32plus_header *hdr = 0;
    auto ec = obj.getPE32PlusHeader(hdr);
    if (ec) return failure(ec.message());
    else if (!hdr) { return failure("PE+ header not found"); }
    else return success(*hdr);
}
#endif

} // namespace prim
