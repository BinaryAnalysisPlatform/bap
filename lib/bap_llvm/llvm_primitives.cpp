#include <iostream>

#include <llvm/ADT/Triple.h>

#include "llvm_primitives.hpp"

namespace prim {
using namespace llvm;
using namespace llvm::object;

std::string arch_of_object(const llvm::object::ObjectFile &obj) {
    return Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
}

int64_t relative_address(uint64_t base, uint64_t abs) {
    return (abs - base);
}

// 4.0 only
#if LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 5

template <typename T>
std::string error_message(Expected<T> &e) {
    return toString(e.takeError());
}

error_or<SymbolRef::Type> symbol_type(const SymbolRef &s) {
    auto typ = s.getType();
    if (typ) return success(*typ);
    else return failure(error_message(typ));
}

// 3.8 only
#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

template <typename T>
std::string error_message(const ErrorOr<T> &er) {
    return er.getError().message();
}

error_or<SymbolRef::Type> symbol_type(const SymbolRef &s) {
    return success(s.getType());
}

#endif

// 4.0 or 3.8
#if LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 5    \
    || LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

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

error_or<uint64_t> symbol_value(const SymbolRef &s) { return success(s.getValue()); }

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

error_or<std::string> section_name(const SectionRef &sec) {
    StringRef name;
    auto er = sec.getName(name);
    if (!er) return success(name.str());
    else return failure(er.message());
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

// 3.4
#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

const char* get_raw_data(const ObjectFile &obj) {
    return obj.getData().begin();
}

symbol_iterator begin_symbols(const ObjectFile &obj) { return obj.begin_symbols(); }
symbol_iterator end_symbols(const ObjectFile &obj) { return obj.end_symbols(); }
section_iterator begin_sections(const ObjectFile &obj) { return obj.begin_sections(); }
section_iterator end_sections(const ObjectFile &obj) { return obj.end_sections(); }

error_or<std::string> symbol_name(const SymbolRef &s) {
    StringRef name;
    auto er_name = s.getName(name);
    if (!er_name) return success(name.str());
    else return failure(er_name.message());
}

error_or<uint64_t> symbol_size(const SymbolRef &s) {
    uint64_t size;
    auto er = s.getSize(size);
    if (!er) return success(size);
    else return failure(er.message());
}

error_or<uint64_t> symbol_value(const SymbolRef &s) {
    uint64_t val;
    auto er = s.getValue(val);
    if (!er) return success(val);
    else return failure(er.message());
}


error_or<SymbolRef::Type> symbol_type(const SymbolRef &s) {
    SymbolRef::Type typ;
    auto er = s.getType(typ);
    if (!er) return success(typ);
    else return failure(er.message());
}

error_or<uint64_t> symbol_address(const SymbolRef &s) {
    uint64_t addr;
    auto er = s.getAddress(addr);
    if (er) return failure(er.message());

    //need to check this due to nice llvm code like:
    // ...
    // Result = UnknownAddressOrSize;
    // return object_error::success;
    // ..
    // where UnknownAddressOrSize = 18446744073709551615
    if (addr == UnknownAddressOrSize)
        addr = 0;
    return success(addr);
}

error_or<section_iterator> symbol_section(const ObjectFile &obj, const SymbolRef &s) {
    section_iterator sec = obj.begin_sections();
    auto er = s.getSection(sec);
    if (!er) return success(sec);
    else return failure(er.message());
}


error_or<std::string> section_name(const SectionRef &sec) {
    StringRef name;
    auto er = sec.getName(name);
    if (!er) return success(name.str());
    else return failure(er.message());
}

error_or<uint64_t> section_address(const SectionRef &sec) {
    uint64_t addr;
    auto er = sec.getAddress(addr);
    if (!er) return success(addr);
    else return failure(er.message());
}

error_or<uint64_t> section_size(const SectionRef &sec) {
    uint64_t size;
    auto er = sec.getSize(size);
    if (!er) return success(size);
    else return failure(er.message());
}

uint64_t relocation_offset(const RelocationRef &rel) {
    uint64_t off;
    auto er_off = rel.getOffset(off); // this operation is always successful
    return off;
}

template <typename T>
std::vector<T> collect(content_iterator<T> begin, content_iterator<T> end) {
    std::vector<T> data;
    for (auto it = begin; it != end; next(it, end))
        data.push_back(*it);
    return data;
}

std::vector<SectionRef> sections(const ObjectFile &obj) {
    return collect(obj.begin_sections(), obj.end_sections());
}

std::vector<RelocationRef> relocations(const SectionRef &sec) {
    return collect(sec.begin_relocations(), sec.end_relocations());
}

std::vector<SymbolRef> symbols(const ObjectFile &obj) {
    return collect(obj.begin_symbols(), obj.end_symbols());
}

#else
#error LLVM version is not supported
#endif

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

} // namespace prim
