
// Here we place primitives that could come in handy,
// mainly to unify interfaces of llvm versions.

#ifndef LLVM_PRIMITIVES_HPP
#define LLVM_PRIMITIVES_HPP

#include <llvm/Object/ObjectFile.h>
#include <llvm/Object/ELF.h>

#include "llvm_error_or.hpp"

namespace prim {

using namespace llvm;
using namespace llvm::object;

// object stuff
std::string arch_of_object(const llvm::object::ObjectFile &obj);
const char* get_raw_data(const ObjectFile &obj);
std::vector<RelocationRef> relocations(const SectionRef &sec);
std::vector<SectionRef> sections(const ObjectFile &obj);
std::vector<SymbolRef> symbols(const ObjectFile &obj);

// sections stuff
section_iterator begin_sections(const ObjectFile &obj);
section_iterator end_sections(const ObjectFile &obj);
error_or<std::string> section_name(const SectionRef &sec);
error_or<uint64_t> section_address(const SectionRef &sec);
error_or<uint64_t> section_size(const SectionRef &sec);

// symbols stuff
symbol_iterator begin_symbols(const ObjectFile &obj);
symbol_iterator end_symbols(const ObjectFile &obj);
error_or<uint64_t> symbol_address(const SymbolRef &sym);
error_or<std::string> symbol_name(const SymbolRef &s);
error_or<uint64_t> symbol_value(const SymbolRef &s);
error_or<SymbolRef::Type> symbol_type(const SymbolRef &s);
error_or<section_iterator> symbol_section(const ObjectFile &obj, const SymbolRef &s);
error_or<uint64_t> symbol_size(const SymbolRef &s);

// relocations stuff
uint64_t relocation_offset(const RelocationRef &rel);

// misc stuff

// returns abs - base if abs >= base or just abs otherwise
uint64_t relative_address(uint64_t base, uint64_t abs);

// next - aimed for iteration over symbols, sections, relocations
template <typename T>
void next(content_iterator<T> &it, content_iterator<T> end);

typedef std::vector<std::pair<SymbolRef, uint64_t>> symbols_sizes;

//replace to computeSymbolSizes function, because sometimes it's
//results could be weird, e.g. symbol size eqauls to 18446744073709550526.
symbols_sizes get_symbols_sizes(const ObjectFile &obj);




// Pity, it's C++ and it isn't easy to hide templates inside .cpp

#if LLVM_VERSION_MAJOR == 4 && LLVM_VERSION_MINOR == 0

template <typename T>
std::vector<typename ELFFile<T>::Elf_Phdr> elf_program_headers(const ELFFile<T> &elf) {
    typedef typename ELFFile<T>::Elf_Phdr hdr;

    auto er_hdrs = elf.program_headers();
    if (!er_hdrs) return std::vector<hdr>();
    auto hdrs = er_hdrs.get();
    return std::vector<hdr>(hdrs.begin(), hdrs.end());
}

template <typename T>
std::vector<typename ELFFile<T>::Elf_Shdr> elf_sections(const ELFFile<T> &elf) {
    typedef typename ELFFile<T>::Elf_Shdr hdr;

    auto er_hdrs = elf.sections();
    if (!er_hdrs) return std::vector<hdr>();
    auto hdrs = er_hdrs.get();
    return std::vector<hdr>(hdrs.begin(), hdrs.end());
}

template <typename T>
error_or<std::string> elf_section_name(const ELFFile<T> &elf, const typename ELFFile<T>::Elf_Shdr *sec) {
    auto er_name = elf.getSectionName(sec);
    if (!er_name) return failure(toString(er_name.takeError()));
    return success(er_name.get().str());
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8

// TODO : repeat functions above


#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

// TODO : repeat functions above

#else
#error LLVM version is not supported
#endif



} // namespace prim

#endif // LLVM_PRIMITIVES_HPP
