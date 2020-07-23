#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

#include <algorithm>
#include <iostream>
#include <iomanip>

#include <llvm/Object/ELFObjectFile.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"
#include "llvm_primitives.hpp"

namespace loader {
namespace elf_loader {

using namespace llvm;
using namespace llvm::object;

template <typename T>
bool is_rel(const ELFObjectFile<T> &obj) {
    auto hdr = obj.getELFFile()->getHeader();
    return (hdr->e_type == ELF::ET_REL);
}

// computes the base address of an ELF file.
//
// The base address is either derived as a difference between the
// virtual address of any loadable code segment or, if there are no
// segments or no loadable segments, it is the difference between
// the suggested address of the PROGBITS section with a minimal offset
// and that offset. For object and relocatable files, it is usually
// 0 - 0x34.
//
// Finally, if there are no loadable segments or PROGBIT sections,
// i.e., we don't really have a binary program but something else
// packed as an ELF file, we just return 0.
template <typename T>
uint64_t base_address(const ELFObjectFile<T> &obj) {
    uint64_t base = 0L;
    auto elf = *obj.getELFFile();
    auto segs = prim::elf_program_headers(elf);
    auto code = segs.end();

    for (auto it = segs.begin(); it != segs.end(); ++it)
        if (it->p_type == ELF::PT_LOAD && (it->p_flags & ELF::PF_X))
            code = it;

    if (code != segs.end()) {
        base = code->p_vaddr - code->p_offset;
    } else {
        auto secs = prim::elf_sections(elf);
        auto first = secs.end();
        auto smallest = std::numeric_limits<uint64_t>::max();
        for (auto it = secs.begin(); it != secs.end(); ++it) {
            if (it->sh_type == ELF::SHT_PROGBITS && it->sh_offset < smallest) {
                first = it;
                smallest = it->sh_offset;
            }
        }

        if (first != secs.end())
            base = first->sh_addr - first->sh_offset;
    }
    return base;
}


template <typename T>
void file_header(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto hdr = obj.getELFFile()->getHeader();
    auto base = base_address(obj);
    s.entry("relocatable") << is_rel(obj);
    s.entry("entry") << hdr->e_entry - base;
}

std::string name_of_index(std::size_t i) {
    std::ostringstream s;
    s << std::setfill('0') << std::setw(2) << i;
    return s.str();
}

template <typename T>
void section_header(const T &hdr, const std::string &name, uint64_t base, ogre_doc &s) {
    auto addr = hdr.sh_addr - base;
    s.entry("section-entry") << name << addr << hdr.sh_size << hdr.sh_offset;
    bool w = static_cast<bool>(hdr.sh_flags & ELF::SHF_WRITE);
    bool x = static_cast<bool>(hdr.sh_flags & ELF::SHF_EXECINSTR);
    s.entry("section-flags") << name << true << w << x;
    if (x)
        s.entry("code-entry") << name << hdr.sh_offset << hdr.sh_size;
}

template <typename T>
bool is_external_symbol(const T &sym) {
    return ((sym.getBinding() == ELF::STB_GLOBAL ||
             sym.getBinding() == ELF::STB_WEAK) && sym.st_size == 0);
}

template <typename T>
bool is_abs_symbol(const T &sym) { return (sym.st_shndx == ELF::SHN_ABS); }

template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it);

template <typename T>
error_or<uint64_t> symbol_file_offset(const ELFObjectFile<T> &obj, const SymbolRef &sym) {
    auto addr = prim::symbol_address(sym);
    auto sect = prim::symbol_section(obj, sym);
    if (auto er = addr || sect) return er;
    uint64_t off = *addr + section_offset(obj, *sect);
    return success(off);
}

template <typename T>
error_or<uint64_t> symbol_address(const ELFObjectFile<T> &obj, const SymbolRef &sym) {
    auto sym_elf = obj.getSymbol(sym.getRawDataRefImpl());
    if (is_rel(obj) && !is_abs_symbol(*sym_elf)) { // abs symbols does not affected by relocations
        return success(uint64_t(0));
    } else {
        auto addr = prim::symbol_address(sym);
        if (!addr) return addr;
        auto base = base_address(obj);
        return success(*addr - base);
    }
}

// [symbol_reference obj relocation section doc] - provide information for [relocation]
// that referred to [section]
template <typename T>
void symbol_reference(const ELFObjectFile<T> &obj, const RelocationRef &rel, section_iterator sec, ogre_doc &s) {
    auto it = rel.getSymbol();
    if (it == prim::end_symbols(obj)) return;
    auto sym_elf = obj.getSymbol(it->getRawDataRefImpl());
    auto sec_offset = section_offset(obj, sec);
    auto off = prim::relocation_offset(rel) + sec_offset; // relocation file offset
    if (is_external_symbol(*sym_elf)) {
        if (auto name = prim::symbol_name(*it))
            s.entry("ref-external") << off << *name;
    } else {
        if (auto file_offset = symbol_file_offset(obj, *it))
            s.entry("ref-internal") << *file_offset << off;
    }
}

template <typename T>
void symbol_entry(const ELFObjectFile<T> &obj, const SymbolRef &sym, ogre_doc &s) {
    auto sym_elf = obj.getSymbol(sym.getRawDataRefImpl());
    if (is_abs_symbol(*sym_elf)) {
        return;
    }
    auto name = prim::symbol_name(sym);
    auto addr = symbol_address(obj, sym);
    auto off = symbol_file_offset(obj, sym);
    if (name && addr && off) {
        s.entry("symbol-entry") << *name << *addr << sym_elf->st_size << *off;
        if (sym_elf->getType() == ELF::STT_FUNC)
            s.entry("code-entry") << *name << *off << sym_elf->st_size ;
    }
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8          \
    || LLVM_VERSION_MAJOR >= 4

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    uint64_t base = base_address(obj);
    auto hdrs = prim::elf_program_headers(*obj.getELFFile());
    for (auto it = hdrs.begin(); it != hdrs.end(); ++it) {
        bool ld = (it->p_type == ELF::PT_LOAD);
        bool r = static_cast<bool>(it->p_flags & ELF::PF_R);
        bool w = static_cast<bool>(it->p_flags & ELF::PF_W);
        bool x = static_cast<bool>(it->p_flags & ELF::PF_X);
        auto off = it->p_offset;
        auto filesz = it->p_filesz;
        auto name = name_of_index(it - hdrs.begin());
        auto addr = it->p_vaddr - base;
        s.entry("program-header") << name << off << filesz;
        s.entry("virtual-program-header") << name << addr << it->p_memsz;
        s.entry("program-header-flags") << name << ld << r << w << x;
    }
}

template <typename T>
void section_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    auto base = base_address(obj);
    for (auto sec : prim::elf_sections(*elf)) {
        auto name = prim::elf_section_name(*elf, &sec);
        if (name)
            section_header(sec, *name, base, s);
    }
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, ogre_doc &s) {
    for (auto it = begin; it != end; ++it)
        symbol_entry(obj, *it, s);
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, ogre_doc &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj, obj.symbol_begin(), obj.symbol_end(), s);
    auto secs = prim::elf_sections(*elf);
    bool is_dyn = std::any_of(secs.begin(), secs.end(),
                              [](const sec_hdr &hdr) { return (hdr.sh_type == ELF::SHT_DYNSYM); });
    if (is_dyn) // preventing from llvm 3.8 fail in case of .dynsym absence
        symbol_entries(obj, obj.dynamic_symbol_begin(), obj.dynamic_symbol_end(), s);
}

// This check prevents llvm-3.8 fail in rare cases when relocated
// section link to some other than symtab - consider code in
// ElfObjectFile.h for section_rel_begin function
template <typename T>
bool checked(const ELFObjectFile<T> &obj, SectionRef sec_ref) {
    typedef typename ELFObjectFile<T>::Elf_Shdr Elf_Shdr;

    auto &elf = *obj.getELFFile();
    const Elf_Shdr *RelSec = obj.getSection(sec_ref.getRawDataRefImpl());
    auto symsec = elf.getSection(RelSec->sh_link);
    if (!symsec) return false;
    uint32_t sec_typ = (*symsec)->sh_type;
    return
        (sec_typ == ELF::SHT_SYMTAB || sec_typ == ELF::SHT_DYNSYM);
}

template <typename T>
void relocations(const ELFObjectFile<T> &obj, ogre_doc &s) {
    for (auto sec : obj.sections()) {
        auto rel_sec = prim::relocated_section(sec);
        if (!checked(obj, sec) || !rel_sec) continue;
        for (auto rel : sec.relocations())
            symbol_reference(obj, rel, *rel_sec, s);
    }
}

template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it) {
    if (it == obj.section_end()) return 0; // check for special elf sections
    auto sec_elf = obj.getSection(it->getRawDataRefImpl());
    return sec_elf->sh_offset;
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    program_headers(elf->begin_program_headers(), elf->end_program_headers(), s);
}

template <typename T>
void section_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    auto base = base_address(obj);
    for (auto it = elf->begin_sections(); it != elf->end_sections(); ++it) {
        auto name = elf->getSectionName(&*it);
        if (name)
            section_header(*it, (*name).str(), base, s);
        else
            s.fail(error_code(name).message());
    }
}

template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it) {
    typedef typename ELFObjectFile<T>::Elf_Shdr_Iter elf_shdr_iterator;

    if (it == obj.end_sections()) return 0; // check for special elf sections

    auto elf = obj.getELFFile();
    auto raw = it->getRawDataRefImpl();
    auto elf_sec_it = elf_shdr_iterator(elf->getHeader()->e_shentsize,
                                        reinterpret_cast<const char *>(raw.p));
    return elf_sec_it->sh_offset;
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, ogre_doc &s) {
    for (auto it = begin; it != end; prim::next(it, end))
        symbol_entry(obj, *it, s);
}

template <typename T>
void symbol_entries(const ELFObjectFile<T> &obj, ogre_doc &s) {
    typedef typename ELFFile<T>::Elf_Shdr sec_hdr;
    auto elf = obj.getELFFile();
    symbol_entries(obj, obj.begin_symbols(), obj.end_symbols(), s);
    symbol_entries(obj, obj.begin_dynamic_symbols(), obj.end_dynamic_symbols(), s);
}

template <typename T>
void relocations(const ELFObjectFile<T> &obj, ogre_doc &s) {
    for (auto sec : prim::sections(obj))
        for (auto rel : prim::relocations(sec)) {
            if (auto rel_sec = prim::relocated_section(sec))
                symbol_reference(obj, rel, *rel_sec, s);
        }
}

#else
#error LLVM version is not supported
#endif

} // namespace elf_loader

template <typename T>
error_or<std::string> load(ogre_doc &s, const llvm::object::ELFObjectFile<T> &obj) {
    using namespace elf_loader;
    s.raw_entry("(file-type elf)");
    s.entry("default-base-address") << base_address(obj);
    file_header(obj, s);
    program_headers(obj, s);
    section_headers(obj, s);
    symbol_entries(obj, s);
    if (is_rel(obj)) // llvm 3.8 has asserts for every relocations related function
        relocations(obj, s);
    return s.str();
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
