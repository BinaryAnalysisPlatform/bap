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
bool has_addresses(const ELFObjectFile<T> &obj) {
    auto hdr = obj.getELFFile()->getHeader();
    return (hdr->e_type == ELF::ET_EXEC ||
            hdr->e_type == ELF::ET_DYN ||
            hdr->e_type == ELF::ET_CORE);
}


template <typename T>
bool is_executable(const ELFObjectFile<T> &obj) {
    auto hdr = obj.getELFFile()->getHeader();
    return (hdr->e_type == ELF::ET_EXEC ||
            hdr->e_type == ELF::ET_DYN);
}

// computes the base address of an ELF file.
//
// The base address is derived as a difference between the
// virtual address of any loadable code segment and its offset.
//
// If there are no loadable segments then the base is 0.
template <typename T>
uint64_t base_address(const ELFObjectFile<T> &obj) {
    uint64_t base = 0L;
    auto elf = *obj.getELFFile();
    auto segs = prim::elf_program_headers(elf);
    auto code = segs.end();

    for (auto it = segs.begin(); it != segs.end(); ++it)
        if (it->p_type == ELF::PT_LOAD && (it->p_flags & ELF::PF_X))
            code = it;

    if (code != segs.end())
        base = code->p_vaddr - code->p_offset;
    return base;
}

template <typename T>
uint64_t minimal_progbits_offset(const ELFObjectFile<T> &obj) {
    auto smallest = std::numeric_limits<uint64_t>::max();
    for (auto sec :  prim::elf_sections(*obj.getELFFile())) {
        if (sec.sh_type == ELF::SHT_PROGBITS && sec.sh_offset < smallest) {
            smallest = sec.sh_offset;
        }
    }
    return smallest == std::numeric_limits<uint64_t>::max() ? 0L : smallest;
}

template <typename T>
void emit_entry_point(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto hdr = obj.getELFFile()->getHeader();
    s.entry("llvm:entry-point") << hdr->e_entry;
}

std::string name_of_index(std::size_t i) {
    std::ostringstream s;
    s << std::setfill('0') << std::setw(2) << i;
    return s.str();
}

template <typename T>
void emit_program_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto hdrs = prim::elf_program_headers(*obj.getELFFile());
    for (auto it = hdrs.begin(); it != hdrs.end(); ++it) {
        bool ld = (it->p_type == ELF::PT_LOAD);
        bool r = static_cast<bool>(it->p_flags & ELF::PF_R);
        bool w = static_cast<bool>(it->p_flags & ELF::PF_W);
        bool x = static_cast<bool>(it->p_flags & ELF::PF_X);
        auto off = it->p_offset;
        auto filesz = it->p_filesz;
        auto name = name_of_index(it - hdrs.begin());
        s.entry("llvm:elf-program-header") << name << off << filesz;
        s.entry("llvm:elf-virtual-program-header") << name << it->p_vaddr << it->p_memsz;
        s.entry("llvm:elf-program-header-flags") << name << ld << r << w << x;
    }
}

template <typename T>
uint64_t section_offset(const ELFObjectFile<T> &obj, section_iterator it) {
    if (it == obj.section_end()) return 0L;
    else {
        auto sec_elf = obj.getSection(it->getRawDataRefImpl());
        return sec_elf->sh_offset;
    }
}

template <typename T>
uint64_t section_address(const ELFObjectFile<T> &obj,
                         const typename ELFFile<T>::Elf_Shdr *sec) {
    return has_addresses(obj)
        ? sec->sh_addr
        : sec->sh_offset - minimal_progbits_offset(obj);
}

template <typename T>
uint64_t section_address(const ELFObjectFile<T> &obj, const SectionRef& sec) {
    return section_address(obj, obj.getSection(sec.getRawDataRefImpl()));
}

template <typename T>
uint64_t section_address(const ELFObjectFile<T> &obj, section_iterator it) {
    if (it == obj.section_end()) return 0;
    else return section_address(obj, *it);
}


template <typename T>
error_or<uint64_t> symbol_file_offset(const ELFObjectFile<T> &obj, const SymbolRef &sym) {
    auto addr = prim::symbol_address(sym);
    auto sect = prim::symbol_section(obj, sym);
    if (auto er = addr || sect) return er;
    uint64_t off = section_offset(obj, *sect) + (*addr - section_address(obj, *sect));
    return success(off);
}
template <typename T>
void emit_section(const ELFObjectFile<T> &obj,
                  const SectionRef& sec,
                  ogre_doc &s) {
    uint64_t addr = section_address(obj, sec);
    auto hdr = obj.getSection(sec.getRawDataRefImpl());
    if (auto name = prim::section_name(sec)) {
        if (!name->empty()) {
            s.entry("llvm:section-entry") << *name << addr << hdr->sh_size << hdr->sh_offset;
            bool w = static_cast<bool>(hdr->sh_flags & ELF::SHF_WRITE);
            bool x = static_cast<bool>(hdr->sh_flags & ELF::SHF_EXECINSTR);
            s.entry("llvm:section-flags") << *name << true << w << x;
            if (x)
                s.entry("llvm:code-entry") << *name << hdr->sh_offset << hdr->sh_size;
        }
    }
}

template <typename T>
void emit_section_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    for (auto sec : prim::sections(obj))
        emit_section(obj, sec, s);
}

bool is_external(uint64_t addr, uint64_t offset, uint64_t size) {
    return !addr && !offset && !size;
}

template <typename T>
void emit_symbol_entry(const ELFObjectFile<T> &obj, const SymbolRef &sym, ogre_doc &s) {
    auto sym_elf = obj.getSymbol(sym.getRawDataRefImpl());
    auto name = prim::symbol_name(sym);
    auto addr = prim::symbol_address(sym);
    auto off = symbol_file_offset(obj, sym);
    if (name && addr && off && !name->empty() &&
        !is_external(*addr, *off, sym_elf->st_size)) {
        s.entry("llvm:symbol-entry") << *name
                                     << *addr
                                     << sym_elf->st_size
                                     << *off
                                     << sym_elf->st_value;

        if (sym_elf->getType() == ELF::STT_FUNC)
            s.entry("llvm:code-entry") << *name << *off << sym_elf->st_size ;
    }
}

template <typename T>
void emit_symbol_entries(const ELFObjectFile<T> &obj, symbol_iterator begin, symbol_iterator end, ogre_doc &s) {
    for (auto it = begin; it != end; ++it)
        emit_symbol_entry(obj, *it, s);
}

template <typename T>
void emit_symbol_entries(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto elf = obj.getELFFile();
    emit_symbol_entries(obj, obj.symbol_begin(), obj.symbol_end(), s);
    auto secs = prim::elf_sections(*elf);
    emit_symbol_entries(obj, obj.dynamic_symbol_begin(), obj.dynamic_symbol_end(), s);
}

template <typename T>
void emit_relocations(const ELFObjectFile<T> &obj, ogre_doc &s) {
    for (auto sec : obj.sections()) {
        if (auto rel_sec = prim::relocated_section(sec)) {
            for (auto rel : sec.relocations()) {
                auto sym = rel.getSymbol();
                if (sym != prim::end_symbols(obj)) {
                    uint64_t base = section_address(obj, *rel_sec);
                    auto raddr = prim::relocation_offset(rel) + base;
                    if (auto addr = prim::symbol_address(*sym))
                        if (*addr) s.entry("llvm:relocation") << raddr << *addr;
                    if (auto name = prim::symbol_name(*sym))
                        s.entry("llvm:name-reference") << raddr << *name;
                }
            }
        }
    }
}


} // namespace elf_loader

template <typename T>
error_or<std::string> load(ogre_doc &s, const llvm::object::ELFObjectFile<T> &obj) {
    using namespace elf_loader;
    s.raw_entry("(format elf)");
    s.entry("llvm:base-address") << base_address(obj);
    s.entry("is-executable") << is_executable(obj);
    emit_entry_point(obj, s);
    emit_program_headers(obj, s);
    emit_section_headers(obj, s);
    emit_symbol_entries(obj, s);
    emit_relocations(obj, s);
    return s.str();
}

} // namespace loader

#endif // LLVM_ELF_LOADER_HPP
