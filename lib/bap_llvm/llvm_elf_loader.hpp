#ifndef LLVM_ELF_LOADER_HPP
#define LLVM_ELF_LOADER_HPP

// Clarification-relocation.
//
// Elf loader provide information about common entries like segments, sectuions and symbols
// Also it provides information about relocations, and there are some details here.
//
// Relocation info is targeting mainly for relocatable files like shared libraries or kernel
// modules. Such files don't have entry point or segments, symbol addresses etc. and contain
// calls to unresolved locations like in example below.
//
// ...
// 0000000000000014 <my_fun>:
//    14:	55                   	push   %rbp
//    15:	48 89 e5             	mov    %rsp,%rbp
//    18:	48 83 ec 18          	sub    $0x18,%rsp
//    1c:	89 7d ec             	mov    %edi,-0x14(%rbp)
//    1f:	c7 45 f8 2a 00 00 00 	movl   $0x2a,-0x8(%rbp)
//    26:	8b 55 ec             	mov    -0x14(%rbp),%edx
//    29:	8b 45 f8             	mov    -0x8(%rbp),%eax
//    2c:	89 d6                	mov    %edx,%esi
//    2e:	89 c7                	mov    %eax,%edi
//--> 30:	e8 00 00 00 00       	callq  35 <my_fun+0x21>
//    35:	89 45 fc             	mov    %eax,-0x4(%rbp)
//    38:	8b 45 fc             	mov    -0x4(%rbp),%eax
//    3b:	c9                   	leaveq
//    3c:	c3                   	retq
// ...
//
// 0x31 is offset where some changes in address expected - 00 00 00 00 defenetly is not
// an address. It could be a reference to a symbol defined in same file or to a symbol defined somewhere
// else (external symbol).
// So, our task is to resolve this case, i.e. to find a mapping from this offset to something sensible.
//
// First of all we should use absoulute offset, i.e. file offsets to make every mapping unique.
// So full offset in example above will be computed as section offset + 0x31. And it is a place
// where relocation should be applied.
//
// We define two attributes for relocations, and every relocation is represented only by one of them:
// 1) ref-internal that is a mapping from one file offset to another ;
// 2) ref-external that is a mapping from file offset to some name.
//

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

static const std::string elf_declarations =
    "(declare file-type (name str))"
    "(declare arch (name str))"
    "(declare default-base-address (addr int))"
    "(declare entry (relative-addr int))"
    "(declare relocatable (flag bool))"
    "(declare program-header (name str) (off int) (size int))"
    "(declare virtual-program-header (name str) (relative-addr int) (size int))"
    "(declare program-header-flags (name str) (ld bool) (r bool) (w bool) (x bool))"
    "(declare section-entry (name str) (relative-addr int) (size int) (off int))"
    "(declare section-flags (name str) (w bool) (x bool))"
    "(declare symbol-entry (name str) (relative-addr int) (size int) (off int))"
    "(declare plt-entry (name str) (relative-addr int) (size int) (off int))"
    "(declare code-entry (name str) (off int) (size int))"
    "(declare ref-internal (sym-off int) (rel-off int))"
    "(declare ref-external (rel-off int) (name str))";

template <typename T>
bool is_rel(const ELFObjectFile<T> &obj) {
    auto hdr = obj.getELFFile()->getHeader();
    return (hdr->e_type == ELF::ET_REL);
}

//taking a smallest virtual address of loadable segments as a base address
template <typename Phdr>
uint64_t base_address(Phdr begin, Phdr end) {
    if (begin == end) return 0;
    std::vector<uint64_t> addrs;
    for (auto it = begin; it != end; ++it)
        if (it->p_type == ELF::PT_LOAD)
            addrs.push_back(it->p_vaddr);
    auto it = std::min_element(addrs.begin(), addrs.end());
    if (it == addrs.end()) return 0;
    return *it;
}

template <typename T>
uint64_t base_address(const ELFObjectFile<T> &obj);

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

template <typename I>
void program_headers(I begin, I end, ogre_doc &s) {
    std::size_t i = 0;
    uint64_t base = base_address(begin, end);
    for (auto it = begin; it != end; ++it, ++i) {
        bool ld = (it->p_type == ELF::PT_LOAD);
        bool r = static_cast<bool>(it->p_flags & ELF::PF_R);
        bool w = static_cast<bool>(it->p_flags & ELF::PF_W);
        bool x = static_cast<bool>(it->p_flags & ELF::PF_X);
        auto off = it->p_offset;
        auto filesz = it->p_filesz;
        auto name = name_of_index(i);
        auto addr = prim::relative_address(base, it->p_vaddr);
        s.entry("program-header") << name << off << filesz;
        s.entry("virtual-program-header") << name << addr << it->p_memsz;
        s.entry("program-header-flags") << name << ld << r << w << x;
    }
}

template <typename T>
void section_header(const T &hdr, const std::string &name, uint64_t base, ogre_doc &s) {
    auto addr = prim::relative_address(base, hdr.sh_addr);
    s.entry("section-entry") << name << addr << hdr.sh_size << hdr.sh_offset;
    bool w = static_cast<bool>(hdr.sh_flags & ELF::SHF_WRITE);
    bool x = static_cast<bool>(hdr.sh_flags & ELF::SHF_EXECINSTR);
    s.entry("section-flags") << name << w << x;
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
        return success(prim::relative_address(base, *addr));
    }
}

// [symbol_reference obj relocation section doc] - provide information for [relocation]
// that refered to [section]
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
    || LLVM_VERSION_MAJOR == 4 && LLVM_VERSION_MINOR == 0

template <typename T>
uint64_t base_address(const ELFObjectFile<T> &obj) {
    auto hdrs = prim::elf_program_headers(*obj.getELFFile());
    return base_address(hdrs.begin(), hdrs.end());
}

template <typename T>
void program_headers(const ELFObjectFile<T> &obj, ogre_doc &s) {
    auto hdrs = prim::elf_program_headers(*obj.getELFFile());
    program_headers(hdrs.begin(), hdrs.end(), s);
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
        auto rel_sec = sec.getRelocatedSection();
        if (!checked(obj, sec)) continue;
        for (auto rel : sec.relocations())
            symbol_reference(obj, rel, rel_sec, s);
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
uint64_t base_address(const ELFObjectFile<T> &obj) {
    auto elf = obj.getELFFile();
    return base_address(elf->begin_program_headers(), elf->end_program_headers());
}

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
        for (auto rel : prim::relocations(sec))
            symbol_reference(obj, rel, sec.getRelocatedSection(), s);
}

#else
#error LLVM version is not supported
#endif

} // namespace elf_loader

template <typename T>
error_or<std::string> load(const llvm::object::ELFObjectFile<T> &obj) {
    using namespace elf_loader;
    ogre_doc s;
    s.raw_entry(elf_declarations);
    s.raw_entry("(file-type elf)");
    s.entry("default-base-address") << base_address(obj);
    s.entry("arch") << prim::arch_of_object(obj);
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
