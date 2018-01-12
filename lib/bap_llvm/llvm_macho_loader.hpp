#ifndef LLVM_MACHO_LOADER_HPP
#define LLVM_MACHO_LOADER_HPP

#include <algorithm>
#include <iostream>
#include <iomanip>


#if LLVM_VERSION_MAJOR == 5
#include <llvm/BinaryFormat/MachO.h>
#else
#include <llvm/Support/MachO.h>
#endif

#include <llvm/Object/MachO.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"
#include "llvm_primitives.hpp"

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
#include <llvm/Object/SymbolSize.h>
#endif

namespace loader {
namespace macho_loader {

using namespace llvm;
using namespace llvm::object;

typedef llvm::object::MachOObjectFile macho;
typedef macho::LoadCommandInfo command_info;
typedef std::vector<command_info> commands;
typedef SymbolRef::Type sym_type;

static std::string macho_declarations =
    "(declare file-type (name str))"
    "(declare arch (name str))"
    "(declare entry (relative-addr int))"
    "(declare default-base-address (addr int))"
    "(declare segment-command (name str) (off int) (size int))"
    "(declare segment-command-flags (name str) (r bool) (w bool) (x bool))"
    "(declare virtual-segment-command (name str) (relative-addr int) (size int))"
    "(declare section-entry (name str) (relative-addr int) (size int) (off int))"
    "(declare symbol-entry (name str) (relative-addr int) (size int) (off int))"
    "(declare macho-symbol (name str) (value int))"
    "(declare code-entry (name str) (off int) (size int))"
    "(declare relocatable (flag bool))"
    "(declare ref-internal (sym-off int) (rel-off int))"
    "(declare ref-external (rel-off int) (name str))";


template <typename T>
void segment_command(const T &cmd, uint64_t base, ogre_doc &s) {
    bool r = static_cast<bool>(cmd.initprot & MachO::VM_PROT_READ);
    bool w = static_cast<bool>(cmd.initprot & MachO::VM_PROT_WRITE);
    bool x = static_cast<bool>(cmd.initprot & MachO::VM_PROT_EXECUTE);
    s.entry("segment-command") << cmd.segname << cmd.fileoff << cmd.filesize;
    s.entry("segment-command-flags") << cmd.segname << r << w << x;
    s.entry("virtual-segment-command") << cmd.segname << prim::relative_address(base, cmd.vmaddr) << cmd.vmsize;
}

void entry_point(command_info &info, ogre_doc &s) {
    const MachO::entry_point_command *entry_cmd =
        reinterpret_cast<const MachO::entry_point_command*>(info.Ptr);
    s.entry("entry") << entry_cmd->entryoff;
}

uint32_t filetype(const macho &obj) {
    if (obj.is64Bit()) return obj.getHeader64().filetype;
    else return obj.getHeader().filetype;
}

bool is_relocatable(const macho &obj) {
    return (filetype(obj) == MachO::MH_OBJECT ||
            filetype(obj) == MachO::MH_KEXT_BUNDLE ||
            filetype(obj) == MachO::MH_BUNDLE);
}

bool is_exec(const macho &obj) { return filetype(obj) == MachO::MH_EXECUTE;  }

// LC_MAIN command is absent in some file types, so we provide an entry here
void image_info(const macho &obj, ogre_doc &s) {
    if (!is_exec(obj))
        s.raw_entry("(entry 0)");
    s.entry("relocatable") << is_relocatable(obj);
}

commands macho_commands(const macho &obj);

// collect address from executable segment commands
template <typename T>
void add_segment_addr(const T &cmd, std::vector<uint64_t> &addrs) {
    if (static_cast<bool>(cmd.initprot & MachO::VM_PROT_EXECUTE))
        addrs.push_back(cmd.vmaddr);
}

template <typename Info>
void add_segment_addr(const macho &obj, const Info &info, std::vector<uint64_t> &addrs) {
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
        add_segment_addr(obj.getSegment64LoadCommand(info), addrs);
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
        add_segment_addr(obj.getSegmentLoadCommand(info), addrs);
}

uint64_t image_base(const macho &obj) {
    std::vector<uint64_t> addrs;
    for (auto info : macho_commands(obj))
        add_segment_addr(obj, info, addrs);
    auto it = std::min_element(addrs.begin(), addrs.end());
    if (it == addrs.end()) return 0;
    else return *it;
}

void dynamic_relocations(const macho &obj, command_info &info, ogre_doc &s);

void macho_command(const macho &obj, command_info &info, ogre_doc &s) {
    auto base = image_base(obj);
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
        segment_command(obj.getSegment64LoadCommand(info), base, s);
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
        segment_command(obj.getSegmentLoadCommand(info), base, s);
    if (info.C.cmd == MachO::LoadCommandType::LC_MAIN)
        entry_point(info, s);
    if (info.C.cmd == MachO::LoadCommandType::LC_DYSYMTAB)
        dynamic_relocations(obj, info, s);
}

uint32_t section_offset(const macho &obj, section_iterator sec) {
    if (sec == prim::end_sections(obj)) return 0;
    if (obj.is64Bit()) {
        MachO::section_64 ms = obj.getSection64(sec->getRawDataRefImpl());
        return ms.offset;
    } else {
        MachO::section ms = obj.getSection(sec->getRawDataRefImpl());
        return ms.offset;
    }
}

#define MACHO_SECTION_FIELD(field, result)                            \
    result section_##field(const macho &obj, SectionRef sec) {        \
        if (obj.is64Bit())                                            \
            return obj.getSection64(sec.getRawDataRefImpl()).field;   \
        else                                                          \
          return obj.getSection(sec.getRawDataRefImpl()).field;       \
    }

MACHO_SECTION_FIELD(flags, uint32_t)
MACHO_SECTION_FIELD(size, uint64_t)
MACHO_SECTION_FIELD(reserved1, uint32_t)
MACHO_SECTION_FIELD(reserved2, uint32_t)
MACHO_SECTION_FIELD(addr, uint64_t)
MACHO_SECTION_FIELD(offset, uint32_t)

uint32_t section_type(const macho &obj, SectionRef sec) {
    return section_flags(obj, sec) & MachO::SECTION_TYPE;
}

void section(const std::string &name, uint64_t rel_addr, uint64_t size, uint64_t off, ogre_doc &s) {
     s.entry("section-entry") << name << rel_addr << size << off;
}

// we distinguish symbols that are defined in some section and symbols that are not. For former it's ok
// to provide size and interpret symbol's value as an address. For later we provide only name and value
// as it is.
void section_symbol(const std::string &name, uint64_t rel_addr, uint64_t size, uint64_t off, sym_type typ, ogre_doc &s) {
    s.entry("symbol-entry") << name << rel_addr << size << off;
    if (typ == SymbolRef::ST_Function)
        s.entry("code-entry") << name << off << size;
}

void macho_symbol(const std::string &name, uint64_t value, ogre_doc &s) {
    s.entry("macho-symbol") << name << value;
}

bool is_external(const macho &obj, const SymbolRef &sym) {
    if (obj.is64Bit()) {
        MachO::nlist_64 ms = obj.getSymbol64TableEntry(sym.getRawDataRefImpl());
        return ms.n_type == MachO::N_EXT;
    } else {
        MachO::nlist ms = obj.getSymbolTableEntry(sym.getRawDataRefImpl());
        return ms.n_type == MachO::N_EXT;
    }
}

uint64_t symbol_value(const macho &obj, const SymbolRef &sym) {
    if (obj.is64Bit())
        return obj.getSymbol64TableEntry(sym.getRawDataRefImpl()).n_value;
    else
        return obj.getSymbolTableEntry(sym.getRawDataRefImpl()).n_value;
}

error_or<uint64_t> symbol_file_offset(const macho &obj, const SymbolRef &sym) {
    auto val = symbol_value(obj, sym);
    if (filetype(obj) == MachO::MH_OBJECT) {
        auto sect = prim::symbol_section(obj, sym);
        if (!sect) return sect;
        uint64_t off = val + section_offset(obj, *sect);
        return success(off);
    } else return success(val);
}

void symbol_reference(const macho &obj, const RelocationRef &rel, section_iterator sec, ogre_doc &s) {
    auto it = rel.getSymbol();
    if (it == prim::end_symbols(obj)) return;
    auto sec_offset = section_offset(obj, sec);
    auto off = prim::relocation_offset(rel) + sec_offset;
    if (is_external(obj, *it)) {
        if (auto name = prim::symbol_name(*it))
            s.entry("ref-external") << off << *name;
    } else {
        if (auto file_offset = symbol_file_offset(obj, *it))
            s.entry("ref-internal") << *file_offset << off;
    }
}

symbol_iterator get_symbol(const macho &obj, std::size_t index);

void symbol_reference(const macho &obj, std::size_t sym_num, uint64_t offset, ogre_doc &s) {
    auto it = get_symbol(obj, sym_num);
    if (it == prim::end_symbols(obj)) return;
    if (is_external(obj, *it)) {
        if (auto name = prim::symbol_name(*it))
            s.entry("ref-external") << offset << *name;
    } else {
        if (auto file_offset = symbol_file_offset(obj, *it))
            s.entry("ref-internal") << *file_offset << offset;
    }
}

// checks that symbol belongs to some sections,
// i.e. it's type is N_SECT.
bool is_in_section(const macho &obj, const SymbolRef &sym) {
    uint8_t typ;
    if (obj.is64Bit()) {
        auto e = obj.getSymbol64TableEntry(sym.getRawDataRefImpl());
        typ = e.n_type;
    } else {
        auto e = obj.getSymbolTableEntry(sym.getRawDataRefImpl());
        typ = e.n_type;
    }
    return ((typ & MachO::N_TYPE) == MachO::N_SECT);
}

error_or<uint64_t> symbol_address(const macho &obj, const SymbolRef &sym) {
    if (is_relocatable(obj))
        return success(uint64_t(0));
    auto addr = prim::symbol_address(sym);
    if (!addr) return addr;
    auto base = image_base(obj);
    return success(prim::relative_address(base, *addr));
}

void relocations(const macho &obj, ogre_doc &s) {
    for (auto sec : prim::sections(obj))
        for (auto rel : prim::relocations(sec))
            symbol_reference(obj, rel, sec.getRelocatedSection(), s);
}

void sections(const macho &obj, ogre_doc &s) {
    auto base = image_base(obj);
    for (auto sec : prim::sections(obj)) {
        auto addr = prim::section_address(sec);
        auto size = prim::section_size(sec);
        auto name = prim::section_name(sec);
        auto offs = section_offset(obj, section_iterator(sec));
        if (!addr || !name || !size) continue;
        section(*name, prim::relative_address(base, *addr), *size, offs, s);
        if (section_flags(obj, sec) & MachO::S_ATTR_PURE_INSTRUCTIONS)
            s.entry("code-entry") << *name << offs << *size;
    }
}

void symbols(const macho &obj, const prim::symbols_sizes &sizes, ogre_doc &s) {
    for (auto sized_sym : sizes) {
        auto sym = sized_sym.first;
        auto size = sized_sym.second;
        auto name = prim::symbol_name(sym);
        if (!name) continue;
        if (is_in_section(obj, sym)) {
            auto addr = symbol_address(obj, sym);
            auto offs = symbol_file_offset(obj, sym);
            auto type = prim::symbol_type(sym);
            if (!addr || !offs || !type) continue;
            section_symbol(*name, *addr, size, *offs, *type, s);
        }
        else
            macho_symbol(*name, symbol_value(obj, sym), s);
    }
}

void iterate_macho_commands(const macho &obj, ogre_doc &s) {
    auto cmds = macho_commands(obj);
    for (auto cmd : cmds)
        macho_command(obj, cmd, s);
}

void iterate_dyn_relocations(const macho &obj, std::size_t off, std::size_t num, ogre_doc &s) {
    const char *ptr = prim::get_raw_data(obj);
    std::size_t next = off;
    for (std::size_t i = 0; i < num; ++i) {
        const MachO::relocation_info *rel = reinterpret_cast<const MachO::relocation_info *>(ptr + next);
        if (rel->r_address & MachO::R_SCATTERED) {
            // not supported
            next = next + sizeof(MachO::scattered_relocation_info);
        } else {
            if (rel->r_extern)
                symbol_reference(obj, rel->r_symbolnum, rel->r_address, s);
            next = next + sizeof(MachO::relocation_info);
        }
    }
}

void indirect_symbols(const macho &obj, const MachO::dysymtab_command &dlc, ogre_doc &s) {

    for (auto sec : prim::sections(obj)) { // sections
        auto typ = section_type(obj, sec);
        if (typ == MachO::S_NON_LAZY_SYMBOL_POINTERS ||
            typ == MachO::S_LAZY_SYMBOL_POINTERS ||
            typ == MachO::S_LAZY_DYLIB_SYMBOL_POINTERS ||
            typ == MachO::S_THREAD_LOCAL_VARIABLE_POINTERS ||
            typ == MachO::S_SYMBOL_STUBS) { // section test

            uint32_t stride;
            if (typ == MachO::S_SYMBOL_STUBS)
                stride = section_reserved2(obj, sec);
            else
                stride = obj.is64Bit() ? 8 : 4;
            if (stride == 0) continue;

            uint64_t count = section_size(obj, sec) / stride;
            uint32_t n = section_reserved1(obj, sec);
            uint64_t addr = section_addr(obj, sec);
            uint32_t offs = section_offset(obj, sec);

            for (uint32_t j = 0; j < count && n + j < dlc.nindirectsyms; j++) {
                auto i_addr = addr + j * stride;
                auto i_offs = offs + j * stride;
                uint32_t indirect_symbol = obj.getIndirectSymbolTableEntry(dlc, n + j);
                MachO::symtab_command Symtab = obj.getSymtabLoadCommand();
                if (indirect_symbol < Symtab.nsyms) {
                    symbol_iterator Sym = get_symbol(obj, indirect_symbol);

                    if (auto name = prim::symbol_name(*Sym)) {
                        s.entry("symbol-entry") << *name << i_addr << stride << i_offs ;
                        s.entry("code-entry") << *name << i_offs << stride;
                    }
                }
            }
        } // section test
    } // sections
}

void dynamic_relocations(const macho &obj, command_info &info, ogre_doc &s) {
    if (info.C.cmd == MachO::LoadCommandType::LC_DYSYMTAB) {
        const MachO::dysymtab_command *cmd =
            reinterpret_cast<const MachO::dysymtab_command*>(info.Ptr);
        indirect_symbols(obj, *cmd, s);
        iterate_dyn_relocations(obj, cmd->locreloff, cmd->nlocrel, s);
        iterate_dyn_relocations(obj, cmd->extreloff, cmd->nextrel, s);
    }
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8 \
    || LLVM_VERSION_MAJOR == 4 || LLVM_VERSION_MAJOR == 5

commands macho_commands(const macho &obj) {
    commands cmds;
    for (auto c : obj.load_commands())
        cmds.push_back(c);
    return cmds;
}

void symbols(const macho &obj, ogre_doc &s) {
    auto sizes = prim::get_symbols_sizes(obj);
    symbols(obj, sizes, s);
}

symbol_iterator get_symbol(const macho &obj, std::size_t index) {
    return obj.getSymbolByIndex(index);
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

std::size_t command_count(const macho &obj) {
    if (obj.is64Bit())  return obj.getHeader64().ncmds;
    else obj.getHeader().ncmds;
}

commands macho_commands(const macho &obj) {
    commands cmds;
    command_info info = obj.getFirstLoadCommandInfo();
    for (std::size_t i = 0; i < command_count(obj); ++i) {
        cmds.push_back(info);
        info = obj.getNextLoadCommandInfo(info);
    }
    return cmds;
}

void symbols(const macho &obj, ogre_doc &s) {
    prim::symbols_sizes syms;
    auto base = image_base(obj);
    for (auto sym : prim::symbols(obj)) {
        auto size = prim::symbol_size(sym);
        if (!size) continue;
        syms.push_back(std::make_pair(sym, *size));
    }
    symbols(obj, syms, s);
}

symbol_iterator get_symbol(const macho &obj, std::size_t index) {
    auto cmds = macho_commands(obj);
    bool x = std::any_of(cmds.begin(), cmds.end(),
                         [](const command_info &info)
                         { return (info.C.cmd == MachO::LoadCommandType::LC_SYMTAB); });
    if (!x) return prim::end_symbols(obj);
    DataRefImpl dri;
    MachO::symtab_command symtab = obj.getSymtabLoadCommand();
    if (index >= symtab.nsyms)
        return prim::end_symbols(obj);
    std::size_t entry_size =
        obj.is64Bit() ? sizeof(MachO::nlist_64) : sizeof(MachO::nlist);
    dri.p = reinterpret_cast<uintptr_t>(prim::get_raw_data(obj) + symtab.symoff);
    dri.p += index * entry_size;
    return symbol_iterator(SymbolRef(dri, &obj));
}

#else
#error LLVM version is not supported
#endif

} // namespace macho_loader

error_or<std::string> load(const llvm::object::MachOObjectFile &obj) {
    using namespace macho_loader;
    ogre_doc s;
    s.raw_entry(macho_declarations);
    s.raw_entry("(file-type macho)");
    s.entry("arch") << prim::arch_of_object(obj);
    s.entry("default-base-address") << image_base(obj);
    image_info(obj, s);
    iterate_macho_commands(obj, s);
    sections(obj, s);
    symbols(obj, s);
    relocations(obj, s);
    return s.str();
}

} // namespace loader

#endif //  LLVM_MACHO_LOADER_HPP
