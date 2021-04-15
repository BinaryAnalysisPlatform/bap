#ifndef LLVM_MACHO_LOADER_HPP
#define LLVM_MACHO_LOADER_HPP

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <limits>

#include <llvm/BinaryFormat/MachO.h>
#include <llvm/Object/MachO.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"
#include "llvm_primitives.hpp"

namespace loader {
namespace macho_loader {

using namespace llvm;
using namespace llvm::object;

typedef llvm::object::MachOObjectFile macho;
typedef macho::LoadCommandInfo command_info;
typedef std::vector<command_info> commands;
typedef SymbolRef::Type sym_type;

template <typename T>
void emit_segment_command(const T &cmd, ogre_doc &s) {
    bool r = static_cast<bool>(cmd.initprot & MachO::VM_PROT_READ);
    bool w = static_cast<bool>(cmd.initprot & MachO::VM_PROT_WRITE);
    bool x = static_cast<bool>(cmd.initprot & MachO::VM_PROT_EXECUTE);
    s.entry("llvm:segment-command") << cmd.segname << cmd.fileoff << cmd.filesize;
    s.entry("llvm:segment-command-flags") << cmd.segname << r << w << x;
    s.entry("llvm:virtual-segment-command") << cmd.segname << cmd.vmaddr << cmd.vmsize;
}

uint32_t filetype(const macho &obj) {
    if (obj.is64Bit()) return obj.getHeader64().filetype;
    else return obj.getHeader().filetype;
}

bool is_executable(const macho &obj) {
    return (filetype(obj) == MachO::MH_EXECUTE ||
            filetype(obj) == MachO::MH_PRELOAD);
}


commands macho_commands(const macho &obj);


template <typename T>
uint64_t is_first_nonempty(const T& cmd) {
    return cmd.fileoff == 0 && cmd.filesize != 0;
}

template <typename T>
uint64_t base_for_relocatable(const T& cmd) {
    return cmd.vmaddr - cmd.fileoff;
}

struct segment_info {
    uint64_t vmaddr;
    uint64_t offset;
    segment_info(uint64_t addr, uint64_t off) : vmaddr(addr), offset(off) {}
};

error_or<segment_info> first_segment(const macho &obj) {
    for (auto info : macho_commands(obj)) {
        if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64) {
            auto cmd = obj.getSegment64LoadCommand(info);
            if (is_first_nonempty(cmd))
                return success(segment_info(cmd.vmaddr, cmd.fileoff));
       } else if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT) {
            auto cmd = obj.getSegmentLoadCommand(info);
            if (is_first_nonempty(cmd))
                return success(segment_info(cmd.vmaddr, cmd.fileoff));
        }
    }
    return failure("can't find any segments");
}

uint64_t image_base(const macho &obj) {
    if (auto first = first_segment(obj))
        return first->vmaddr;
    else
        return 0L;
}

void emit_dynamic_relocations(const macho &obj, command_info &info, ogre_doc &s);

void emit_macho_command(const macho &obj, command_info &info, ogre_doc &s) {
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
        emit_segment_command(obj.getSegment64LoadCommand(info), s);
    if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
        emit_segment_command(obj.getSegmentLoadCommand(info), s);
    if (info.C.cmd == MachO::LoadCommandType::LC_DYSYMTAB)
        emit_dynamic_relocations(obj, info, s);
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
MACHO_SECTION_FIELD(segname, std::string)

constexpr uint64_t max_uint64 = std::numeric_limits<uint64_t>::max();

error_or<uint64_t> entry_of_commands(const macho &obj) {
    uint64_t entry = max_uint64;
    for (auto info : macho_commands(obj)) {
        if (info.C.cmd == MachO::LoadCommandType::LC_MAIN) {
            const MachO::entry_point_command *entry_cmd =
                reinterpret_cast<const MachO::entry_point_command*>(info.Ptr);
            entry = entry_cmd->entryoff;
            if (obj.isLittleEndian() != sys::IsLittleEndianHost)
                sys::swapByteOrder(entry);
        }
    }
    if (entry != max_uint64)
        return success(entry);
    else
        return failure("entry not found");
}

error_or<uint64_t> entry_of_sections(const macho &obj) {
    uint64_t entry = max_uint64;
    for (auto sec : prim::sections(obj)) {
        auto addr = prim::section_address(sec);
        if (addr && section_flags(obj, sec) & MachO::S_ATTR_PURE_INSTRUCTIONS)
            entry = std::min(*addr, entry);
    }
    if (entry != max_uint64)
        return success(entry);
    else
        return failure("entry not found");
}

void entry_point(const macho &obj, ogre_doc &s) {
    if (auto entry = entry_of_commands(obj))
        s.entry("llvm:entry-point") << *entry;
    else if (auto entry = entry_of_sections(obj))
        s.entry("llvm:entry-point") << *entry;
}

void emit_image_info(const macho &obj, ogre_doc &s) {
    bool is_exec = is_executable(obj);
    s.entry("is-executable") << is_exec;
    if (is_exec)
        entry_point(obj, s);
    else
        s.raw_entry("(llvm:entry-point 0)");
}

uint32_t section_type(const macho &obj, SectionRef sec) {
    return section_flags(obj, sec) & MachO::SECTION_TYPE;
}

void section(const std::string &name, uint64_t rel_addr, uint64_t size, uint64_t off, ogre_doc &s) {
    s.entry("llvm:section-entry") << name << rel_addr << size << off;
}


void macho_symbol(const std::string &name, uint64_t value, ogre_doc &s) {
    s.entry("llvm:macho-symbol") << name << value;
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


void emit_relocation(const macho &obj, uint64_t off, symbol_iterator sym, section_iterator sec, ogre_doc &s) {
    if (sec != prim::end_sections(obj)) {
        if (auto saddr = prim::section_address(*sec) && sym != prim::end_symbols(obj)) {
            auto raddr = saddr + off;
            if (auto addr = prim::symbol_address(*sym))
                if (*addr) s.entry("llvm:relocation") << raddr << *addr;
            if (auto name = prim::symbol_name(*sym))
                s.entry("llvm:name-reference") << raddr << *name;
        }
    }
}


void emit_relocation(const macho &obj, const RelocationRef &rel, section_iterator sec, ogre_doc &s) {
    emit_relocation(obj, prim::relocation_offset(rel), rel.getSymbol(), sec, s);
}

void emit_libraries(const macho &obj, ogre_doc &s) {
  for (const auto &entry : obj.load_commands()) {
    if (entry.C.cmd == MachO::LC_LOAD_DYLIB ||
        entry.C.cmd == MachO::LC_ID_DYLIB ||
        entry.C.cmd == MachO::LC_LOAD_WEAK_DYLIB ||
        entry.C.cmd == MachO::LC_REEXPORT_DYLIB ||
        entry.C.cmd == MachO::LC_LAZY_LOAD_DYLIB ||
        entry.C.cmd == MachO::LC_LOAD_UPWARD_DYLIB) {
      MachO::dylib_command cmd = obj.getDylibIDLoadCommand(entry);
      if (cmd.dylib.name < cmd.cmdsize) {
        auto *name = static_cast<const char*>(entry.Ptr) + cmd.dylib.name;
        s.entry("require") << name;
      }
    }
  }
}

symbol_iterator get_symbol(const macho &obj, std::size_t index);

void emit_dyn_relocation(const macho &obj, uint32_t sym_num, uint64_t off, ogre_doc &s) {
    auto sym = get_symbol(obj, sym_num);
    if (sym != prim::end_symbols(obj)) {
        auto name = prim::symbol_name(*sym);
        auto first = first_segment(obj);
        uint64_t base = image_base(obj);
        if (name && first) {
            uint64_t raddr = base + (off - first->offset);
            if (auto addr = prim::symbol_address(*sym))
                if (*addr) s.entry("llvm:relocation") << raddr << *addr;
            s.entry("llvm:name-reference") << raddr << *name;
        }
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


void emit_relocations(const macho &obj, ogre_doc &s) {
    for (auto sec : prim::sections(obj))
        for (auto rel : prim::relocations(sec))
            if (auto rel_sec = prim::relocated_section(sec))
                emit_relocation(obj, rel, *rel_sec, s);
}

bool is_code_section(const macho &obj, const SectionRef &sec) {
    auto flags = section_flags(obj,sec);
    return static_cast<bool>
        (flags & MachO::S_ATTR_PURE_INSTRUCTIONS ||
         flags & MachO::S_ATTR_SOME_INSTRUCTIONS);
}

void emit_sections(const macho &obj, ogre_doc &s) {
    for (auto sec : prim::sections(obj)) {
        auto addr = prim::section_address(sec);
        auto size = prim::section_size(sec);
        auto name = prim::section_name(sec);
        auto offs = section_offset(obj, section_iterator(sec));
        if (addr && name && size) {
            section(*name, *addr, *size, offs, s);
            if (is_code_section(obj, sec))
                s.entry("llvm:code-entry") << *name << offs << *size;
        }
    }
}

void emit_symbols(const macho &obj, const prim::symbols_sizes &sizes, ogre_doc &s) {
    for (auto sized_sym : sizes) {
        auto sym = sized_sym.first;
        auto size = sized_sym.second;
        auto name = prim::symbol_name(sym);
        if (name) {
            if (is_in_section(obj, sym)) {
                auto addr = prim::symbol_address(sym);
                auto offs = symbol_file_offset(obj, sym);
                auto type = prim::symbol_type(sym);
                if (addr && offs && type) {
                    s.entry("llvm:symbol-entry") << *name
                                                 << *addr
                                                 << size
                                                 << *offs
                                                 << symbol_value(obj, sym);
                    if (*type == SymbolRef::ST_Function)
                        s.entry("llvm:code-entry") << *name << *offs << size;
                }
            }
            else {
                macho_symbol(*name, symbol_value(obj, sym), s);
            }
        }
    }
}

void emit_macho_commands(const macho &obj, ogre_doc &s) {
    auto cmds = macho_commands(obj);
    for (auto cmd : cmds)
        emit_macho_command(obj, cmd, s);
}

bool is_rel_scattered(const macho &obj, const MachO::any_relocation_info &rel) {
    return !obj.is64Bit() && (rel.r_word0 & MachO::R_SCATTERED);
}

bool is_rel_extern(const macho &obj, const MachO::any_relocation_info &rel) {
    if (obj.isLittleEndian())
        return (rel.r_word1 >> 27) & 1;
    else
        return (rel.r_word1 >> 4) & 1;
}

uint32_t rel_symbolnum(const macho &obj, const MachO::any_relocation_info &rel) {
    if (obj.isLittleEndian())
        return rel.r_word1 & 0xffffff;
    else
        return rel.r_word1 >> 8;
}

error_or<MachO::any_relocation_info> get_rel(const macho &obj, uint64_t pos) {
    uint64_t max_pos = obj.getData().size();
    if (pos + sizeof(MachO::any_relocation_info) > max_pos)
        return failure("request data beyond object bounds");
    const char *ptr = prim::get_raw_data(obj) + pos;
    MachO::any_relocation_info rel;
    std::copy_n(ptr, sizeof(MachO::any_relocation_info), reinterpret_cast<char *>(&rel));
    if (obj.isLittleEndian() != sys::IsLittleEndianHost)
        MachO::swapStruct(rel);
    if (is_rel_scattered(obj, rel)) {
        return failure("scattered relocations are not supported");
    }
    return success(rel);
}

// Note, that if r_extern is set to 1, then r_symbolnum contains an index in symbtab, and
// section index otherwise.
void emit_dyn_relocations(const macho &obj, uint64_t file_pos, uint32_t num, ogre_doc &s) {
    for (std::size_t i = 0; i < num; ++i) {
        auto rel = get_rel(obj, file_pos + i * sizeof(MachO::any_relocation_info));
        if (rel && is_rel_extern(obj, *rel))
            emit_dyn_relocation(obj, rel_symbolnum(obj, *rel), rel->r_word0, s);
    }
}

symbol_iterator get_indirect_symbol(const macho &obj, const MachO::dysymtab_command &dlc, uint32_t index) {
    uint64_t max_offset = obj.getData().size();

    // this is actually a check that we will not get out of object
    // bounds while calling getIndirectSymbolTableEntry.
    // llvm does approximately the same to raise a nasty
    // runtime LLVM_ERROR
    uint64_t file_offset = dlc.indirectsymoff + index * sizeof(uint32_t);
    if (file_offset < max_offset)
        return get_symbol(obj, obj.getIndirectSymbolTableEntry(dlc, index));
    else
        return prim::end_symbols(obj);
}

// returns stride for indirect symbols entries
uint32_t indirect_symbols_stride(const macho &obj, const SectionRef &sec) {
    if (section_type(obj, sec) == MachO::S_SYMBOL_STUBS)
        return section_reserved2(obj, sec);
    else
        return obj.is64Bit() ? 8 : 4;
}

// true for symbol stubs or symbol pointers sections
bool contains_sym_stubs(const macho &obj, const SectionRef &sec) {
    switch (section_type(obj, sec)) {
    case MachO::S_NON_LAZY_SYMBOL_POINTERS:
    case MachO::S_LAZY_SYMBOL_POINTERS:
    case MachO::S_LAZY_DYLIB_SYMBOL_POINTERS:
    case MachO::S_THREAD_LOCAL_VARIABLE_POINTERS:
    case MachO::S_SYMBOL_STUBS:
        return true;
    default:
        return false;
    }
}

void emit_indirect_symbols(const macho &obj, const MachO::dysymtab_command &dlc, ogre_doc &s) {
    for (auto sec : prim::sections(obj)) {
        auto stride = indirect_symbols_stride(obj, sec);
        if (contains_sym_stubs(obj, sec) && stride != 0) {
            uint64_t sym_numb = section_size(obj, sec) / stride;
            uint32_t tab_indx = section_reserved1(obj, sec);
            uint64_t sec_addr = section_addr(obj, sec);

            for (uint32_t j = 0; j < sym_numb && tab_indx + j < dlc.nindirectsyms; j++) {
                auto sym = get_indirect_symbol(obj, dlc, tab_indx + j);
                if (sym != prim::end_symbols(obj)) {
                    if (auto name = prim::symbol_name(*sym)) {
                        auto sym_addr = sec_addr + j * stride;
                        s.entry("llvm:name-reference") << sym_addr << *name;
                    }
                }
            }
        }
    }
}

// It's not enough just to iterate over sections in order to get relocations.
// We are going to take a look for indirect symbols and external relocations.
// Speaking about local relocations referenced from dyn sym tab, even modern (6.0)
// does not perform a lot of job about them, so let's do the same. One can take
// a look at implementation of MachOObjectFile::getRelocationSymbol to get the
// whole picture about how llvm tries to get symbol iterator from relocation,
void emit_dynamic_relocations(const macho &obj, command_info &info, ogre_doc &s) {
    if (info.C.cmd == MachO::LoadCommandType::LC_DYSYMTAB) {
        MachO::dysymtab_command cmd = obj.getDysymtabLoadCommand();
        emit_indirect_symbols(obj, cmd, s);
        emit_dyn_relocations(obj, cmd.extreloff, cmd.nextrel, s);
     }
}


commands macho_commands(const macho &obj) {
    commands cmds;
    for (auto c : obj.load_commands())
        cmds.push_back(c);
    return cmds;
}

void emit_symbols(const macho &obj, ogre_doc &s) {
    auto sizes = prim::get_symbols_sizes(obj);
    emit_symbols(obj, sizes, s);
}

// It's safe to call getSymtabLoadCommand without any checks,
// because in case of symtab absence, llvm returns just a
// struct with all fields set to zero (for llvm version >= 3.8).
symbol_iterator get_symbol(const macho &obj, std::size_t index) {
    MachO::symtab_command symtab = obj.getSymtabLoadCommand();
    if (index >= symtab.nsyms)
        return prim::end_symbols(obj);
    return obj.getSymbolByIndex(index);
}
} // namespace macho_loader

error_or<std::string> load(ogre_doc &s, const llvm::object::MachOObjectFile &obj) {
    using namespace macho_loader;
    s.raw_entry("(format macho)");
    s.entry("llvm:base-address") << image_base(obj);
    emit_image_info(obj, s);
    emit_macho_commands(obj, s);
    emit_sections(obj, s);
    emit_symbols(obj, s);
    emit_relocations(obj, s);
    emit_libraries(obj, s);
    return s.str();
}

} // namespace loader

#endif //  LLVM_MACHO_LOADER_HPP
