#ifndef LLVM_COFF_LOADER_HPP
#define LLVM_COFF_LOADER_HPP

#include <tuple>

#include <llvm/Object/COFF.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"
#include "llvm_primitives.hpp"

#include "llvm_pdb_loader.hpp"


namespace loader {
namespace coff_loader {

using namespace llvm;
using namespace llvm::object;

typedef COFFObjectFile coff_obj;

bool is_code_section(const coff_section &sec) {
    return
        static_cast<bool>(
            sec.Characteristics & COFF::IMAGE_SCN_MEM_EXECUTE ||
            sec.Characteristics & COFF::IMAGE_SCN_CNT_CODE);
}

void emit_section(const coff_section &sec, uint64_t base, bool is_rel, ogre_doc &s) {
    bool r = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_READ);
    bool w = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_WRITE);
    bool x = is_code_section(sec);
    uint64_t vaddr = sec.VirtualAddress + base;
    uint64_t vsize = is_rel ? sec.SizeOfRawData : sec.VirtualSize;
    uint64_t fsize = sec.SizeOfRawData;
    uint64_t off = sec.PointerToRawData;
    s.entry("llvm:section-entry") << sec.Name << vaddr << fsize << off;
    s.entry("llvm:coff-virtual-section-header") << sec.Name << vaddr << vsize;
    s.entry("llvm:section-flags") << sec.Name << r << w << x;
    if (x) s.entry("llvm:code-entry") << sec.Name << off << fsize;
}

error_or<uint64_t> symbol_file_offset(const coff_obj &obj, const SymbolRef &sym);
const coff_section* get_coff_section(const coff_obj &obj, const SectionRef &sec);
error_or<int> section_number(const coff_obj &obj, const SymbolRef &sym);
error_or<uint64_t> symbol_value(const coff_obj &obj, const SymbolRef &sym);

#if LLVM_VERSION_MAJOR >= 11
const coff_section * get_coff_section(const coff_obj &obj, std::size_t index) {
    if (index != COFF::IMAGE_SYM_UNDEFINED) {
        auto sec = obj.getSection(index);
        return sec ? *sec : nullptr;
    } else {
        return nullptr;
    }
}
#else
const coff_section * get_coff_section(const coff_obj &obj, std::size_t index) {
    const coff_section *sec = nullptr;
    bool fail = (index == COFF::IMAGE_SYM_UNDEFINED) || obj.getSection(index, sec);
    if (fail) return nullptr;
    else return sec;
}

#endif


void emit_base_address(const coff_obj &obj, ogre_doc &s) {
    s.entry("llvm:base-address") << obj.getImageBase();
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

bool is_executable(const COFFObjectFile &obj) {
    return obj.getCharacteristics() & COFF::IMAGE_FILE_EXECUTABLE_IMAGE;
}


void emit_entry_point(const coff_obj &obj, ogre_doc &s) {
    auto entry = 0L;
    if (obj.getBytesInAddress() == 4) {
        if (auto hdr = get_pe32_header(obj))
            entry = hdr->AddressOfEntryPoint;
    } else {
        if (auto hdr = get_pe32plus_header(obj))
            entry = hdr->AddressOfEntryPoint;
    }
    s.entry("llvm:entry-point") << entry + obj.getImageBase();
}

uint64_t section_offset(const coff_obj &obj, section_iterator sec) {
    auto coff_sec = get_coff_section(obj, *sec);
    return coff_sec->PointerToRawData;
}

void emit_relocation(const coff_obj &obj, const RelocationRef &rel, section_iterator sec, ogre_doc &s) {
    auto sym = rel.getSymbol();
    if (sym != prim::end_symbols(obj)) {
        if (auto saddr = prim::section_address(*sec)) {
            auto raddr = prim::relocation_offset(rel) + *saddr;
            if (auto addr = prim::symbol_address(*sym))
                if (*addr) s.entry("llvm:relocation") << raddr << *addr;
            if (auto name = prim::symbol_name(*sym))
                s.entry("llvm:name-reference") << raddr << *name;
        }
    }
}

void emit_sections(const coff_obj &obj, ogre_doc &s) {
    uint64_t base = obj.getImageBase();
    bool is_rel = obj.isRelocatableObject();
    for (auto sec : prim::sections(obj))
        emit_section(*get_coff_section(obj, sec), base, is_rel, s);
}

#if LLVM_VERSION_MAJOR >= 11
uint64_t get_symbol_value(const SymbolRef &sym) {
    auto value = sym.getValue();
    return value ? *value : 0;
}
#else
uint64_t get_symbol_value(const SymbolRef &sym) {
   return sym.getValue();
}
#endif

void emit_symbols(const coff_obj &obj, ogre_doc &s) {
    for (auto sized_sym : prim::get_symbols_sizes(obj)) {
        auto sym = sized_sym.first;
        auto name = prim::symbol_name(sym);
        auto type = prim::symbol_type(sym);
        auto addr = prim::symbol_address(sym);
        auto offs = symbol_file_offset(obj, sym);
        if (name && type && addr && offs) {
            s.entry("llvm:symbol-entry") << *name
                                         << *addr
                                         << sized_sym.second
                                         << *offs
                                         << get_symbol_value(sym);
            if (*type == SymbolRef::ST_Function)
                s.entry("llvm:code-entry") << *name << *offs << sized_sym.second;
        }
    }
}

void emit_relocations(const coff_obj &obj, ogre_doc &s) {
    for (auto sec : prim::sections(obj))
        for (auto rel : prim::relocations(sec)) {
            if (auto rel_sec = prim::relocated_section(sec))
                emit_relocation(obj, rel, *rel_sec, s);
        }
}


typedef support::ulittle16_t ulittle16_t;
typedef support::ulittle32_t ulittle32_t;
typedef support::ulittle64_t ulittle64_t;

//RVA - relative virtual address
struct export_directory_table_entry {
  ulittle32_t export_flags;
  ulittle32_t time_date_stamp;
  ulittle16_t major_version;
  ulittle16_t minor_version;
  ulittle32_t name_rva;
  ulittle32_t ordinal_base;
  ulittle32_t address_table_entries;
  ulittle32_t number_of_name_pointers;
  ulittle32_t export_address_table_rva;
  ulittle32_t name_pointer_rva;
  ulittle32_t ordinal_table_rva;
};

union export_address_table_entry {
  ulittle32_t export_rva;
  ulittle32_t forwarder_rva;
};

typedef std::pair<uint64_t, std::string> exported_sym;
typedef std::vector<exported_sym> exports;

const coff_section* get_coff_section(const coff_obj &obj, const exported_sym &sym) {
    for (auto sec : prim::sections(obj)) {
        auto *c = get_coff_section(obj, sec);
        if (c->VirtualAddress <= sym.first && sym.first < c->VirtualAddress + c->VirtualSize)
            return c;
    }
    return nullptr;
}

uint64_t exp_sym_size(const coff_section *sec, const exported_sym &sym) {
    return sec->VirtualAddress + sec->VirtualSize - sym.first;
}

uint64_t exp_sym_size(const exported_sym &sym, const exported_sym &next) {
    return next.first - sym.first;
}

uint64_t exp_sym_offset(const coff_section *sec, const exported_sym &sym) {
    return sec->PointerToRawData + sym.first - sec->VirtualAddress;
}

bool same_sections(const coff_obj &obj, const exported_sym &sym1, const exported_sym &sym2) {
    auto *c1 = get_coff_section(obj, sym1);
    auto *c2 = get_coff_section(obj, sym2);
    if (!c1 || !c2) return false;
    return (c1 == c2);
}

void emit_exported_symbols(const coff_obj &obj, exports &syms, ogre_doc &s) {
    uint64_t base = obj.getImageBase();
    std::sort(syms.begin(), syms.end(),
              [](const exported_sym &x, const exported_sym &y)
              { return x.first < y.first; });
    auto last = syms.size() - 1;
    for (std::size_t i = 0; i <= last; ++i) {
        auto *c = get_coff_section(obj, syms[i]);
        if (!c) continue;
        uint64_t size = 0;
        if (i == last)
            size = exp_sym_size(c, syms[i]);
        else if (same_sections(obj, syms[i], syms[i + 1]))
            size = exp_sym_size(syms[i], syms[i + 1]);
        else
            size = exp_sym_size(c, syms[i]);
        uint64_t offs = exp_sym_offset(c, syms[i]);
        uint64_t addr = syms[i].first + base;
        s.entry("llvm:symbol-entry") << syms[i].second << addr << size << offs << 0;
        s.entry("llvm:code-entry") << syms[i].second << offs << size;
    }
}

#if LLVM_VERSION_MAJOR >= 11
const data_directory *get_export_table(const coff_obj &obj) {
    return obj.getDataDirectory(COFF::EXPORT_TABLE);
}
#else
const data_directory *get_export_table(const coff_obj &obj) {
    const data_directory *data_entry;
    if (obj.getDataDirectory(COFF::EXPORT_TABLE, data_entry)) {
        return nullptr;
    } else {
        return data_entry;
    }
}
#endif // llvm >= 11

void emit_exported_symbols(const coff_obj &obj, ogre_doc &s) {

    uintptr_t ptr = 0;
    const data_directory *data_entry = get_export_table(obj);
    if (!data_entry) return;
    uint32_t export_table_rva = data_entry->RelativeVirtualAddress;
    if (!export_table_rva) return;
    if (obj.getRvaPtr(export_table_rva, ptr)) return;
    auto *export_dir = reinterpret_cast<const export_directory_table_entry *>(ptr);

    auto num_ents = export_dir->number_of_name_pointers;
    if (obj.getRvaPtr(export_dir->ordinal_table_rva, ptr)) return;
    auto *start = reinterpret_cast<const ulittle16_t *>(ptr);
    exports v;
    for (std::size_t i = 0; i < export_dir->number_of_name_pointers; ++i) {
        int off = 0;
        for (const ulittle16_t *j = start, *e = start + num_ents; j < e; ++j, ++off) {
            if (i != *j) continue;
            if (obj.getRvaPtr(export_dir->name_pointer_rva, ptr)) continue;
            const ulittle32_t* name_ptr = reinterpret_cast<const ulittle32_t *>(ptr);
            if (obj.getRvaPtr(name_ptr[off], ptr)) continue;
            StringRef name(reinterpret_cast<const char *>(ptr));
            if (obj.getRvaPtr(export_dir->export_address_table_rva, ptr)) continue;
            auto *ent = reinterpret_cast<const export_address_table_entry *>(ptr);
            auto addr = ent[i].export_rva;
            v.push_back(std::make_pair(addr, name.str()));
            break;
        }
    }
    emit_exported_symbols(obj, v, s);
}

error_or<int> section_number(const coff_obj &obj, const SymbolRef &s) {
    auto sym = obj.getCOFFSymbol(s);
    return success(int(sym.getSectionNumber()));
}

error_or<uint64_t> symbol_value(const coff_obj &obj, const SymbolRef &s) {
    auto sym = obj.getCOFFSymbol(s);
    return success(uint64_t(sym.getValue()));
}

const coff_section* get_coff_section(const coff_obj &obj, const SectionRef &sec) {
    return obj.getCOFFSection(sec);
}

error_or<uint64_t> symbol_file_offset(const coff_obj &obj, const SymbolRef &sym) {
    auto coff_sym = obj.getCOFFSymbol(sym);
    auto num = coff_sym.getSectionNumber();

    //check for special section number
    if (num == COFF::IMAGE_SYM_UNDEFINED ||
        num == COFF::IMAGE_SYM_ABSOLUTE ||
        num == COFF::IMAGE_SYM_DEBUG)
        return success(coff_sym.getValue());

    const coff_section *coff_sec = get_coff_section(obj, num);
    if (coff_sec) {
        uint64_t off = coff_sec->PointerToRawData + coff_sym.getValue();
        return success(off);
    } else {
        return failure("failed to get the section");
    }
}
} // namespace coff_loader

error_or<std::string> load(ogre_doc &s, const llvm::object::COFFObjectFile &obj, const char* pdb_path) {
    using namespace coff_loader;
    s.raw_entry("(format coff)");
    s.entry("is-executable") << is_executable(obj);
    emit_base_address(obj, s);
    emit_entry_point(obj, s);
    emit_sections(obj, s);
    emit_symbols(obj, s);
    emit_relocations(obj, s);
    emit_exported_symbols(obj, s);
    if (pdb_path)
        pdb_loader::load(obj, pdb_path, s);
    return s.str();
}

} //namespace loader

#endif // LLVM_COFF_LOADER_HPP
