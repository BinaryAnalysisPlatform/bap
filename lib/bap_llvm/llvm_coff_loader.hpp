#ifndef LLVM_COFF_LOADER_HPP
#define LLVM_COFF_LOADER_HPP

#include <tuple>

#include <llvm/Object/COFF.h>

#include "llvm_error_or.hpp"
#include "llvm_loader_utils.hpp"
#include "llvm_primitives.hpp"

namespace loader {
namespace coff_loader {

using namespace llvm;
using namespace llvm::object;

typedef COFFObjectFile coff_obj;

static const std::string coff_declarations =
    "(declare file-type (name str))"
    "(declare arch (name str))"
    "(declare default-base-address (addr int))"
    "(declare entry (relative-addr int))"
    "(declare section-entry (name str) (relative-addr int) (size int) (off int))"
    "(declare virtual-section-header (name str) (relative-addr int) (size int))"
    "(declare code-entry (name str) (off int) (size int))"
    "(declare section-flags (name str) (r bool) (w bool) (x bool))"
    "(declare symbol-entry (name str) (relative-addr int) (size int) (off int))"
    "(declare function (off int) (name str))"
    "(declare relocatable (flag bool))"
    "(declare ref-internal (sym-off int) (rel-off int))"
    "(declare ref-external (rel-off int) (name str))";

void section(const coff_section &sec, uint64_t image_base,  ogre_doc &s) {
    bool r = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_READ);
    bool w = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_WRITE);
    bool x = static_cast<bool>(sec.Characteristics & COFF::IMAGE_SCN_MEM_EXECUTE);
    s.entry("section-entry") << sec.Name << sec.VirtualAddress << sec.SizeOfRawData << sec.PointerToRawData;
    s.entry("virtual-section-header") << sec.Name << sec.VirtualAddress << sec.VirtualSize;
    s.entry("section-flags") << sec.Name << r << w << x;
    auto c = sec.Characteristics;
    if ((c & COFF::IMAGE_SCN_CNT_CODE) ||
        (c & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA) ||
        (c & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA))
        s.entry("code-entry") << sec.Name << sec.PointerToRawData << sec.SizeOfRawData;
}

void symbol(const std::string &name, uint64_t relative_addr, uint64_t size, uint64_t off, SymbolRef::Type typ, ogre_doc &s) {
    s.entry("symbol-entry") << name << relative_addr << size << off;
    if (typ == SymbolRef::ST_Function)
        s.entry("code-entry") << name << off << size;
}

error_or<pe32plus_header> get_pe32plus_header(const coff_obj& obj);
error_or<uint64_t> get_image_base(const coff_obj &obj);
bool is_relocatable(const coff_obj &obj);
bool is_external_symbol(const coff_obj &obj, symbol_iterator s);
error_or<uint64_t> symbol_file_offset(const coff_obj &obj, const SymbolRef &sym);
const coff_section* get_coff_section(const coff_obj &obj, const SectionRef &sec);
error_or<int> section_number(const coff_obj &obj, const SymbolRef &sym);
error_or<uint64_t> symbol_value(const coff_obj &obj, const SymbolRef &sym);
error_or<uint64_t> symbol_relative_address(const coff_obj &obj, const SymbolRef &sym);

const coff_section * get_coff_section(const coff_obj &obj, std::size_t index) {
    const coff_section *sec = nullptr;
    bool fail = (index == COFF::IMAGE_SYM_UNDEFINED) || obj.getSection(index, sec);
    if (fail) return nullptr;
    else return sec;
}

void image_base(const coff_obj &obj, ogre_doc &s) {
    auto base = get_image_base(obj);
    if (base)
        s.entry("default-base-address") << *base;
}

void entry_point(const coff_obj &obj, ogre_doc &s) {
    // coff object files may not have a PE/PE+ header
    if (is_relocatable(obj)) {
        s.entry("entry") << 0;
        return;
    }
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (auto ec = obj.getPE32Header(hdr)) { s.fail(ec.message()); return; }
        if (!hdr) { s.fail("PE header not found"); return; }
        s.entry("entry") << hdr->AddressOfEntryPoint;
    } else {
        error_or<pe32plus_header> hdr = get_pe32plus_header(obj);
        if (!hdr) { s.fail("PE+ header not found"); return; }
        s.entry("entry") << hdr->AddressOfEntryPoint;
    }
}

error_or<uint64_t> symbol_address(const coff_obj &obj, const SymbolRef &sym) {
    if (is_relocatable(obj)) return success(uint64_t(0));
    else return prim::symbol_address(sym);
}

uint64_t section_offset(const coff_obj &obj, section_iterator sec) {
    auto coff_sec = get_coff_section(obj, *sec);
    return coff_sec->PointerToRawData;
}

void symbol_reference(const coff_obj &obj, const RelocationRef &rel, section_iterator sec, ogre_doc &s) {
    auto it = rel.getSymbol();
    if (it == prim::end_symbols(obj)) return;
    auto sec_offset = section_offset(obj, sec);
    auto off = prim::relocation_offset(rel) + sec_offset; // relocation file offset
    if (is_external_symbol(obj, it)) {
        if (auto name = prim::symbol_name(*it))
            s.entry("ref-external") << off << *name;
    } else {
        if (auto file_offset = symbol_file_offset(obj, *it))
            s.entry("ref-internal") << *file_offset << off;
    }
}

void sections(const coff_obj &obj, ogre_doc &s) {
    auto base = get_image_base(obj);
    if (!base) { s.fail(base.message()); return; }
    for (auto sec : prim::sections(obj))
        section(*get_coff_section(obj, sec), *base, s);
}

void symbols(const coff_obj &obj, ogre_doc &s) {
    for (auto sized_sym : prim::get_symbols_sizes(obj)) {
        auto sym = sized_sym.first;
        auto name = prim::symbol_name(sym);
        auto type = prim::symbol_type(sym);
        auto addr = symbol_relative_address(obj, sym);
        auto offs = symbol_file_offset(obj, sym);
        if (!name || !type || !addr || !offs) continue;
        symbol(*name, *addr, sized_sym.second, *offs, *type, s);
    }
}

void relocations(const coff_obj &obj, ogre_doc &s) {
    for (auto sec : prim::sections(obj))
        for (auto rel : prim::relocations(sec))
            symbol_reference(obj, rel, sec.getRelocatedSection(), s);
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

void exported_symbols(const coff_obj &obj, exports &syms, ogre_doc &s) {
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
        auto offs = exp_sym_offset(c, syms[i]);
        s.entry("symbol-entry") << syms[i].second << syms[i].first << size << offs;
        s.entry("code-entry") << syms[i].second << offs << size;
    }
}

void exported_symbols(const coff_obj &obj, ogre_doc &s) {
    auto base = get_image_base(obj);
    if (!base) { s.fail(base.message()); return; }

    const data_directory *data_entry;
    uintptr_t ptr = 0;

    if (obj.getDataDirectory(COFF::EXPORT_TABLE, data_entry)) return;
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
    exported_symbols(obj, v, s);
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8 \
    || LLVM_VERSION_MAJOR == 4 && LLVM_VERSION_MINOR == 0

error_or<uint64_t> symbol_relative_address(const coff_obj &obj, const SymbolRef &sym) {
    auto base = obj.getImageBase();
    auto addr = symbol_address(obj, sym);
    if (!addr) return addr;
    auto raddr = prim::relative_address(base, *addr);
    return success(raddr);
}

error_or<int> section_number(const coff_obj &obj, const SymbolRef &s) {
    auto sym = obj.getCOFFSymbol(s);
    return success(int(sym.getSectionNumber()));
}

error_or<uint64_t> symbol_value(const coff_obj &obj, const SymbolRef &s) {
    auto sym = obj.getCOFFSymbol(s);
    return success(uint64_t(sym.getValue()));
}

bool is_relocatable(const coff_obj &obj) {
    return obj.isRelocatableObject();
}

bool is_external_symbol(const coff_obj &obj, symbol_iterator it) {
    auto coff_sym = obj.getCOFFSymbol(*it);
    return coff_sym.isExternal();
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

    const coff_section *coff_sec;
    if (auto er = obj.getSection(num, coff_sec)) {
        return failure(er.message());
    }
    uint64_t off = coff_sec->PointerToRawData + coff_sym.getValue();
    return success(off);
}

error_or<uint64_t> get_image_base(const coff_obj &obj) {
    return success(obj.getImageBase());
}

error_or<pe32plus_header> get_pe32plus_header(const coff_obj &obj) {
    const pe32plus_header *hdr = 0;
    auto ec = obj.getPE32PlusHeader(hdr);
    if (ec) return failure(ec.message());
    else if (!hdr) { return failure("PE+ header not found"); }
    else return success(*hdr);
}

#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4

// symbol address for 3.4 is already relative, i.e. doesn't include image base
error_or<uint64_t> symbol_relative_address(const coff_obj &obj, const SymbolRef &sym) {
    return symbol_address(obj, sym);
}

bool is_relocatable(const coff_obj &obj) {
    std::size_t n = 0;
    for (auto s : prim::sections(obj))
        for (auto r : prim::relocations(s))
            ++n;
    return (n != 0);
}

bool is_external_symbol(const coff_obj &obj, symbol_iterator s) {
    auto coff_sym = obj.getCOFFSymbol(s);
    return coff_sym->StorageClass == COFF::IMAGE_SYM_CLASS_EXTERNAL;
}

const coff_section* get_coff_section(const coff_obj &obj, const SectionRef &sec) {
    section_iterator it(sec);
    return obj.getCOFFSection(it);
}

error_or<uint64_t> symbol_file_offset(const coff_obj &obj, const SymbolRef &sym) {
    uint64_t off;
    if (auto er = sym.getFileOffset(off)) return failure(er.message());
    return success(off);
}

error_or<pe32plus_header> get_pe32plus_header(const coff_obj &obj) {
    uint64_t cur_ptr = 0;
    const char *buf = (obj.getData()).data();
    const uint8_t *start = reinterpret_cast<const uint8_t *>(buf);
    uint8_t b0 = start[0];
    uint8_t b1 = start[1];
    if (b0 == 0x4d && b1 == 0x5a) { // check if this is a PE/COFF file
        // a pointer at offset 0x3C points to the
        cur_ptr += *reinterpret_cast<const uint16_t *>(start + 0x3c);
        // check the PE magic bytes.
        if (std::memcmp(start + cur_ptr, "PE\0\0", 4) != 0)
            return failure("PE Plus header not found");
        cur_ptr += 4; // skip the PE magic bytes.
        cur_ptr += sizeof(llvm::object::coff_file_header);
        auto p = reinterpret_cast<const pe32plus_header *>(start + cur_ptr);
        return error_or<pe32plus_header>(*p);
    }
    return failure("Failed to extract PE32+ header");
}

error_or<uint64_t> get_image_base(const COFFObjectFile &obj) {
    if (is_relocatable(obj)) return success(uint64_t(0));
    if (obj.getBytesInAddress() == 4) {
        const pe32_header *hdr;
        if (error_code ec = obj.getPE32Header(hdr))
            return failure(ec.message());
        return error_or<uint64_t>(hdr->ImageBase);
    } else {
        error_or<pe32plus_header> hdr = get_pe32plus_header(obj);
        if (!hdr) return hdr;
        return std::move(error_or<uint64_t>(hdr->ImageBase) << hdr.warnings());
    }
}

error_or<int> section_number(const coff_obj &obj, const SymbolRef &s) {
    symbol_iterator it(s);
    if (auto sym = obj.getCOFFSymbol(it))
        return success(int(sym->SectionNumber));
    else failure("Failed to obtain coff symbol");
}

error_or<uint64_t> symbol_value(const coff_obj &obj, const SymbolRef &s) {
    symbol_iterator it(s);
    if (auto sym = obj.getCOFFSymbol(it))
        return success(uint64_t(sym->Value));
    else failure("Failed to obtain coff symbol");
}

#else
#error LLVM version is not supported
#endif


} // namespace coff_loader


error_or<std::string> load(const llvm::object::COFFObjectFile &obj) {
    using namespace coff_loader;
    ogre_doc s;
    s.raw_entry(coff_declarations);
    s.raw_entry("(file-type coff)");
    s.entry("arch") << prim::arch_of_object(obj);
    s.entry("relocatable") << is_relocatable(obj);
    image_base(obj, s);
    entry_point(obj, s);
    sections(obj, s);
    symbols(obj, s);
    relocations(obj, s);
    exported_symbols(obj, s);
    return s.str();
}

} //namespace loader

#endif // LLVM_COFF_LOADER_HPP
