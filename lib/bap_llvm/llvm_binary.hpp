#ifndef LLVM_BINARY_HPP
#define LLVM_BINARY_HPP

#include <memory>
#include <numeric>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <tuple>
#include <cstdlib>

#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/Archive.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Config/llvm-config.h>

#include "llvm_binary_34.hpp"
#include "llvm_binary_38.hpp"
#include "llvm_error_or.hpp"

extern "C" {
    void bap_notify_error(const char*);
    void bap_notify_warning(const char*);
}

void bap_notify_error(const std::string & message) {
    bap_notify_error(message.c_str());
}

void bap_notify_warning(const std::string & message) {
    bap_notify_warning(message.c_str());
}

namespace seg {
using namespace llvm;
using namespace llvm::object;

struct segment {
    std::string name;
    int offset;
    uint64_t addr;
    uint64_t size;
    bool is_readable;
    bool is_writable;
    bool is_executable;
};

typedef std::vector<segment> segments;

template <typename S>
segment make_segment(const S &s) {
    int off = s.vmsize == 0 ? -1 : s.fileoff;
    return segment{s.segname, off, s.vmaddr, s.filesize, //s.vmsize
            static_cast<bool>(s.initprot & MachO::VM_PROT_READ),
            static_cast<bool>(s.initprot & MachO::VM_PROT_WRITE),
            static_cast<bool>(s.initprot & MachO::VM_PROT_EXECUTE)};
}

segment make_segment(const coff_section &s, uint64_t image_base) {
    int offset = s.SizeOfRawData == 0 ? -1 : s.PointerToRawData;
    return segment{s.Name,
            static_cast<int>(offset),
            static_cast<uint64_t>(s.VirtualAddress + image_base),
            static_cast<uint64_t>(s.VirtualSize),
            static_cast<bool>((s.Characteristics) &
                              COFF::IMAGE_SCN_MEM_READ),
            static_cast<bool>((s.Characteristics) &
                              COFF::IMAGE_SCN_MEM_WRITE),
            static_cast<bool>((s.Characteristics) &
                              COFF::IMAGE_SCN_MEM_EXECUTE)};
}

template<typename T>
error_or<segments> read(const ELFObjectFile<T>& obj) {
    auto begin = elf_header_begin(obj.getELFFile());
    auto end   = elf_header_end(obj.getELFFile());
    segments s;
    s.reserve(std::distance(begin, end));
    auto it = begin;
    for (int pos = 0; it != end; ++it, ++pos) {
        if (it -> p_type == ELF::PT_LOAD) {
            std::ostringstream oss;
            oss << std::setfill('0') << std::setw(2) << pos;
            int offset = it->p_filesz == 0 ? -1 : it->p_offset;
            s.push_back(segment{oss.str(),
                        offset,
                        it->p_vaddr,
                        it->p_filesz, //it->p_memsz,
                        static_cast<bool>(it->p_flags & ELF::PF_R),
                        static_cast<bool>(it->p_flags & ELF::PF_W),
                        static_cast<bool>(it->p_flags & ELF::PF_X)});
        }
    }
    return success(std::move(s));
}

error_or<segments> read(const MachOObjectFile& obj) {
    auto cmds = load_commands(obj);
    segments s;
    for (auto it : cmds) {
        if (it.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
            s.push_back(make_segment(obj.getSegment64LoadCommand(it)));
        if (it.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
            s.push_back(make_segment(obj.getSegmentLoadCommand(it)));
    }
    return success(std::move(s));
}

error_or<segments> read(const COFFObjectFile& obj) {
    auto image_base = getImageBase(obj);
    auto obj_sections = sec::sections_range(obj);
    if (auto er = image_base || obj_sections)
        return failure(er.message());
    segments segs;
    for (auto it : *obj_sections) {
        const coff_section *s = obj.getCOFFSection(it);
        uint64_t c = static_cast<uint64_t>(s->Characteristics);
        if ( c & COFF::IMAGE_SCN_CNT_CODE ||
             c & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA ||
             c & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA )
            segs.push_back(make_segment(*s, *image_base));
    }
    return std::move(success(std::move(segs)) << obj_sections.warnings() << image_base.warnings());
}

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

typedef SymbolRef::Type kind_type;

struct symbol {
    std::string name;
    bool is_fun;
    bool is_debug;
    uint64_t addr;
    uint64_t size;
};

typedef std::vector<symbol> symbols;

error_or<symbol> make_symbol(const SymbolRef &sym, uint64_t size, error_or<uint64_t> addr) {
    auto name = get_name(sym);
    auto kind = get_kind(sym);
    if (auto er = name || kind || addr) {
        name << addr.warnings() << kind.warnings();
        info & w = name.warning();
        w << "skipping symbol ";
        std::string str = " error while extracting ";
        if (!name) w << str << "name: ";
        if (!addr) w << *name << str << "address: ";
        if (!kind) w << *name << " at " << std::hex << *addr << std::hex << str << "kind: ";
        w << er.message();
        return name;
    }
    bool is_fun = *kind == SymbolRef::ST_Function;
    bool is_dbg = *kind == SymbolRef::ST_Debug;
    return std::move(success(symbol{*name, is_fun, is_dbg, *addr, size}) << name.warnings());
}

template <typename T>
error_or<symbols> read(const T &obj) {
    auto symbol_sizes = getSymbolSizes(obj);
    if (!symbol_sizes) return symbol_sizes;
    error_or<symbols> syms = success(symbols());
    syms << symbol_sizes.warnings() ;
    for (auto it : *symbol_sizes) {
        auto sym = make_symbol(it.first, it.second, get_addr(it.first, obj));
        if (sym) syms->push_back(*sym);
        syms << sym.warnings();
    }
    return syms;
}

} //namespace sym

namespace sec {

using namespace llvm;
using namespace llvm::object;

struct section {
    std::string name;
    uint64_t addr;
    uint64_t size;
};

typedef std::vector<section> sections;

error_or<section> make_section(const SectionRef &sec) {
    auto descr = get_section(sec);
    if (!descr) return descr;
    std::string name;
    uint64_t addr, size;
    std::tie(name, addr, size) = *descr;
    return std::move(success(section{name, addr, size}) << descr.warnings());
}

section make_section(const coff_section &s, const uint64_t image_base) {
    std::string name;
    uint64_t addr, size;
    std::tie(name, addr, size) = get_section(s);
    return section{name, addr + image_base, size};
}

error_or<sections> read(const ObjectFile &obj) {
    auto obj_secs = sections_range(obj);
    if (!obj_secs) return obj_secs;
    error_or<sections> secs = success(sections());
    secs << obj_secs.warnings();
    for (section_iterator it : *obj_secs) {
        auto sec = make_section(*it);
        if (!sec) return sec;
        secs->push_back(*sec);
        secs << sec.warnings();
    }
    return secs;
}

error_or<sections> read(const COFFObjectFile &obj) {
    auto image_base = getImageBase(obj);
    auto obj_secs = sections_range(obj);
    if (auto er = image_base || obj_secs)
        return failure(er.message());
    error_or<sections> secs = success(sections());
    secs << image_base.warnings() << obj_secs.warnings();
    for (section_iterator it : *obj_secs) {
        const coff_section *s = getCOFFSection(obj, it);
        secs->push_back(make_section(*s, *image_base));
    }
    return secs;
}

} //namespace sec

namespace img {

using std::move;
using namespace llvm;
using namespace llvm::object;

struct image {
    std::string arch;
    uint64_t entry;
    seg::segments segments;
    sym::symbols  symbols;
    sec::sections sections;
};

std::string image_arch(const ObjectFile& obj) {
    return Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
}

template <typename ELFT>
error_or<uint64_t> image_entry(const ELFObjectFile<ELFT>& obj) {
    return error_or<uint64_t>(obj.getELFFile()->getHeader()->e_entry);
}

error_or<uint64_t> image_entry(const MachOObjectFile &obj) {
    return image_entry_macho(obj);
}

error_or<uint64_t> image_entry(const COFFObjectFile &obj) {
    return image_entry_coff(obj);
}

void print_warnings(const std::vector<std::string> &warns) {
    if(const char* env_p = std::getenv("BAP_DEBUG")) {
        if (std::string(env_p) == "1")
            for (auto w : warns)
                bap_notify_warning(w);
    }
}

template <typename T>
image* create_image(error_or<object::Binary> &binary) {
    if (auto ptr = llvm::dyn_cast<T>(binary.get())) {
        auto arch     = image_arch(*ptr);
        auto entry    = image_entry(*ptr);
        auto segments = seg::read(*ptr);
        auto symbols  = sym::read(*ptr);
        auto sections = sec::read(*ptr);

        if (auto er = entry || segments || symbols || sections) {
            bap_notify_error(er.message());
            return nullptr;
        }
        binary << entry.warnings() << segments.warnings() << symbols.warnings() << sections.warnings();
        print_warnings(binary.warnings());

        return (new image
            {arch, *entry, move(*segments), move(*symbols), move(*sections)});

    }
    bap_notify_error("Unrecognized object format");
    return nullptr;
}

image* create_image_elf(error_or<object::Binary> &binary) {
    if (isa<ELF32LEObjectFile>(*binary))
        return create_image<ELF32LEObjectFile>(binary);

    if (isa<ELF32BEObjectFile>(*binary))
        return create_image<ELF32BEObjectFile>(binary);

    if (isa<ELF64LEObjectFile>(*binary))
        return create_image<ELF64LEObjectFile>(binary);

    if (isa<ELF64BEObjectFile>(*binary))
        return create_image<ELF64BEObjectFile>(binary);

    bap_notify_error("Unrecognized ELF format");
    return nullptr;
}

image* create_image_obj(error_or<object::Binary> &binary) {
    if (binary->isCOFF())
        return create_image<COFFObjectFile>(binary);
    if (binary->isELF())
        return create_image_elf(binary);
    if (binary->isMachO())
        return create_image<MachOObjectFile>(binary);
    bap_notify_error("Unrecognized object format");
    return nullptr;
}

image* create_image_arch(error_or<object::Binary> &binary) {
    bap_notify_error("Archive loading unimplemented");
    return nullptr;
}

image* create(error_or<object::Binary> &binary) {
    if (!binary)
        return nullptr;
    if (isa<Archive>(*binary))
        return create_image_arch(binary);
    if (isa<ObjectFile>(*binary))
        return create_image_obj(binary);
    bap_notify_error("Unrecognized binary format");
    return nullptr;
}

image* create(const char* data, std::size_t size) {
    error_or<object::Binary> bin = get_binary(data, size);
    if (!bin) {
        bap_notify_error(bin.message());
        return nullptr;
    }
    return create(bin);
}

} //namespace img

#endif //LLVM_BINARY_HPP
