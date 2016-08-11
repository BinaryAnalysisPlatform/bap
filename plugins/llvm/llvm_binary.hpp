#ifndef LLVM_BINARY_HPP
#define LLVM_BINARY_HPP

#include <memory>
#include <numeric>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>

#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/Archive.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Config/llvm-config.h>

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
#include "llvm_binary_38.hpp"
#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4
#include "llvm_binary_34.hpp"
#else
#error LLVM version not supported.
#endif

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
using std::distance;
#endif

using std::move;

namespace {

template<typename Derived, typename Base>
std::unique_ptr<Derived> dynamic_unique_ptr_cast(std::unique_ptr<Base>&& ptr) {
    if (Derived* d = llvm::dyn_cast<Derived>(ptr.get())) {
	ptr.release();
	return std::unique_ptr<Derived>(d);
    }
    return std::unique_ptr<Derived>(nullptr);
}

} // namespace

namespace seg {
using namespace llvm;
using namespace llvm::object;

struct segment {
    std::string name;
    uint64_t offset;
    uint64_t addr;
    uint64_t size;
    bool is_readable;
    bool is_writable;
    bool is_executable;
};

template <typename S>
segment make_segment(const S &s) {
    return segment{s.segname, s.fileoff, s.vmaddr, s.filesize,
	    static_cast<bool>(s.initprot & MachO::VM_PROT_READ),
	    static_cast<bool>(s.initprot & MachO::VM_PROT_WRITE),
	    static_cast<bool>(s.initprot & MachO::VM_PROT_EXECUTE)};
}

segment make_segment(const coff_section &s, uint64_t image_base) {
    return segment{s.Name,
	    static_cast<uint64_t>(s.PointerToRawData),
	    static_cast<uint64_t>(s.VirtualAddress + image_base),
	    static_cast<uint64_t>(s.SizeOfRawData),
	    static_cast<bool>((s.Characteristics) &
			      COFF::IMAGE_SCN_MEM_READ),
	    static_cast<bool>((s.Characteristics) &
			      COFF::IMAGE_SCN_MEM_WRITE),
	    static_cast<bool>((s.Characteristics) &
			      COFF::IMAGE_SCN_MEM_EXECUTE)};
}

template<typename T>
std::vector<segment> read(const ELFObjectFile<T>& obj) {
    auto begin = elf_header_begin(obj.getELFFile());
    auto end = elf_header_end(obj.getELFFile());
    std::vector<segment> segments;
    segments.reserve(distance(begin, end));
    auto it = begin;
    for (int pos = 0; it != end; ++it, ++pos) {
        if (it -> p_type == ELF::PT_LOAD) {
	    std::ostringstream oss;
	    oss << std::setfill('0') << std::setw(2) << pos;
	    segments.push_back(segment{oss.str(),
			it->p_offset,
			it->p_vaddr,
			it->p_filesz,
			static_cast<bool>(it->p_flags & ELF::PF_R),
			static_cast<bool>(it->p_flags & ELF::PF_W),
			static_cast<bool>(it->p_flags & ELF::PF_X)});
	}
    }
    return segments;
}

std::vector<segment> read(const MachOObjectFile& obj) {
    auto cmds = load_commands(obj);
    std::vector<segment> segments;
    for (auto it : cmds) {
        if (it.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
            segments.push_back(make_segment(obj.getSegment64LoadCommand(it)));
        if (it.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
            segments.push_back(make_segment(obj.getSegmentLoadCommand(it)));
    }
    return segments;
}

std::vector<segment> read(const COFFObjectFile& obj) {
    std::vector<segment> segments;
    uint64_t image_base = getImageBase(obj);
    for (auto it : sections(obj)) {
        const coff_section *s = obj.getCOFFSection(it);
        uint64_t c = static_cast<uint64_t>(s->Characteristics);
        if ( c & COFF::IMAGE_SCN_CNT_CODE ||
             c & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA ||
             c & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA )
            segments.push_back(make_segment(*s, image_base));
    }
    return segments;
}

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

typedef SymbolRef::Type kind_type;

struct symbol {
    std::string name;
    kind_type kind;
    uint64_t addr;
    uint64_t size;
};

symbol make_symbol(const SymbolRef& sym, uint64_t size) {
    auto name = value_or_default(sym.getName()).str();
    auto addr = value_or_default(sym.getAddress());
    return symbol{name, sym.getType(), addr, size}; 
}

std::vector<symbol> read(const ObjectFile &obj) {
    std::vector<symbol> symbols;
    auto symbol_sizes = getSymbolSizes(obj);
    for (auto it : symbol_sizes)
        symbols.push_back(make_symbol(it.first, it.second));
    return symbols;
}

std::vector<symbol> read(const COFFObjectFile &obj) {
    std::vector<symbol> symbols;
    auto symbol_sizes = getSymbolSizes(obj);
    for (auto it : symbol_sizes)
        symbols.push_back(make_symbol(it.first, it.second));
    return symbols;
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

section make_section(const SectionRef &sec) {
    auto name = getName(sec);
    auto addr = getAddr(sec);
    auto size = getSize(sec);

    return section{name, addr, size};
}

section make_section(const coff_section &s, const uint64_t image_base) {
    auto name = getName(s);
    auto addr = getAddr(s);
    auto size = getSize(s);

    return section{name, addr + image_base, size};
}

std::vector<section> read(const ObjectFile &obj) {
    auto size = distance(begin_sections(obj), end_sections(obj));
    std::vector<section> sections;
    sections.reserve(size);
    std::transform(begin_sections(obj),
                   end_sections(obj),
                   std::back_inserter(sections),
                   [](const SectionRef& s) { return make_section(s); });
    return sections;
}

std::vector<section> read(const COFFObjectFile &obj) {
    std::vector<section> sections;
    auto size = distance(begin_sections(obj), end_sections(obj));
    uint64_t image_base = getImageBase(obj);
// TODO
#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
    std::cout << "In 3.8 section read of COFFObjectFile...\n";
    sections.reserve(size);
    std::transform(begin_sections(obj),
                   end_sections(obj),
                   std::back_inserter(sections),
                   [&obj, image_base](const SectionRef& s) { return make_section(*obj.getCOFFSection(s), image_base); });
#else
    for (auto it = begin_sections(obj);
         it != end_sections(obj); ++it) {
        const coff_section *s = obj.getCOFFSection(it);
        sections.push_back(make_section(*s, image_base));
    }
#endif
    return sections;
}

} //namespace sec

namespace img {
using namespace llvm;
using namespace llvm::object;

struct image {
    virtual uint64_t entry() const = 0;
    virtual const std::string &arch() const = 0;
    virtual const std::vector<seg::segment>& segments() const = 0;
    virtual const std::vector<sym::symbol>& symbols() const = 0;
    virtual const std::vector<sec::section>& sections() const = 0;
    virtual ~image() {}
};

std::string image_arch(const ObjectFile& obj) {
    return Triple::getArchTypeName(static_cast<Triple::ArchType>(obj.getArch()));
}

template <typename ELFT>
uint64_t image_entry(const ELFObjectFile<ELFT>& obj) {
    return obj.getELFFile()->getHeader()->e_entry;
}

uint64_t image_entry(const MachOObjectFile &obj) {
    return image_entry_macho(obj);
}

uint64_t image_entry(const COFFObjectFile &obj) {
    return image_entry_coff(obj);
}

template <typename T>
struct objectfile_image : image {
    explicit objectfile_image(std::unique_ptr<T> ptr)
	: arch_(image_arch(*ptr))
	, entry_(image_entry(*ptr))
	, segments_(seg::read(*ptr))
	, symbols_(sym::read(*ptr))
	, sections_(sec::read(*ptr))
	, binary_(move(ptr))
    {}
    const std::string &arch() const { return arch_; }
    uint64_t entry() const { return entry_; }
    const std::vector<seg::segment>& segments() const { return segments_; }
    const std::vector<sym::symbol>& symbols() const { return symbols_; }
    const std::vector<sec::section>& sections() const { return sections_; }
protected:
    const std::string arch_;
    uint64_t entry_;
    std::vector<seg::segment> segments_;
    std::vector<sym::symbol> symbols_;
    std::vector<sec::section> sections_;
    std::unique_ptr<T> binary_;
};

template <typename T>
image* create_image(std::unique_ptr<object::Binary> binary) {
    if (std::unique_ptr<T> ptr =
	dynamic_unique_ptr_cast<T, object::Binary>(move(binary))) {
	return new objectfile_image<T>(move(ptr));
    }
    std::cerr << "Unrecognized object format\n";
    return NULL;
}

image* create_image_elf(std::unique_ptr<object::Binary> binary) {
    if (isa<ELF32LEObjectFile>(*binary))
	return create_image<ELF32LEObjectFile>(move(binary));

    if (isa<ELF32BEObjectFile>(*binary))
        return create_image<ELF32BEObjectFile>(move(binary));

    if (isa<ELF64LEObjectFile>(*binary))
        return create_image<ELF64LEObjectFile>(move(binary));

    if (isa<ELF64BEObjectFile>(*binary))
        return create_image<ELF64BEObjectFile>(move(binary));
    std::cerr << "Unrecognized ELF format\n";
    return NULL;
}

image* create_image_obj(std::unique_ptr<object::Binary> binary) {
    if (binary->isCOFF())
	return create_image<COFFObjectFile>(move(binary));
    if (binary->isELF())
	return create_image_elf(move(binary));
    if (binary->isMachO())
	return create_image<MachOObjectFile>(move(binary));
    std::cerr << "Unrecognized object format\n";
    return NULL;
}

image* create_image_arch(std::unique_ptr<object::Binary> binary) {
    std::cerr << "Archive loading unimplemented\n";
    return NULL;
}

image* create(std::unique_ptr<object::Binary> binary) {
    if (isa<Archive>(*binary))
	return create_image_arch(move(binary));
    if (isa<ObjectFile>(*binary))
	return create_image_obj(move(binary));
    std::cerr << "Unrecognized binary format\n";
    return NULL;
}

image* create(const char* data, std::size_t size) {
    return create(move(get_binary(data, size)));
}

} //namespace img

#endif //LLVM_BINARY_HPP
