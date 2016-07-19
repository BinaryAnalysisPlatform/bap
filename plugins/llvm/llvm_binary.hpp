#ifndef LLVM_BINARY_HPP
#define LLVM_BINARY_HPP

#include <memory>
#include <numeric>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>

#include <llvm/ADT/iterator_range.h>
#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/Archive.h>
#include <llvm/Object/SymbolSize.h>
#include <llvm/Support/Compiler.h>

using std::move;
using std::error_code;


extern "C" void llvm_binary_fail(const char*) LLVM_ATTRIBUTE_NORETURN ;

LLVM_ATTRIBUTE_NORETURN void llvm_binary_fail (const error_code ec) {
    llvm_binary_fail(ec.message().c_str());
}

namespace llvm { namespace object {

template <typename T>
content_iterator<T>& operator++(content_iterator<T>& a) {
    error_code ec;
    a.increment(ec);
    if(ec) llvm_binary_fail(ec);
    return a;
}

}} //namespace llvm::object

//! Use anonymous namespace if enclosing functions are not used
//! outside of the module
namespace utils {
using namespace llvm;
using namespace llvm::object;

//! this function essentially translates an object of type
//! iterator_range<MachOObjectFile::load_command_iterator> to an object of type
//! std::vector<MachOObjectFile::LoadCommandInfo>
//! that is later only used for iteration in a very weird way.
//! to conclude we don't need this function at all
std::vector<MachOObjectFile::LoadCommandInfo> load_commands(const MachOObjectFile& obj) {
    std::vector<MachOObjectFile::LoadCommandInfo> cmds;
    iterator_range<MachOObjectFile::load_command_iterator> info_list = obj.load_commands();
    for (MachOObjectFile::LoadCommandInfo info : info_list) {
        cmds.push_back(info);
    }
    return cmds;
}

} // namespace utils

namespace {
//! using directive inside of unnamed namespace actually polutes the whole
//! (global) namespace that follows after the using.
//! also it is not really needed.
//! finally, it is better not to use using directives (not using declarations)
//! at all in general, and in header files in general, as their precise meaning
//! depends on where and how your header would be included.
using namespace llvm;

template<typename Derived, typename Base>
std::unique_ptr<Derived> dynamic_unique_ptr_cast(std::unique_ptr<Base>&& ptr) {
    if (Derived* d = dyn_cast<Derived>(ptr.get())) {
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


template<typename T>
std::vector<segment> read(const ELFObjectFile<T>& obj) {
    auto begin = obj.getELFFile()->program_header_begin();
    auto end = obj.getELFFile()->program_header_end();
    std::vector<segment> segments;
    segments.reserve(std::distance(begin, end));
    //! do not comment out code (and commit it)
    //auto current = begin;
    for (int pos = 0; begin != end; ++begin, ++pos) {
        if (begin -> p_type == ELF::PT_LOAD) {
	    std::ostringstream oss;
	    oss << std::setfill('0') << std::setw(2) << pos;
	    segments.push_back(segment{oss.str(),
			begin->p_offset,
			begin->p_vaddr,
			begin->p_filesz,
			static_cast<bool>(begin->p_flags & ELF::PF_R),
			static_cast<bool>(begin->p_flags & ELF::PF_W),
			static_cast<bool>(begin->p_flags & ELF::PF_X)});
	}
    }
    return segments;
}

template <typename S>
segment make_segment(const S &s) {
    return segment{s.segname, s.fileoff, s.vmaddr, s.filesize,
	    static_cast<bool>(s.initprot & MachO::VM_PROT_READ),
	    static_cast<bool>(s.initprot & MachO::VM_PROT_WRITE),
	    static_cast<bool>(s.initprot & MachO::VM_PROT_EXECUTE)};
}

std::vector<segment> read(const MachOObjectFile& obj) {
    typedef MachOObjectFile::LoadCommandInfo command_info;
    std::vector<command_info> cmds = utils::load_commands(obj);
    std::vector<segment> segments;
    for (std::size_t i = 0; i < cmds.size(); ++i) {
        command_info info = cmds.at(i);
        if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
            segments.push_back(make_segment(obj.getSegment64LoadCommand(info)));
        if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
            segments.push_back(make_segment(obj.getSegmentLoadCommand(info)));
    }
    return segments;
}

template <typename T>
segment make_segment(T image_base, const coff_section &s) {
    return segment{s.Name, 
	    static_cast<T>(s.PointerToRawData),
	    static_cast<T>(s.VirtualAddress + image_base),
	    static_cast<T>(s.SizeOfRawData),
	    static_cast<bool>((s.Characteristics) &
			      COFF::IMAGE_SCN_MEM_READ),
	    static_cast<bool>((s.Characteristics) &
			      COFF::IMAGE_SCN_MEM_WRITE),
	    static_cast<bool>((s.Characteristics) &
			      COFF::IMAGE_SCN_MEM_EXECUTE)};
}

template <typename T>
std::vector<segment> readPE(const COFFObjectFile& obj, const T image_base) {
    std::vector<segment> segments;
    for (auto it = obj.begin_sections();
         it != obj.end_sections(); ++it) {
        const coff_section *s = obj.getCOFFSection(it);
        T c = static_cast<T>(s->Characteristics);
        if ( c & COFF::IMAGE_SCN_CNT_CODE ||
             c & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA ||
             c & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA )
            segments.push_back(make_segment<T>(image_base, *s));
    }
    return segments;
}

std::vector<segment> read(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
	const pe32_header *pe32;
	if (error_code err = obj.getPE32Header(pe32))
	    llvm_binary_fail(err);
	return readPE<uint32_t>(obj, pe32->ImageBase);
    } else {
	const pe32plus_header *pe32plus = utils::getPE32PlusHeader(obj);
        if (!pe32plus)
            llvm_binary_fail("Failed to extract PE32+ header");
	return readPE<uint64_t>(obj, pe32plus->ImageBase);
    }
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
    auto name = sym.getName();
    if (error_code ec = name.getError())
        llvm_binary_fail(ec);

    auto addr = sym.getAddress();
    if (error_code ec = addr.getError())
        llvm_binary_fail(ec);

    return symbol{name->str(), sym.getType(), addr.get(), size};
}
std::vector<symbol> read(const ObjectFile& obj) {
    std::vector<symbol> symbols;
    auto symbol_sizes = computeSymbolSizes(obj);
    for (auto s : symbol_sizes)
	symbols.push_back(make_symbol(s.first, s.second));
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
    StringRef name;
    if (error_code ec = sec.getName(name))
	llvm_binary_fail(ec);

    return section{name.str(), sec.getAddress(), sec.getSize()};
}

std::vector<section> read(const ObjectFile &obj) {
    auto size = std::distance(obj.sections().begin(),
                             obj.sections().end());
    std::vector<section> sections;
    sections.reserve(size);

    std::transform(obj.sections().begin(),
                   obj.sections().end(),
                   std::back_inserter(sections),
                   [](const SectionRef& s) { return make_section(s); });
    return sections;
}

} //namespace sec

namespace img {
using namespace llvm;
using namespace llvm::object;

struct image {
    virtual uint64_t entry() const = 0;
    virtual Triple::ArchType arch() const = 0;
    virtual const std::vector<seg::segment>& segments() const = 0;
    virtual const std::vector<sym::symbol>& symbols() const = 0;
    virtual const std::vector<sec::section>& sections() const = 0;
    virtual ~image() {}
};

Triple::ArchType image_arch(const ObjectFile& obj) {
    return static_cast<Triple::ArchType>(obj.getArch());
}

template <typename ELFT>
uint64_t image_entry(const ELFObjectFile<ELFT>& obj) {
    return obj.getELFFile()->getHeader()->e_entry;
}

uint64_t image_entry(const MachOObjectFile& obj) {
    typedef MachOObjectFile::LoadCommandInfo command_info;
    typedef std::vector<command_info> commands;
    typedef std::vector<command_info>::const_iterator const_iterator;
    commands cmds = utils::load_commands(obj);
    const_iterator it =
        std::find_if(cmds.begin(), cmds.end(),
                     [](const command_info &info){
                         return
                         info.C.cmd == MachO::LoadCommandType::LC_MAIN;});
    if (it != cmds.end()) {
        const MachO::entry_point_command *entry_cmd =
            reinterpret_cast<const MachO::entry_point_command*>(it->Ptr);
        return entry_cmd->entryoff;
    } else {
        llvm_binary_fail("LC_MAIN not found, binary version < 10.8");
    }
}

uint64_t image_entry(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (error_code ec = obj.getPE32Header(hdr))
	    llvm_binary_fail(ec);
        if (!hdr)
            llvm_binary_fail("PE header not found");
        return hdr->AddressOfEntryPoint;
    } else {
	const pe32plus_header* hdr = 0;
	if (error_code ec = obj.getPE32PlusHeader(hdr))
	    llvm_binary_fail(ec);
	if (!hdr)
	    llvm_binary_fail("PEplus header no found");
	return hdr->AddressOfEntryPoint;
    }
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
    Triple::ArchType arch() const { return arch_; }
    uint64_t entry() const { return entry_; }
    const std::vector<seg::segment>& segments() const { return segments_; }
    const std::vector<sym::symbol>& symbols() const { return symbols_; }
    const std::vector<sec::section>& sections() const { return sections_; }
protected:
    Triple::ArchType arch_;
    uint64_t entry_;
    std::vector<seg::segment> segments_;
    std::vector<sym::symbol> symbols_;
    std::vector<sec::section> sections_;
public:
    std::unique_ptr<T> binary_;
};

template <typename T>
image* create_image(std::unique_ptr<object::Binary> binary) {
    if (std::unique_ptr<T> ptr =
	dynamic_unique_ptr_cast<T, object::Binary>(move(binary))) {
	return new objectfile_image<T>(move(ptr));
    }
    llvm_binary_fail("Unrecognized object format");
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
    llvm_binary_fail("Unrecognized ELF format");
}

image* create_image_obj(std::unique_ptr<object::Binary> binary) {
    if (binary->isCOFF())
	return create_image<COFFObjectFile>(move(binary));
    if (binary->isELF())
	return create_image_elf(move(binary));
    if (binary->isMachO())
	return create_image<MachOObjectFile>(move(binary));
    llvm_binary_fail("Unrecognized object format");
}

image* create_image_arch(std::unique_ptr<object::Binary> binary) {
    llvm_binary_fail("Archive loading unimplemented");
}

image* create(std::unique_ptr<object::Binary> binary) {
    if (isa<Archive>(*binary))
	return create_image_arch(move(binary));
    if (isa<ObjectFile>(*binary))
	return create_image_obj(move(binary));
    llvm_binary_fail("Unrecognized binary format");
}

image* create(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (error_code ec = binary.getError())
	llvm_binary_fail(ec);
    return create(move(*binary));
}

} //namespace img

#endif //LLVM_BINARY_HPP
