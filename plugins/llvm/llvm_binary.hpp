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

extern "C" void llvm_binary_fail(const char*) LLVM_ATTRIBUTE_NORETURN ;

LLVM_ATTRIBUTE_NORETURN void llvm_binary_fail (const std::error_code ec) {
    llvm_binary_fail(ec.message().c_str());
}

namespace llvm { namespace object {

template <typename T>
content_iterator<T>& operator++(content_iterator<T>& a) {
    std::error_code ec;
    a.increment(ec);
    if(ec) llvm_binary_fail(ec);
    return a;
}

}} //namespace llvm::object

namespace utils {
using namespace llvm;
using namespace llvm::object;
	
std::vector<MachOObjectFile::LoadCommandInfo> load_commands(const MachOObjectFile* obj) {
    std::vector<MachOObjectFile::LoadCommandInfo> cmds;
    iterator_range<MachOObjectFile::load_command_iterator> info_list = obj->load_commands();
    for (MachOObjectFile::LoadCommandInfo info : info_list) {
        cmds.push_back(info);
    }
    return cmds;
}

} // namespace utils

namespace seg {
using namespace llvm;
using namespace llvm::object;

struct segment {
    template <typename T>
    segment(const Elf_Phdr_Impl<T>& hdr, int pos)
        : offset_(hdr.p_offset)
        , addr_(hdr.p_vaddr)
        , size_(hdr.p_filesz)
        , is_readable_(hdr.p_flags & ELF::PF_R)
        , is_writable_(hdr.p_flags & ELF::PF_W)
        , is_executable_(hdr.p_flags & ELF::PF_X) {
        std::ostringstream oss;
        oss << std::setfill('0') << std::setw(2) << pos ;
        name_ = oss.str();
    }

    segment(const MachO::segment_command &s) {
        init_macho_segment(s);
    }

    segment(const MachO::segment_command_64 &s) {
        init_macho_segment(s);
    }

    segment(const pe32_header &hdr, const coff_section &s)
        : name_(s.Name)
        , offset_(static_cast<uint32_t>(s.PointerToRawData))
        , addr_(static_cast<uint32_t>(s.VirtualAddress + hdr.ImageBase))
        , size_(static_cast<uint32_t>(s.SizeOfRawData))
        , is_readable_(static_cast<uint32_t>(s.Characteristics) &
                       COFF::IMAGE_SCN_MEM_READ)
        , is_writable_(static_cast<uint32_t>(s.Characteristics) &
                       COFF::IMAGE_SCN_MEM_WRITE)
        , is_executable_(static_cast<uint32_t>(s.Characteristics) &
                         COFF::IMAGE_SCN_MEM_EXECUTE)
        {}

    const std::string& name() const { return name_; }
    uint64_t offset() const { return offset_; }
    uint64_t addr() const { return addr_; }
    uint64_t size() const { return size_; }
    bool is_readable() const { return is_readable_; }
    bool is_writable() const { return is_writable_; }
    bool is_executable() const { return is_executable_; }

private:

    template <typename S>
    void init_macho_segment(const S &s) {
        name_ = s.segname;
        offset_ = s.fileoff;
        addr_ = s.vmaddr;
        size_ = s.filesize;
        is_readable_ = s.initprot & MachO::VM_PROT_READ;
        is_writable_ = s.initprot & MachO::VM_PROT_WRITE;
        is_executable_ = s.initprot & MachO::VM_PROT_EXECUTE;
    }

private:
    std::string name_;
    uint64_t offset_;
    uint64_t addr_;
    uint64_t size_;
    bool is_readable_;
    bool is_writable_;
    bool is_executable_;
};

template<typename T>
std::vector<segment> read(const ELFObjectFile<T>* obj) {
    auto begin = obj->getELFFile()->program_header_begin();
    auto end = obj->getELFFile()->program_header_end();
    std::vector<segment> segments;
    segments.reserve(std::distance(begin, end));
    for (int pos = 0; begin != end; ++begin, ++pos) {
        if (begin -> p_type == ELF::PT_LOAD) {
            segments.push_back(segment(*begin, pos));
        }
    }
    return segments;
}

std::vector<segment> read(const MachOObjectFile* obj) {
    typedef MachOObjectFile::LoadCommandInfo command_info;
    std::vector<command_info> cmds = utils::load_commands(obj);
    std::vector<segment> segments;
    for (std::size_t i = 0; i < cmds.size(); ++i) {
        command_info info = cmds.at(i);
        if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT_64)
            segments.push_back(segment(obj->getSegment64LoadCommand(info)));
        if (info.C.cmd == MachO::LoadCommandType::LC_SEGMENT)
            segments.push_back(segment(obj->getSegmentLoadCommand(info)));
    }
    return segments;
}

std::vector<segment> read(const COFFObjectFile* obj) {
    std::vector<segment> segments;
    const pe32_header *pe32;
    if (std::error_code err = obj->getPE32Header(pe32))
        llvm_binary_fail(err);
	for (auto it : obj->sections()) {
        const coff_section *s = obj->getCOFFSection(it);
        uint32_t c = static_cast<uint32_t>(s->Characteristics);
        if ( c & COFF::IMAGE_SCN_CNT_CODE ||
             c & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA ||
             c & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA )
            segments.push_back(segment(*pe32, *s));
    }
    return segments;
}

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

struct symbol {
    typedef SymbolRef::Type kind_type;

	symbol(const SymbolRef& sym, uint64_t size)
		: symbol(sym) {
		size_ = size;
	}
	
    explicit symbol(const SymbolRef& sym) {
		auto name = sym.getName();
		if (std::error_code ec = name.getError())
			llvm_binary_fail(ec);
        this->name_ = name->str();

		auto addr = sym.getAddress();
		if (std::error_code ec = addr.getError())
			llvm_binary_fail(ec);
		this->addr_ = addr.get();

		this->kind_ = sym.getType();
   	}
	
    const std::string& name() const { return name_; }
    kind_type kind() const { return kind_; }
    uint64_t addr() const { return addr_; }
    uint64_t size() const { return size_; }
private:
    std::string name_;
    kind_type kind_;
    uint64_t addr_;
    uint64_t size_;
};

std::vector<symbol> read(const ObjectFile* obj) {
	std::vector<symbol> symbols;
	auto symbol_sizes = computeSymbolSizes(*obj);
	for (auto s : symbol_sizes)
		symbols.push_back(symbol(s.first, s.second));
	return symbols;
}

} //namespace sym

namespace sec {
using namespace llvm;
using namespace llvm::object;

struct section {
    explicit section(const SectionRef& sec) {
        StringRef name;
        if(std::error_code err = sec.getName(name))
            llvm_binary_fail(err);
        this->name_ = name.str();
	  
		this->addr_ = sec.getAddress();
		this->size_ = sec.getSize();
    }
    const std::string& name() const { return name_; }
    uint64_t addr() const { return addr_; }
    uint64_t size() const { return size_; }

private:
    std::string name_;
    uint64_t addr_;
    uint64_t size_;
};

std::vector<section> read(const ObjectFile* obj) {
    auto size = std::distance(obj->sections().begin(), 
                             obj->sections().end());
    std::vector<section> sections;
    sections.reserve(size);

    std::transform(obj->sections().begin(),
                   obj->sections().end(),
                   std::back_inserter(sections),
                   [](const SectionRef& s) { return section(s); });
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

Triple::ArchType image_arch(const ObjectFile* obj) {
    return static_cast<Triple::ArchType>(obj->getArch());
}

template <typename ELFT>
uint64_t image_entry(const ELFObjectFile<ELFT>* obj) {
    return obj -> getELFFile() -> getHeader() -> e_entry;
}

uint64_t image_entry(const MachOObjectFile* obj) {
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

uint64_t image_entry(const COFFObjectFile* obj) {
    if (obj->getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (std::error_code ec = obj -> getPE32Header(hdr))
            llvm_binary_fail(ec);
        if (!hdr)
            llvm_binary_fail("PE header not found");
        return hdr->AddressOfEntryPoint;
    } else {
        // llvm version 3.4 doesn't support pe32plus_header,
        // but in version 3.5 it does. So, later one will be
        // able to write obj->getPE32PlusHeader(hdr) for 64-bit files.
        uint64_t cur_ptr = 0;
        const char * buf = (obj->getData()).data();
        const uint8_t *start = reinterpret_cast<const uint8_t *>(buf);
        uint8_t b0 = start[0];
        uint8_t b1 = start[1];
        if (b0 == 0x4d && b1 == 0x5a) { // Check if this is a PE/COFF file.
            // A pointer at offset 0x3C points to the PE header.
            cur_ptr += *reinterpret_cast<const uint16_t *>(start + 0x3c);
            // Check the PE magic bytes.
            if (std::memcmp(start + cur_ptr, "PE\0\0", 4) != 0)
                llvm_binary_fail("PE header not found");
            cur_ptr += 4; // Skip the PE magic bytes.
            cur_ptr += sizeof(coff_file_header);
            const pe32plus_header *hdr =
                reinterpret_cast<const pe32plus_header *>(start + cur_ptr);
            if (hdr->Magic == 0x20b)
                return hdr->AddressOfEntryPoint;
            else
                llvm_binary_fail("PEplus header not found");
        } else {
            llvm_binary_fail("PEplus header not found");
        }
    }
};

template <typename T>
struct objectfile_image : image {
    explicit objectfile_image(const T* obj, std::unique_ptr<object::Binary> binary)
        : arch_(image_arch(obj))
        , entry_(image_entry(obj))
        , segments_(seg::read(obj))
        , symbols_(sym::read(obj))
		, sections_(sec::read(obj))
		, binary_(std::move(binary))	  
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
	std::unique_ptr<object::Binary> binary_;
};

template <typename T>
image* create_image(const ObjectFile* obj, std::unique_ptr<object::Binary> binary) {
    if (const T* ptr = dyn_cast<T>(obj))
        return new objectfile_image<T>(ptr, std::move(binary));
    llvm_binary_fail("Unrecognized object format");
}

image* create_image_elf(const ObjectFile* obj, std::unique_ptr<object::Binary> binary) {
	if (const ELF32LEObjectFile *elf = dyn_cast<ELF32LEObjectFile>(obj))
        return create_image<ELF32LEObjectFile>(elf, std::move(binary));

    if (const ELF32BEObjectFile *elf = dyn_cast<ELF32BEObjectFile>(obj))
        return create_image<ELF32BEObjectFile>(elf, std::move(binary));

    if (const ELF64LEObjectFile *elf = dyn_cast<ELF64LEObjectFile>(obj))
        return create_image<ELF64LEObjectFile>(elf, std::move(binary));

    if (const ELF64BEObjectFile *elf = dyn_cast<ELF64BEObjectFile>(obj))
        return create_image<ELF64BEObjectFile>(elf, std::move(binary));
    llvm_binary_fail("Unrecognized ELF format");
}

image* create_image(const ObjectFile* obj, std::unique_ptr<object::Binary> binary) {
	if (obj->isCOFF())
		return create_image<COFFObjectFile>(obj, std::move(binary));
	if (obj->isELF())
		return create_image_elf(obj, std::move(binary));
	if (obj->isMachO())
		return create_image<MachOObjectFile>(obj, std::move(binary));
	llvm_binary_fail("Unrecognized object format");
}
	
image* create_image(const Archive* arch, std::unique_ptr<object::Binary> binary) {
    llvm_binary_fail("Archive loading unimplemented");
}

image* create_image(std::unique_ptr<object::Binary> binary) {
	const Binary* b = binary.get();
	if (const Archive *arch = dyn_cast<Archive>(b))
		return create_image(arch, std::move(binary));
	if (const ObjectFile *obj = dyn_cast<ObjectFile>(b))
		create_image(obj, std::move(binary));
	llvm_binary_fail("Unrecognized binary format");
}

image* create(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    ErrorOr<std::unique_ptr<object::Binary>> binary = createBinary(buf);
    if (std::error_code ec = binary.getError())
        llvm_binary_fail(ec);
	return create_image(std::move(*binary));
}

} //namespace img

#endif //LLVM_BINARY_HPP
