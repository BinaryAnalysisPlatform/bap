#ifndef BAP_LLVM_BINARY_STUBS_HPP
#define BAP_LLVM_BINARY_STUBS_HPP

#include <memory>
#include <numeric>
#include <vector>
#include <algorithm>

#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/Archive.h>

void llvm_binary_fail [[ noreturn ]](const char*);

void llvm_binary_fail [[ noreturn ]](const llvm::error_code& ec) {
    llvm_binary_fail(ec.message().c_str());
}

namespace llvm { namespace object {

template <typename T>
content_iterator<T>& operator++(content_iterator<T>& a) {
    error_code ec;
    a.increment(ec);
    if(ec) llvm_binary_fail(ec);
}

}} //namespace llvm::object

namespace utils {
using namespace llvm;
using namespace llvm::object;

template <typename T>
int distance(content_iterator<T> begin, content_iterator<T> end) {
    error_code ec;
    int n = 0;
    while (begin != end) {
        ++n;
        begin.increment(ec);
        if (ec)
            llvm_binary_fail(ec);
    }
    return n;
}
    
std::vector<MachOObjectFile::LoadCommandInfo> load_commands(const MachOObjectFile* obj) {
    std::size_t cmd_count = 0;
    if (obj->is64Bit()) 
        cmd_count = obj->getHeader64().ncmds;
    else
        cmd_count = obj->getHeader().ncmds;
    std::vector<MachOObjectFile::LoadCommandInfo> cmds;
    MachOObjectFile::LoadCommandInfo info = obj->getFirstLoadCommandInfo();
    for (std::size_t i = 0; i < cmd_count; ++i) {
        cmds.push_back(info);
        info = obj->getNextLoadCommandInfo(info);
    }
    return cmds;
}

} // namespace utils

namespace seg {
using namespace llvm;
using namespace llvm::object;

struct segment {
    template <typename T>
    segment(const Elf_Phdr_Impl<T>& hdr)
        : name_("not applicable")
        , offset_(hdr.p_offset)
        , addr_(hdr.p_vaddr)
        , size_(hdr.p_filesz)
        , is_readable_(hdr.p_flags & ELF::PF_R)
        , is_writable_(hdr.p_flags & ELF::PF_W)
        , is_executable_(hdr.p_flags & ELF::PF_X) { }

    segment(const MachO::segment_command &s) {
        init_macho_segment(s);
    }

    segment(const MachO::segment_command_64 &s) {
        init_macho_segment(s);
    }    

    segment(const coff_section &s) 
        : name_(s.Name)
        , offset_(static_cast<uint32_t>(s.PointerToRawData))
        , addr_(static_cast<uint32_t>(s.VirtualAddress))
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
    auto begin = obj->getELFFile()->begin_program_headers();
    auto end = obj->getELFFile()->end_program_headers();
    std::vector<segment> segments;
    segments.reserve(std::distance(begin, end));
    std::copy_if(begin,
                 end,
                 std::back_inserter(segments),
                 [](const Elf_Phdr_Impl<T>& hdr){ return hdr.p_type == ELF::PT_LOAD;});
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
    for (auto it = obj->begin_sections();
         it != obj->end_sections(); ++it) {
        const coff_section *s = obj->getCOFFSection(it);
        if (static_cast<uint32_t>(s->Characteristics) & COFF::IMAGE_SCN_CNT_CODE) 
            segments.push_back(segment(*s));
    }
    return segments;
}

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

struct symbol {
    typedef SymbolRef::Type kind_type;
    explicit symbol(const SymbolRef& sym) {
        StringRef name;
        if(error_code err = sym.getName(name))
            llvm_binary_fail(err);
        this->name_ = name.str();
        
        if (error_code err = sym.getType(this->kind_))
            llvm_binary_fail(err);
        
        if (error_code err = sym.getAddress(this->addr_))
            llvm_binary_fail(err);
        
        if (error_code err = sym.getSize(this->size_))
            llvm_binary_fail(err);
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

template <typename OutputIterator>
OutputIterator read(symbol_iterator begin,
                    symbol_iterator end,
                    OutputIterator out) {
    return std::transform(begin, end, out,
                          [](const SymbolRef& s) { return symbol(s); });
}

std::vector<symbol> read(const ObjectFile* obj) {
    int size = utils::distance(obj->begin_symbols(),
                               obj->end_symbols());
    std::vector<symbol> symbols;
    symbols.reserve(size);
    
    read(obj->begin_symbols(),
         obj->end_symbols(),
         std::back_inserter(symbols));
    return symbols;
}

template <typename ELFT>
std::vector<symbol> read(const ELFObjectFile<ELFT>* obj) {
    int size1 = utils::distance(obj->begin_symbols(),
                                obj->end_symbols());
    int size2 = utils::distance(obj->begin_dynamic_symbols(),
                                obj->end_dynamic_symbols());

    std::vector<symbol> symbols;
    symbols.reserve(size1+size2);
    
    auto it = read(obj->begin_symbols(),
                   obj->end_symbols(),
                   std::back_inserter(symbols));

    read(obj->begin_dynamic_symbols(),
         obj->end_dynamic_symbols(),
         it);
    return symbols;
}

    
} //namespace sym

namespace sec {
using namespace llvm;
using namespace llvm::object;

struct section {
    explicit section(const SectionRef& sec) {
        StringRef name;
        if(error_code err = sec.getName(name))
            llvm_binary_fail(err);
        this->name_ = name.str();
        if (error_code err = sec.getAddress(this->addr_))
            llvm_binary_fail(err);
        
        if (error_code err = sec.getSize(this->size_))
            llvm_binary_fail(err);
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
    int size = utils::distance(obj->begin_sections(),
                               obj->end_sections());
    std::vector<section> sections;
    sections.reserve(size);
    std::transform(obj->begin_sections(),
                   obj->end_sections(),
                   std::back_inserter(sections),
                   [](const SectionRef& s) { return section(s); });
    return sections;
}

} //namespace sec

namespace ext {
using namespace llvm;
using namespace llvm::object;

//Extractor. Extracts require values from binary
struct extractor_base {
    virtual uint64_t entry() const = 0;
    virtual Triple::ArchType arch() const = 0;
    virtual std::vector<seg::segment> segments() const = 0;
    virtual std::vector<sym::symbol> symbols() const = 0;
    virtual std::vector<sec::section> sections() const = 0;
    virtual ~extractor_base() {}
};


template <typename T>
struct extractor_objfile : extractor_base {
    explicit extractor_objfile(const T* obj) : obj_(obj) {}
    Triple::ArchType arch() const {
        return static_cast<Triple::ArchType>(obj_->getArch());
    }

    std::vector<seg::segment> segments() const {
        return seg::read(obj_);
    }

    std::vector<sym::symbol> symbols() const {
        return sym::read(obj_);
    }

    std::vector<sec::section> sections() const {
        return sec::read(obj_);
    }
protected:
    const T *obj_;
};

template <typename T>
struct extractor;

//ELF extractor
template <typename ELFT>
struct extractor< ELFObjectFile<ELFT> > : extractor_objfile< ELFObjectFile<ELFT> > {
    explicit extractor(const ELFObjectFile<ELFT> *obj)
        : extractor_objfile< ELFObjectFile<ELFT> >(obj) {}

    uint64_t entry() const {
        return this -> obj_ -> getELFFile() -> getHeader() -> e_entry;
    }
};

//MachO extractor
template <>
struct extractor<MachOObjectFile> : extractor_objfile<MachOObjectFile> {
    explicit extractor(const MachOObjectFile *obj)
        : extractor_objfile<MachOObjectFile>(obj) {}

    uint64_t entry() const {
        typedef MachOObjectFile::LoadCommandInfo command_info;
        typedef std::vector<command_info> commands;
        typedef std::vector<command_info>::const_iterator const_iterator;
        commands cmds = utils::load_commands(obj_);
        const_iterator it =
            std::find_if(cmds.begin(),
                         cmds.end(),
                         [](const command_info &info)
                         {return info.C.cmd == MachO::LoadCommandType::LC_MAIN;});
        if (it != cmds.end()) {
            const MachO::entry_point_command *entry_cmd =
                reinterpret_cast<const MachO::entry_point_command*>(it->Ptr);
            return entry_cmd->entryoff;
        } else {
            llvm_binary_fail("LC_MAIN not found, binary version < 10.8");
        }
    }
};

//COFF extractor
template <>
struct extractor<COFFObjectFile> : extractor_objfile<COFFObjectFile> {
    explicit extractor(const COFFObjectFile *obj)
        : extractor_objfile<COFFObjectFile>(obj) {}
    uint64_t entry() const {
        const pe32_header* hdr = 0;
        if (error_code ec = this -> obj_ -> getPE32Header(hdr))
            llvm_binary_fail(ec);
        if (!hdr)
            llvm_binary_fail("PE header not found");
        return hdr->AddressOfEntryPoint;
    };
};

template <typename T>
std::shared_ptr<extractor_base> create_extractor(const ObjectFile* obj) {
    if (const T* ptr = dyn_cast<T>(obj))
        return std::make_shared< extractor<T> >(ptr);
    llvm_binary_fail("Unrecognized object format");
}

std::shared_ptr<extractor_base> create_extractor_elf(const ObjectFile* obj) {
    if (const ELF32LEObjectFile *elf = dyn_cast<ELF32LEObjectFile>(obj))
        return create_extractor<ELF32LEObjectFile>(elf);

    if (const ELF32BEObjectFile *elf = dyn_cast<ELF32BEObjectFile>(obj))
        return create_extractor<ELF32BEObjectFile>(elf);

    if (const ELF64LEObjectFile *elf = dyn_cast<ELF64LEObjectFile>(obj))
        return create_extractor<ELF64LEObjectFile>(elf);

    if (const ELF64BEObjectFile *elf = dyn_cast<ELF64BEObjectFile>(obj))
        return create_extractor<ELF64BEObjectFile>(elf);
    llvm_binary_fail("Unrecognized ELF format");
}

std::shared_ptr<extractor_base> create_extractor(const ObjectFile* obj) {
    if (obj->isCOFF())
        return create_extractor<COFFObjectFile>(obj);
    if (obj->isELF())
        return create_extractor_elf(obj);
    if (obj->isMachO())
        return create_extractor<MachOObjectFile>(obj);
    llvm_binary_fail("Unrecognized object format");            
}

std::shared_ptr<extractor_base> create_extractor(const Archive* arch) {
    llvm_binary_fail("Archive loading unimplemented");
}

std::shared_ptr<extractor_base> create_extractor(const Binary* binary) {
    if (const Archive *arch = dyn_cast<Archive>(binary))
        return create_extractor(arch);
    if (const ObjectFile *obj = dyn_cast<ObjectFile>(binary))
        return create_extractor(obj);
    llvm_binary_fail("Unrecognized binary format");
}
} //namespace ext


namespace binary {
using namespace llvm;
using namespace llvm::object;

Binary* llvm_binary_create(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBuffer* buff(MemoryBuffer::getMemBufferCopy(data_ref, "binary"));
    OwningPtr<object::Binary> binary;
    if (error_code ec = createBinary(buff, binary))
        llvm_binary_fail(ec);
    return binary.take();
}

Triple::ArchType llvm_binary_arch(Binary* binary) {
    return ext::create_extractor(binary)->arch();
}

uint64_t llvm_binary_entry(Binary* binary) {
   return ext::create_extractor(binary)->entry();
}

std::vector<seg::segment> llvm_binary_segments(Binary* binary) {
    return ext::create_extractor(binary)->segments();
}

std::vector<sym::symbol> llvm_binary_symbols(Binary* binary) {
    return ext::create_extractor(binary)->symbols();
}

std::vector<sec::section> llvm_binary_sections(Binary* binary) {
    return ext::create_extractor(binary)->sections();
}

} //binary

#endif //BAP_LLVM_BINARY_STUBS_HPP
