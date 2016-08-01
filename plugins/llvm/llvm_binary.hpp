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
#include <llvm/ADT/iterator_range.h>
#include <llvm/Object/SymbolSize.h>
#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4
#include <llvm/ADT/OwningPtr.h>
#else
#error LLVM version not supported.
#endif

using std::move;

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
using std::error_code;
using std::distance;
#else
using llvm::error_code;
#endif

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

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4
//! I don't think that there is any reason to have non-anonymous
//! namespace here.
namespace utils {
using namespace llvm;
using namespace llvm::object;

const pe32plus_header* getPE32PlusHeader(const COFFObjectFile& obj) {
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
            llvm_binary_fail("PE Plus header not found");
        cur_ptr += 4; // skip the PE magic bytes.
        cur_ptr += sizeof(coff_file_header);
        return reinterpret_cast<const pe32plus_header *>(start + cur_ptr);
    }
    return NULL;
}

} //namespace utils
#endif

namespace {

template<typename Derived, typename Base>
std::unique_ptr<Derived> dynamic_unique_ptr_cast(std::unique_ptr<Base>&& ptr) {
    if (Derived* d = llvm::dyn_cast<Derived>(ptr.get())) {
	ptr.release();
	return std::unique_ptr<Derived>(d);
    }
    return std::unique_ptr<Derived>(nullptr);
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
uint64_t getPE32PlusEntry(const llvm::object::COFFObjectFile &obj) {
    const llvm::object::pe32plus_header *hdr = 0;
    if (error_code ec = obj.getPE32PlusHeader(hdr))
        llvm_binary_fail(ec);
    return hdr->AddressOfEntryPoint;
}
#else
typedef llvm::object::MachOObjectFile macho;
typedef macho::LoadCommandInfo command_info;

template <typename T>
int distance(llvm::object::content_iterator<T> begin, llvm::object::content_iterator<T> end) {
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

std::vector<command_info> load_commands(const macho& obj) {
    std::size_t cmd_count = 0;
    if (obj.is64Bit())
        cmd_count = obj.getHeader64().ncmds;
    else
        cmd_count = obj.getHeader().ncmds;
    std::vector<command_info> cmds;
    command_info info = obj.getFirstLoadCommandInfo();
    for (std::size_t i = 0; i < cmd_count; ++i) {
        cmds.push_back(info);
        info = obj.getNextLoadCommandInfo(info);
    }
    return cmds;
}

uint64_t getPE32PlusEntry(const llvm::object::COFFObjectFile &obj) {
    const llvm::object::pe32plus_header *hdr = utils::getPE32PlusHeader(obj);;
    if (!hdr)
        llvm_binary_fail("Failed to extract PE32+ header");
    return hdr->AddressOfEntryPoint;
}
#endif

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

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
using namespace llvm;
using namespace llvm::object;

template <typename ELFT>
const typename ELFFile<ELFT>::Elf_Phdr* elf_header_begin(const ELFFile<ELFT> *elf) {
    return elf->program_header_begin();
}

template <typename ELFT>
const typename ELFFile<ELFT>::Elf_Phdr* elf_header_end(const ELFFile<ELFT> *elf) {
    return elf->program_header_end();
}

iterator_range<MachOObjectFile::load_command_iterator> load_commands(const MachOObjectFile &obj) {
    return obj.load_commands();
}

ObjectFile::section_iterator_range sections(const COFFObjectFile &obj) {
    return obj.sections();
}
#else
using namespace llvm;
using namespace llvm::object;

template <typename ELFT>
const typename ELFFile<ELFT>::Elf_Phdr_Iter elf_header_begin(const ELFFile<ELFT> *elf) {
    return elf->begin_program_headers();
}

template <typename ELFT>
const typename ELFFile<ELFT>::Elf_Phdr_Iter elf_header_end(const ELFFile<ELFT> *elf) {
    return elf->end_program_headers();
}

std::vector<section_iterator> sections(const COFFObjectFile &obj) {
    std::vector<section_iterator> sections;
    for (auto it = obj.begin_sections(); it != obj.end_sections(); ++it) {
        sections.push_back(it);
    }
    return sections;
}
#endif

template<typename T>
std::vector<segment> read(const ELFObjectFile<T>& obj) {
    auto begin = elf_header_begin(obj.getELFFile());
    auto end = elf_header_end(obj.getELFFile());
    std::vector<segment> segments;
    segments.reserve(std::distance(begin, end));
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

template <typename S>
segment make_segment(const S &s) {
    return segment{s.segname, s.fileoff, s.vmaddr, s.filesize,
	    static_cast<bool>(s.initprot & MachO::VM_PROT_READ),
	    static_cast<bool>(s.initprot & MachO::VM_PROT_WRITE),
	    static_cast<bool>(s.initprot & MachO::VM_PROT_EXECUTE)};
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
    for (auto it : sections(obj)) {
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
	if (error_code ec = obj.getPE32Header(pe32))
	    llvm_binary_fail(ec);
	return readPE<uint32_t>(obj, pe32->ImageBase);
    } else {
        const pe32plus_header *pe32 = utils::getPE32PlusHeader(obj);
        return readPE<uint64_t>(obj, pe32->ImageBase);
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

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
template <typename T>
T value_or_default(const ErrorOr<T> &e, T def=T()) {
    if (e) return e.get();
    return def;
}

template <typename T>
T value_or_fail(const ErrorOr<T> &e) {
    if (error_code ec = e.getError())
	llvm_binary_fail(ec);
    return e.get();
}

symbol make_symbol(const SymbolRef& sym, uint64_t size) {
    auto name = value_or_fail(sym.getName())->str();
    auto addr = value_or_fail(sym.getAddress());
    return symbol{name->str(), sym.getType(), addr, size}; 
}

//! This is our error handling policy for LLVM
//! We distinguish between two kinds of errors:
//! 1. logical errors
//! 2. runtime errors
//!
//! A logic_error means that there is a bug in a programmer's head or
//! DNA. Only valid thing that can be done, is to abort and fix the
//! program. This kind of errors are usually handled with some kind of
//! abnormal and fast exit.
//!
//! A runtime error is an error that is anticipated. A good example,
//! is the connection error. In fact, runtime errors, are not errors at
//! all but just a possible outcome of a computation. Thus if we were
//! in OCaml or Rust or any other normal language, then we can use
//! algebraic data types to encode this, but we're not.
//
//! So the policy for us, is to abort a program on a first kind of
//! error. And handle the anticipated error conditions using a normal
//! control flow. We can't use exceptions, as they are disabled by
//! LLVM. So, we need to use an old C if/else chaining.
//!
//! This is an example of a logic error:
//!
//!    auto name = sym.getName();
//!    if (error_code ec = name.getError())
//!       llvm_binary_fail(ec);
//!
//! We assert that a symbol must have a name, we don't know what to do
//! if there is no name for a symbol. We just abort the program. Instead,
//! of repeating this pattern every time, we can write a function
//!
//! template <typename T>
//! T value_or_fail(const ErrorOr<T>& e) {
//!     if (error_code ec = e.getError())
//!         llvm_binary_fail(ec);
//!     return e.get();
//! }
//!
//! And now we can rewrite `make_symbol` in a more concise, clean and
//! readable way:
//!
//! symbol make_symbol(const SymbolRef& sym, uint64_t size) {
//!     auto name = value_or_fail(sym.getName());
//!     auto addr = value_or_fail(sym.getAddress());
//!     return symbol{name->str(), sym.getType(), addr, size};
//! }
//!
//! So, now it is obvious, that we have two failure points, based on
//! our assumptions, that every symbol in a binary must have a name
//! and an address. This is not true by the way. So we push this
//! precondition to the `make_symbol` function, and we're saying that
//! this `make_symbol` function is defined only for symbols with names
//! and addresses. If someone has called on a wrong symbol, then it is
//! the logic error, and we fail.
//!
//! Now we can start to check, that indeed in all context were we're
//! called this precondition is satisfied. And it can be only
//! satisfied, if there is an explicit check. It is easy to find, that
//! there're no checks. And it looks like, that no such guarantee is
//! made by LLVM functions that we're calling.
//!
//! The conclusion. The `make_symbol` function is logically
//! incorrect. It makes wrong assumptions. Do not document this
//! assumptions and is called is a such manner, that it can be easily
//! broken.
//!
//! Suggested solutions:
//! 1. make `make_symbol` function non total, and return `ErrorOr<symbol>`
//! 2. make `make_symbol` total and define a special invalid symbol.
//!
//! I would suggest the last. Although it is usually a bad idea. But
//! in our case, the function `make_symbol` is just a mapping from
//! llvm symbol representation to our symbol representation. That is
//! intermediate, and later will be mapped again to an OCaml symbol
//! representation. The OCaml representation doesn't make any
//! assumptions, and perform carefull validation of all the input
//! information. That actually means, that we do not need to perform
//! any validation here (why do the double work). So we just create a
//! 4-tuple and give it back. And if any symbol didn't have an address
//! or valid name, then it will be filtered out later.
//!
//! To handle such approach in a generic way we may define the
//! following template function:
//!
//! template <typename T>
//! T value_or_default(const ErrorOr<T> &x, T def=T()) {
//!     if (x) return x.get();
//!     else return def;
//! }
//!
//! It will extract a value from ErrorOr<T> or if there is no value,
//! then it will
//!
//! symbol make_symbol(const SymbolRef& sym, uint64_t size) {
//!     auto name = value_or_default(sym.getName())->str();
//!     auto addr = value_or_default(sym.getAddress());
//!     return symbol{name, sym.getType(), addr, size};
//! }
//!

std::vector<symbol> read(const ObjectFile& obj) {
    std::vector<symbol> symbols;
    auto symbol_sizes = computeSymbolSizes(obj);
    for (auto s : symbol_sizes)
	symbols.push_back(make_symbol(s.first, s.second));
    return symbols;
}
#else
// TODO - refactoring this 3.4 code block
symbol make_symbol(const SymbolRef &sym) {
    StringRef name;
    if(error_code err = sym.getName(name))
	llvm_binary_fail(err);

    kind_type kind;
    if (error_code err = sym.getType(kind))
	llvm_binary_fail(err);

    uint64_t addr;
    if (error_code err = sym.getAddress(addr))
	llvm_binary_fail(err);

    uint64_t size;
    if (error_code err = sym.getSize(size))
	llvm_binary_fail(err);
    return symbol{name.str(), kind, addr, size};
}

template <typename OutputIterator>
OutputIterator read(symbol_iterator begin,
		    symbol_iterator end,
		    OutputIterator out) {
    return std::transform(begin, end, out,
			  [](const SymbolRef& s) { return make_symbol(s); });
}

std::vector<symbol> read(const ObjectFile& obj) {
    int size = distance(obj.begin_symbols(),
                               obj.end_symbols());
    std::vector<symbol> symbols;
    symbols.reserve(size);

    read(obj.begin_symbols(),
         obj.end_symbols(),
         std::back_inserter(symbols));
    return symbols;
}

symbol make_symbol(const SymbolRef& sym, uint64_t addr, uint64_t size) {
    StringRef name;
    if(error_code err = sym.getName(name))
	llvm_binary_fail(err);

    kind_type kind;
    if (error_code err = sym.getType(kind))
	llvm_binary_fail(err);
    return symbol{name.str(), kind, addr, size};
}

std::vector<symbol> read(const COFFObjectFile& obj, const uint64_t image_base) {
    std::vector<symbol> symbols;
    for (auto it = obj.begin_symbols(); it != obj.end_symbols(); ++it) {
        auto sym = obj.getCOFFSymbol(it);

        if (!sym) llvm_binary_fail("not a coff symbol");

        const coff_section *sec = nullptr;
        if (sym->SectionNumber == COFF::IMAGE_SYM_UNDEFINED)
            continue;

        if (error_code ec = obj.getSection(sym->SectionNumber, sec))
            llvm_binary_fail(ec);

        if (!sec) continue;

        uint64_t size = (sec->VirtualAddress + sec->SizeOfRawData) - sym->Value;

        for (auto it = obj.begin_symbols(); it != obj.end_symbols(); ++it) {
            auto next = obj.getCOFFSymbol(it);
            if (next->SectionNumber == sym->SectionNumber) {
                auto new_size = next->Value > sym->Value ?
                    next->Value - sym->Value : size;
                size = new_size < size ? new_size : size;
            }
        }

        auto addr = sec->VirtualAddress + image_base + sym->Value;
        symbols.push_back(make_symbol(*it,addr,size));
    }
    return symbols;
}

std::vector<symbol> read(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
	const pe32_header *pe32;
	if (error_code err = obj.getPE32Header(pe32))
	    llvm_binary_fail(err);
	return read(obj, pe32->ImageBase);
    } else {
        const pe32plus_header *pe32plus = utils::getPE32PlusHeader(obj);
        if (!pe32plus)
            llvm_binary_fail("Failed to extract PE32+ header");
        return read(obj, pe32plus->ImageBase);
    }
}

template <typename ELFT>
std::vector<symbol> read(const ELFObjectFile<ELFT>& obj) {
    //!! identation
    int size1 = distance(obj.begin_symbols(),
                                obj.end_symbols());
    int size2 = distance(obj.begin_dynamic_symbols(),
                                obj.end_dynamic_symbols());

    std::vector<symbol> symbols;
    symbols.reserve(size1+size2);

    auto it = read(obj.begin_symbols(),
                   obj.end_symbols(),
                   std::back_inserter(symbols));

    read(obj.begin_dynamic_symbols(),
         obj.end_dynamic_symbols(),
         it);
    return symbols;
}
#endif

} //namespace sym

namespace sec {
using namespace llvm;
using namespace llvm::object;

struct section {
    std::string name;
    uint64_t addr;
    uint64_t size;
};

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
using namespace llvm;
using namespace llvm::object;


//! rewrite using `value_or_default` function
section make_section(const SectionRef &sec) {
    StringRef name;
    if (error_code ec = sec.getName(name))
	llvm_binary_fail(ec);

    return section{name.str(), sec.getAddress(), sec.getSize()};
}

section_iterator begin_sections(const ObjectFile &obj) {
    return obj.sections().begin();
}

section_iterator end_sections(const ObjectFile &obj) {
    return obj.sections().end();
}
#else
using namespace llvm;
using namespace llvm::object;

section make_section(const SectionRef &sec) {
    StringRef name;
    if (error_code ec = sec.getName(name))
	llvm_binary_fail(ec);

    uint64_t addr;
    if (error_code err = sec.getAddress(addr))
	llvm_binary_fail(err);

    uint64_t size;
    if (error_code err = sec.getSize(size))
	llvm_binary_fail(err);
    return section{name.str(), addr, size};
}

section_iterator begin_sections(const ObjectFile &obj) {
    return obj.begin_sections();
}

section_iterator end_sections(const ObjectFile &obj) {
    return obj.end_sections();
}

section make_section(const coff_section &s, const uint64_t image_base) {
    return section{s.Name, s.VirtualAddress + image_base, s.SizeOfRawData};
}

template <typename T>
std::vector<section> readPE(const COFFObjectFile &obj, const T image_base) {
    std::vector<section> sections;
    for (auto it = obj.begin_sections();
         it != obj.end_sections(); ++it) {
        const coff_section *s = obj.getCOFFSection(it);
        sections.push_back(make_section(*s, image_base));
    }
    return sections;
}

std::vector<section> read(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
	const pe32_header *pe32;
	if (error_code err = obj.getPE32Header(pe32))
	    llvm_binary_fail(err);
	return readPE(obj, pe32->ImageBase);
    } else {
	const pe32plus_header *pe32plus = utils::getPE32PlusHeader(obj);
	if (!pe32plus)
	    llvm_binary_fail("Failed to extract PE32+ header");
	return readPE(obj, pe32plus->ImageBase);
    }
}
#endif

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
    const Triple::ArchType arch_type = static_cast<Triple::ArchType>(obj.getArch());
    const std::string arch = Triple::getArchTypeName(arch_type);
    return arch;
}

template <typename ELFT>
uint64_t image_entry(const ELFObjectFile<ELFT>& obj) {
    return obj.getELFFile()->getHeader()->e_entry;
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
uint64_t image_entry(const MachOObjectFile& obj) {
    typedef MachOObjectFile::LoadCommandInfo command_info;
    auto it =
        std::find_if(obj.begin_load_commands(), obj.end_load_commands(),
                     [](const command_info &info){
                         return
                         info.C.cmd == MachO::LoadCommandType::LC_MAIN;});
    if (it != obj.end_load_commands()) {
        const MachO::entry_point_command *entry_cmd =
            reinterpret_cast<const MachO::entry_point_command*>(it->Ptr);
        return entry_cmd->entryoff;
    } else {
        llvm_binary_fail("LC_MAIN not found, binary version < 10.8");
    }
}
#else
uint64_t image_entry(const MachOObjectFile& obj) {
    typedef MachOObjectFile::LoadCommandInfo command_info;
    typedef std::vector<command_info> commands;
    typedef std::vector<command_info>::const_iterator const_iterator;
    commands cmds = load_commands(obj);
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
#endif

//!! I suspect, that an address of the entry point should also be
//!! shifted by the ImageBase. Please, consult the PE32 documentation
//!! from MSDN
uint64_t image_entry(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (error_code ec = obj.getPE32Header(hdr))
	    llvm_binary_fail(ec);
        if (!hdr)
            llvm_binary_fail("PE header not found");
        return hdr->AddressOfEntryPoint;
    } else {
        return getPE32PlusEntry(obj);
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

//! return NULL in case of error in the following functions, use
//! `std::cerr` to output diagnostics.  Justification. Current
//! implementation of `llvm_binary_fail` shouldn't ever be called in
//! C++ program. It will raise an OCaml exception that will basically
//! transfer a control flow to an exception handler directly into
//! OCaml code with a simple jump instruction. It will break all
//! assumptions of a C++ compiler, and all automatic variables will
//! not be destructed, that will lead to an incosistent state, and
//! every consequent call to C/C++ runtime is undefined. We didn't hit
//! any specific issues so far, because, we indeed bail out as soon as
//! we get the exception, but it is not guaranteed, that this behavior
//! will not change. So, soon I will substitute `llvm_binary_fail`
//! with a function that will pring a diagnostic message and
//! exit/abort. That means, that we should not call `llvm_binary_fail`
//! function any more for just bailing out to OCaml, and must assume,
//! that a call to this function terminates a program. Later we will
//! fix the error handling, but so far, the first step would be to rid
//! of `llvm_binary_fail` calls in places where it is not
//! appropriate. I.e., it should be called only for reporting logical
//! errors. An unsupported binary is not a logic error.
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

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 8
image* create(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBufferRef buf(data_ref, "binary");
    auto binary = createBinary(buf);
    if (error_code ec = binary.getError())
	llvm_binary_fail(ec);
    return create(move(*binary));
}
#else
image* create(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBuffer* buff(MemoryBuffer::getMemBufferCopy(data_ref, "binary"));
    OwningPtr<object::Binary> bin;
    if (error_code ec = createBinary(buff, bin))
        llvm_binary_fail(ec);
    std::unique_ptr<object::Binary> binary(bin.take());
    return create(move(binary));
}
#endif

} //namespace img

#endif //LLVM_BINARY_HPP
