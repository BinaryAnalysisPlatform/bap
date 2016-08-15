#ifndef LLVM_BINARY_34_HPP
#define LLVM_BINARY_34_HPP

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
#include <llvm/ADT/OwningPtr.h>

using std::move;
using llvm::error_code;

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


namespace {
using namespace llvm;
using namespace llvm::object;

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

const llvm::object::pe32plus_header* getPE32PlusHeader(const llvm::object::COFFObjectFile& obj) {
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
        cur_ptr += sizeof(llvm::object::coff_file_header);
        return reinterpret_cast<const llvm::object::pe32plus_header *>(start + cur_ptr);
    }
    return NULL;
}

uint64_t getImageBase(const COFFObjectFile &obj) {
    if (obj.getBytesInAddress() == 4) {
	const pe32_header *hdr;
	if (error_code ec = obj.getPE32Header(hdr))
	    llvm_binary_fail(ec);
	return hdr->ImageBase;
    } else {
	const pe32plus_header *hdr = getPE32PlusHeader(obj);
	if (!hdr)
	    llvm_binary_fail("Failed to extract PE32+ header");
	return hdr->ImageBase;
    }
}

} // namespace

namespace seg {
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
    for (auto it = obj.begin_sections(); it != obj.end_sections(); ++it)
	sections.push_back(it);
    return sections;
}

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

typedef SymbolRef::Type kind_type;
 
std::string name_or_default(const SymbolRef &sym) {
    StringRef name;
    if(error_code err = sym.getName(name))
	name = StringRef("INVALID SYMBOL");

    return name.str();
}

uint64_t addr_or_default(const SymbolRef &sym) {
    uint64_t addr;
    if (error_code err = sym.getAddress(addr))
	addr = 0;

    return addr;
}

kind_type get_type(const SymbolRef &sym) {
    kind_type kind;
    if (error_code err = sym.getType(kind))
	kind = kind_type::ST_Unknown;

    return kind;
}

uint64_t addr_or_default(const SymbolRef &sym, const COFFObjectFile &obj) {
    uint64_t image_base = getImageBase(obj);

    auto it = symbol_iterator(sym);
    auto coff_sym = obj.getCOFFSymbol(it);
    
    const coff_section *sec = nullptr;
    if (coff_sym->SectionNumber == COFF::IMAGE_SYM_UNDEFINED)
	return 0;

    if (error_code ec = obj.getSection(coff_sym->SectionNumber, sec))
	llvm_binary_fail(ec);

    auto addr = sec->VirtualAddress + image_base + coff_sym->Value;
    return addr;
}

uint64_t get_size(const SymbolRef &sym) {
    uint64_t size;
    if (error_code err = sym.getSize(size))
	size = 0;

    return size;
}

// TODO
std::vector<std::pair<SymbolRef, uint64_t>> getSymbolSizes(const COFFObjectFile& obj) {
    std::vector<std::pair<SymbolRef, uint64_t>> symbol_sizes;
    uint64_t image_base = getImageBase(obj);
    for (auto it = obj.begin_symbols(); it != obj.end_symbols(); ++it) {
	auto sym = obj.getCOFFSymbol(it);

	// As per discussed regarding value_or_default, failure
	// of this check should not terminate program
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

	symbol_sizes.push_back(std::make_pair(*it, size));
    }
    return symbol_sizes;
}

std::vector<std::pair<SymbolRef, uint64_t>> getSymbolSizes(const ObjectFile &obj) {
    int size = distance(obj.begin_symbols(),
			obj.end_symbols());
    std::vector<std::pair<SymbolRef, uint64_t>> symbol_sizes;
    symbol_sizes.reserve(size);

    for (auto it = obj.begin_symbols(); it != obj.end_symbols(); ++it) {
	symbol_sizes.push_back(std::make_pair(*it, get_size(*it)));
    }
    
    return symbol_sizes;
}

template <typename OutputIterator>
OutputIterator getSymbolSizes(symbol_iterator begin,
		    symbol_iterator end,
		    OutputIterator out) {
    return std::transform(begin, end, out,
			  [](const SymbolRef& s) { return std::make_pair(s, get_size(s)); });
}

template <typename ELFT>
std::vector<std::pair<SymbolRef, uint64_t>> getSymbolSizes(const ELFObjectFile<ELFT>& obj) {
    int size1 = distance(obj.begin_symbols(),
			 obj.end_symbols());
    int size2 = distance(obj.begin_dynamic_symbols(),
			 obj.end_dynamic_symbols());

    std::vector<std::pair<SymbolRef, uint64_t>> symbol_sizes;
    symbol_sizes.reserve(size1+size2);

    auto it = getSymbolSizes(obj.begin_symbols(),
		   obj.end_symbols(),
		   std::back_inserter(symbol_sizes));

    getSymbolSizes(obj.begin_dynamic_symbols(),
	 obj.end_dynamic_symbols(),
	 it);
    return symbol_sizes;
}

} //namespace sym

namespace sec {
using namespace llvm;
using namespace llvm::object;

std::string getName(const SectionRef &sec) {
    StringRef name;
    if (error_code ec = sec.getName(name))
	llvm_binary_fail(ec);

    return name.str();
}

uint64_t getAddr(const SectionRef &sec) {
    uint64_t addr;
    if (error_code err = sec.getAddress(addr))
	llvm_binary_fail(err);
    
    return addr;
}

uint64_t getSize(const SectionRef &sec) {
    uint64_t size;
    if (error_code err = sec.getSize(size))
        llvm_binary_fail(err);

    return size;;
}

std::string getName(const coff_section &s) {
    return s.Name;
}

uint64_t getAddr(const coff_section &s) {
    return s.VirtualAddress;
}

uint64_t getSize(const coff_section &s) {
    return s.SizeOfRawData;
}

section_iterator begin_sections(const ObjectFile &obj) {
    return obj.begin_sections();
}

section_iterator end_sections(const ObjectFile &obj) {
    return obj.end_sections();
}

std::vector<section_iterator> obj_sections(const ObjectFile &obj) {
    std::vector<section_iterator> sections;
    for (auto it = obj.begin_sections(); it != obj.end_sections(); ++it)
	sections.push_back(it);
    return sections;
}

const coff_section* 
getCOFFSection(const COFFObjectFile &obj, section_iterator it) {
    return obj.getCOFFSection(it);
}

} //namespace sec

namespace img {
using namespace llvm;
using namespace llvm::object;

uint64_t image_entry_macho(const MachOObjectFile& obj) {
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

uint64_t image_entry_coff(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (error_code ec = obj.getPE32Header(hdr))
	    llvm_binary_fail(ec);
        if (!hdr)
            llvm_binary_fail("PE header not found");
        return hdr->AddressOfEntryPoint + hdr->ImageBase;
    } else {
        const pe32plus_header *hdr = getPE32PlusHeader(obj);
        return hdr->AddressOfEntryPoint + hdr->ImageBase;
    }
}

std::unique_ptr<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBuffer* buff(MemoryBuffer::getMemBufferCopy(data_ref, "binary"));
    OwningPtr<object::Binary> bin;
    if (error_code ec = createBinary(buff, bin)) {
	std::cerr << ec << "\n";
        return NULL;
    }
    std::unique_ptr<object::Binary> binary(bin.take());
    return move(binary);
}

} //namespace img

#endif //LLVM_BINARY_34_HPP
