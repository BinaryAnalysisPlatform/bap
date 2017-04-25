#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 4
#ifndef LLVM_BINARY_34_HPP
#define LLVM_BINARY_34_HPP

#include <memory>
#include <numeric>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <system_error>
#include <tuple>

#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/COFF.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/Archive.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Config/llvm-config.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/Support/ErrorOr.h>

#include "llvm_error_or.hpp"

using llvm::error_code;

namespace {

using namespace llvm;
using namespace llvm::object;

typedef llvm::object::MachOObjectFile macho;
typedef macho::LoadCommandInfo command_info;
typedef std::vector<command_info> macho_commands;

struct failure_of_error : public failure {
    explicit failure_of_error(const error_code &ec) : failure(ec.message()) {}
};

template <typename T, typename OutputIterator, typename UnaryOperation>
OutputIterator content_transform(content_iterator<T> first, content_iterator<T> last,
                                 OutputIterator out, error_code &ec, UnaryOperation op) {
    while (first != last) {
        *out++ = op(first);
        first.increment(ec);
        if (ec) return out;
    }
    return out;
}

macho_commands load_commands(const macho& obj) {
    std::size_t cmd_count = 0;
    if (obj.is64Bit())
        cmd_count = obj.getHeader64().ncmds;
    else
        cmd_count = obj.getHeader().ncmds;
    macho_commands cmds(cmd_count);
    command_info info = obj.getFirstLoadCommandInfo();
    for (std::size_t i = 0; i < cmd_count; ++i) {
        cmds.push_back(info);
        info = obj.getNextLoadCommandInfo(info);
    }
    return cmds;
}

error_or<pe32plus_header> getPE32PlusHeader(const llvm::object::COFFObjectFile& obj) {
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

error_or<uint64_t> getImageBase(const COFFObjectFile &obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header *hdr;
        if (error_code ec = obj.getPE32Header(hdr))
            return failure_of_error(ec);
        return error_or<uint64_t>(hdr->ImageBase);
    } else {
        error_or<pe32plus_header> hdr = getPE32PlusHeader(obj);
        if (!hdr) return hdr;
        return std::move(error_or<uint64_t>(hdr->ImageBase) << hdr.warnings());
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

} //namespace seg

namespace sym {
using namespace llvm;
using namespace llvm::object;

typedef SymbolRef::Type kind_type;
typedef std::pair<SymbolRef, uint64_t> sized_symbol;
typedef std::vector<sized_symbol> symbols_sizes;

error_or<std::string> get_name(const SymbolRef &sym) {
    StringRef name;
    if(error_code ec = sym.getName(name))
        return failure_of_error(ec);
    return success(name.str());
}

error_or<uint64_t> get_addr(const SymbolRef &sym, const ObjectFile &) {
    uint64_t addr;
    if (error_code ec = sym.getAddress(addr))
        return failure_of_error(ec);
    return success(addr);
}

error_or<kind_type> get_kind(const SymbolRef &sym) {
    kind_type kind;
    if (error_code ec = sym.getType(kind))
        return failure_of_error(ec);
    return success(kind);
}

error_or<uint64_t> get_addr(const SymbolRef &sym, const COFFObjectFile &obj) {
    auto image_base = getImageBase(obj);
    if (!image_base) return image_base;
    auto it = symbol_iterator(sym);
    auto coff_sym = obj.getCOFFSymbol(it);
    const coff_section *sec = nullptr;
    if (coff_sym->SectionNumber == COFF::IMAGE_SYM_UNDEFINED)
        return failure("coff image symbol is undefined");
    if (error_code ec = obj.getSection(coff_sym->SectionNumber, sec))
        return failure_of_error(ec);
    return success(sec->VirtualAddress + *image_base + coff_sym->Value);
}

uint64_t get_size(const SymbolRef &sym) {
    uint64_t size;
    if (error_code err = sym.getSize(size))
        size = 0;
    return size;
}

sized_symbol sized_sym(const SymbolRef &s, uint64_t size) {
    return std::make_pair(s, size);
}

sized_symbol sized_sym(const SymbolRef &s) {
    return sized_sym(s, get_size(s));
}

error_or<symbols_sizes> getSymbolSizes(const COFFObjectFile& obj) {
    symbols_sizes sizes;
    error_code ec;
    for (auto it = obj.begin_symbols(); it != obj.end_symbols(); it.increment(ec)) {
        if (ec) return failure_of_error(ec);

        auto sym = obj.getCOFFSymbol(it);
        const coff_section *sec = nullptr;
        if (!sym || sym->SectionNumber == COFF::IMAGE_SYM_UNDEFINED) continue;
        if (error_code ec = obj.getSection(sym->SectionNumber, sec))
            return failure_of_error(ec);
        if (!sec) continue;

        uint64_t size = sec->VirtualAddress + sec->SizeOfRawData - sym->Value;

        for (auto it = obj.begin_symbols(); it != obj.end_symbols(); it.increment(ec)) {
            if (ec) return failure_of_error(ec);
            auto next = obj.getCOFFSymbol(it);
            if (next->SectionNumber == sym->SectionNumber) {
                auto new_size = next->Value > sym->Value ?
                    next->Value - sym->Value : size;
                size = new_size < size ? new_size : size;
            }
        }
        sizes.push_back(sized_sym(*it, size));
    }
    return success(std::move(sizes));
}

template <typename Container>
void fill_symbols(symbol_iterator begin, symbol_iterator end,
                  Container &c, error_code &ec) {
    content_transform(begin, end, std::back_inserter(c),
                      ec, [](symbol_iterator it){ return sized_sym(*it); });
}

error_or<symbols_sizes> getSymbolSizes(const ObjectFile &obj) {
    symbols_sizes sizes;
    error_code ecs, ecd;
    fill_symbols(obj.begin_symbols(), obj.end_symbols(), sizes, ecs);
    fill_symbols(obj.begin_dynamic_symbols(), obj.end_dynamic_symbols(), sizes, ecd);
    if (ecs || ecd)
        return failure(ecs ? ecs.message() : ecd.message());
    return success(std::move(sizes));
}

error_or<symbols_sizes> getSymbolSizes(const MachOObjectFile& obj) {
    symbols_sizes sizes;
    error_code ec;
    fill_symbols(obj.begin_symbols(), obj.end_symbols(), sizes, ec);
    return success(std::move(sizes));
}

} //namespace sym

namespace sec {
using namespace llvm;
using namespace llvm::object;

typedef std::vector<SectionRef> sections_refs;
typedef std::vector<section_iterator> sections_iterators;
typedef std::tuple<std::string, uint64_t, uint64_t> section_descr;

error_or<std::string> getName(const SectionRef &sec) {
    StringRef name;
    if (error_code ec = sec.getName(name))
        return failure_of_error(ec);
    return success(name.str());
}

error_or<uint64_t> getAddr(const SectionRef &sec) {
    uint64_t addr;
    if (error_code ec = sec.getAddress(addr))
        return failure_of_error(ec);
    return success(addr);
}

error_or<uint64_t> getSize(const SectionRef &sec) {
    uint64_t size;
    if (error_code ec = sec.getSize(size))
        return failure_of_error(ec);
    return success(size);
}

std::string getName(const coff_section &s) { return s.Name; }
uint64_t getAddr(const coff_section &s) { return s.VirtualAddress; }
uint64_t getSize(const coff_section &s) { return s.SizeOfRawData;  }

error_or<section_descr> get_section(const SectionRef &sec) {
    auto name = getName(sec);
    auto addr = getAddr(sec);
    auto size = getSize(sec);
    if (auto er = name || addr || size) return failure(er.message());
    return std::move(success(std::make_tuple(*name, *addr, *size))
                     << name.warnings() << addr.warnings() << size.warnings());

}

section_descr get_section(const coff_section &s) {
    return std::make_tuple(getName(s), getAddr(s), getSize(s));
}

error_or<sections_iterators> sections_range(const ObjectFile &obj) {
    sections_iterators its;
    error_code ec;
    content_transform(obj.begin_sections(), obj.end_sections(), std::back_inserter(its),
                      ec, [](section_iterator it) { return it; });
    if (ec) return failure_of_error(ec);
    return success(std::move(its));
}

const coff_section* getCOFFSection(const COFFObjectFile &obj, section_iterator it) {
    return obj.getCOFFSection(it);
}

} //namespace sec

namespace img {
using namespace llvm;
using namespace llvm::object;

error_or<uint64_t> image_entry_macho(const MachOObjectFile& obj) {
    typedef MachOObjectFile::LoadCommandInfo command_info;
    typedef macho_commands::const_iterator const_iterator;
    macho_commands cmds(load_commands(obj));
    const_iterator it =
        std::find_if(cmds.begin(), cmds.end(),
                     [](const command_info &info){
                         return
                         info.C.cmd == MachO::LoadCommandType::LC_MAIN;});
    if (it != cmds.end()) {
        const MachO::entry_point_command *entry_cmd =
            reinterpret_cast<const MachO::entry_point_command*>(it->Ptr);
        return error_or<uint64_t>(entry_cmd->entryoff);
    } else {
        return failure("LC_MAIN not found, binary version < 10.8");
    }
}

error_or<uint64_t> image_entry_coff(const COFFObjectFile& obj) {
    if (obj.getBytesInAddress() == 4) {
        const pe32_header* hdr = 0;
        if (error_code ec = obj.getPE32Header(hdr))
            return failure_of_error(ec);
        if (!hdr)
            return failure("PE header not found");
        return error_or<uint64_t>(hdr->AddressOfEntryPoint + hdr->ImageBase);
    } else {
        error_or<pe32plus_header> hdr = getPE32PlusHeader(obj);
        if (!hdr) return hdr;
        return error_or<uint64_t>(hdr->AddressOfEntryPoint + hdr->ImageBase);
    }
}

error_or<object::Binary> get_binary(const char* data, std::size_t size) {
    StringRef data_ref(data, size);
    MemoryBuffer* buff(MemoryBuffer::getMemBufferCopy(data_ref, "binary"));
    OwningPtr<object::Binary> bin;
    if (error_code ec = createBinary(buff, bin))
        return failure_of_error(ec);
    return error_or<object::Binary>(bin.take());
}

} //namespace img

#endif //LLVM_BINARY_34_HPP
#endif //LLVM=3.4
