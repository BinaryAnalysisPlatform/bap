
#include <iostream>
#include <stdint.h>

#include <llvm/Support/Casting.h>
#include <llvm/Support/ELF.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/MachO.h>

#include "llvm_segment.hpp"

namespace impl {

using namespace llvm;
using namespace llvm::object;

template <typename Phdr>
void add_elf_segment(Phdr hdr, std::string name, std::size_t bits, segment_sequence &s) {
    using namespace llvm::ELF;
    segment seg; 
    seg.name = name;
    seg.offset = hdr.p_offset;
    seg.address = hdr.p_vaddr;
    seg.length = hdr.p_filesz;
    seg.bitwidth = bits;
    seg.is_readable = (hdr.p_flags & PF_R) != 0;
    seg.is_writable = (hdr.p_flags & PF_W) != 0;
    seg.is_executable = (hdr.p_flags & PF_X) != 0;
    s.push_back(seg);    
}

template <typename Elf_file>
void fill_elf_segments(const Elf_file *obj, std::size_t bits, segment_sequence &s) {
    using namespace llvm::ELF;
    typedef typename Elf_file::Elf_Phdr p_header;
    typedef typename Elf_file::Elf_Phdr_Iter iterator;    
    std::size_t counter = 0 ;
    for (iterator it = obj->begin_program_headers();
         it != obj->end_program_headers(); ++it, ++counter) {
        p_header hdr = *it;
        if (hdr.p_type == PT_LOAD) 
            add_elf_segment(hdr, std::to_string(counter), bits, s);
    }
}

segment_sequence elf_segments(const Binary *bin) {
    std::vector<segment> s;
    const ObjectFile* obj = dyn_cast<ObjectFile>(bin);
    if (obj) {
        const ELF32LEObjectFile *elf_32le = dyn_cast<ELF32LEObjectFile>(obj);
        const ELF32BEObjectFile *elf_32be = dyn_cast<ELF32BEObjectFile>(obj);
        const ELF64LEObjectFile *elf_64le = dyn_cast<ELF64LEObjectFile>(obj);
        const ELF64BEObjectFile *elf_64be = dyn_cast<ELF64BEObjectFile>(obj);
        if (elf_32le) {
            impl::fill_elf_segments(elf_32le->getELFFile(), 32, s);
        } 
        else if (elf_32be) {
            impl::fill_elf_segments(elf_32be->getELFFile(), 32, s);  
        } 
        else if (elf_64le) {
            impl::fill_elf_segments(elf_64le->getELFFile(), 64, s);
        }
        else if (elf_64be) {
            impl::fill_elf_segments(elf_64be->getELFFile(), 64, s);
        } else { }
    }
    return s;
}

template <typename SegCommand>
void add_mac_segment(SegCommand cmd, std::size_t bits, segment_sequence &s) {
    using namespace MachO; 
    segment seg; 
    seg.name = cmd.segname;
    seg.offset = cmd.fileoff;
    seg.address = cmd.vmaddr;
    seg.length = cmd.filesize;
    seg.bitwidth = bits;
    seg.is_readable = (cmd.initprot & VM_PROT_READ) != 0;
    seg.is_writable = (cmd.initprot & VM_PROT_WRITE) != 0;
    seg.is_executable = (cmd.initprot & VM_PROT_EXECUTE) != 0;
    s.push_back(seg);    
}

std::size_t macho_commands_count(const MachOObjectFile *macobj) {
    if (macobj->is64Bit())
        return macobj->getHeader64().ncmds;
    else
        return macobj->getHeader().ncmds;   
}

uint64_t macho_enrty_point(const MachOObjectFile *macobj) {
    MachOObjectFile::LoadCommandInfo info = macobj->getFirstLoadCommandInfo();
    std::size_t cmd_count = macho_commands_count(macobj);
    //check for LC_MAIN command
    for (std::size_t i = 0; i < cmd_count; ++i) {
        if (info.C.cmd == MachO::LC_MAIN) {
            MachO::section_64 s = macobj->getSection64(info, 1);
            return ((const MachO::entry_point_command *)info.Ptr)->entryoff;
        }
        info = macobj->getNextLoadCommandInfo(info);
    }
    error_code ec;
    StringRef name;
    // otherwise looking for __text section
    for (section_iterator it = macobj->begin_sections();
         it != macobj->end_sections(); it.increment(ec)) {
        if (ec || it->getName(name))
            break;
        if (name == "__text") {
            uint64_t addr;
            ec = it->getAddress(addr);
            if (!ec)
                return addr;
        }
    }
    // otherwise looking for _start or _main symbol
    for (symbol_iterator si = macobj->begin_symbols(); 
         si != macobj->end_symbols(); si.increment(ec)) {
        if (ec || si->getName(name))
            break;
        if (name == "_start" || name == "_main") {
            uint64_t addr;
            ec = si->getAddress(addr);
            if (ec)
                return addr;
        }
    }
    // otherwise
    return 0;    
}

segment_sequence mac_segments(const Binary *bin) {
    using namespace MachO; 
    segment_sequence s;
    const MachOObjectFile *macobj = dyn_cast<MachOObjectFile>(bin);
    if (macobj) {
        bool is64 = macobj->is64Bit();
        std::size_t cmd_count = 0;
        if (is64) 
            cmd_count = macobj->getHeader64().ncmds;
        else
            cmd_count = macobj->getHeader().ncmds;
        MachOObjectFile::LoadCommandInfo info = macobj->getFirstLoadCommandInfo();
        for (std::size_t i = 0; i < cmd_count; ++i) {
            if (is64 && info.C.cmd == LoadCommandType::LC_SEGMENT_64) 
                add_mac_segment(macobj->getSegment64LoadCommand(info), 64, s);
            if (!is64 && info.C.cmd == LoadCommandType::LC_SEGMENT)
                add_mac_segment(macobj->getSegmentLoadCommand(info), 32, s);
            info = macobj->getNextLoadCommandInfo(info);
        }
    }
    return s;
}

} // namespace impl

segment_sequence get_segments(const llvm::object::Binary *bin) {
    if (bin->isELF())
        return impl::elf_segments(bin);
    if (bin->isMachO())
        return impl::mac_segments(bin);
    else return segment_sequence();
}
