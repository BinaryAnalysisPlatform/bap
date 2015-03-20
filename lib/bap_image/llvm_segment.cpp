
#include <iostream>
#include <stdint.h>

#include <llvm/Support/Casting.h>
#include <llvm/Support/ELF.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Object/ELFObjectFile.h>

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

} // namespace impl

segment_sequence elf_segments(const llvm::object::Binary *bin) {
    using namespace llvm;
    using namespace llvm::object;
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

segment_sequence get_segments(const llvm::object::Binary *bin) {
    if (bin->isELF())
        return elf_segments(bin);
    else return segment_sequence();
}
