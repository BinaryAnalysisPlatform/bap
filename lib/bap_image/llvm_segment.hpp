#ifndef BAP_IMAGE_LLVM_SEGMENT_HPP
#define BAP_IMAGE_LLVM_SEGMENT_HPP

#include <stdint.h>

#include <llvm/Object/Binary.h>
#include <llvm/Object/ObjectFile.h>

struct segment {
    std::string name;
    int64_t address;
    int64_t offset;
    int length;
    int bitwidth;
    bool is_readable;
    bool is_writable;
    bool is_executable;
};

typedef std::vector<segment> segment_sequence;

segment_sequence get_segments(const llvm::object::Binary*);

#endif // BAP_IMAGE_LLVM_SEGMENT_HPP
