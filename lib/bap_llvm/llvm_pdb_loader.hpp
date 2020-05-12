#ifndef LLVM_PDB_LOADER_HPP
#define LLVM_PDB_LOADER_HPP

// loads symbol information from pdb file
//
// Briefly, PDB is multi-stream file format, where each
// stream can represent arbitary data.
// The most important for us is debug information stream - DBI,
//
//
//
//
//
// Basicaly, llvm's pdb dumpers supports two kinds of sessions:
// - dia
// - native
//
//
// Links:
// [1] https://llvm.org/docs/PDB/index.html
// [2] https://github.com/microsoft/microsoft-pdb
// [3] https://pierrelib.pagesperso-orange.fr/exec_formats/MS_Symbol_Type_v1.0.pdf
//


#if LLVM_VERSION_MAJOR < 5
namespace loader {
namespace pdb_loader {

void load(const llvm::object::COFFObjectFile &obj, const std::string &path, ogre_doc &s) {}

}
}
#else

#include <map>

#include <llvm/Object/COFF.h>
#include <llvm/Object/Binary.h>
#include "llvm/DebugInfo/PDB/PDB.h"
#include "llvm/DebugInfo/PDB/PDBTypes.h"
#include "llvm/DebugInfo/PDB/IPDBSession.h"
#include "llvm/DebugInfo/PDB/Native/NativeSession.h"
#include "llvm/DebugInfo/PDB/Native/PDBFile.h"
#include "llvm/DebugInfo/PDB/Native/DbiStream.h"
#include "llvm/DebugInfo/PDB/Native/SymbolStream.h"
#include "llvm/DebugInfo/PDB/Native/ModuleDebugStream.h"
#include "llvm/DebugInfo/CodeView/CodeView.h"
#include "llvm/DebugInfo/CodeView/CVSymbolVisitor.h"
#include "llvm/DebugInfo/CodeView/SymbolVisitorCallbackPipeline.h"
#include "llvm/DebugInfo/CodeView/SymbolDeserializer.h"
#include "llvm/DebugInfo/CodeView/SymbolRecord.h"
#include "llvm/DebugInfo/CodeView/CVSymbolVisitor.h"
#include "llvm/DebugInfo/CodeView/SymbolVisitorCallbacks.h"

#include "llvm_primitives.hpp"
#include "llvm_loader_utils.hpp"

namespace loader {
namespace coff_loader {
const object::coff_section* get_coff_section(const object::COFFObjectFile &obj, const object::SectionRef &sec);
}


namespace pdb_loader {

using namespace llvm;

struct section_info {
    int64_t rel_addr;
    uint64_t offset;
};

typedef std::map<int, section_info> sections;

// still not uniform among llvm versions
uint64_t section_offset(const object::COFFObjectFile &obj, const object::SectionRef &sec) {
    auto coff_sec = coff_loader::get_coff_section(obj, sec);
    return coff_sec->PointerToRawData;
}

// sections indexes start from 1
sections collect_sections(const object::COFFObjectFile &obj) {
    std::size_t i = 1;
    sections secs;
    auto base = obj.getImageBase();
    for (auto sec : prim::sections(obj)) {
        if (auto addr = prim::section_address(sec)) {
            auto raddr = prim::relative_address(base, *addr);
            auto offset = section_offset(obj, sec);
            secs.insert(std::make_pair(i, section_info{raddr,offset}));
        }
        ++i;
    }
    return secs;
}


// For PE-formatted executables, the segment field is interpreted as the PE section number.
struct myvisitor : public codeview::SymbolVisitorCallbacks {

    myvisitor(const sections &secs, ogre_doc &s) : sections(secs), doc(s) {}

    // S_GPROC32, S_LPROC32, S_GPROC32_ID, S_LPROC32_ID, S_LPROC32_DPC or
    // S_LPROC32_DPC_ID
    virtual Error visitKnownRecord(codeview::CVSymbol &CVR, codeview::ProcSym &proc) override {
         auto it = sections.find(proc.Segment);
        if (it == sections.end())
            return Error::success();;

        uint64_t relative_addr = it->second.rel_addr + proc.CodeOffset;
        uint64_t off = it->second.offset + proc.CodeOffset;

        doc.entry("symbol-entry") << proc.Name.str() << relative_addr << proc.CodeSize << off;
        doc.entry("code-entry") << proc.Name.str() << off << proc.CodeSize;

        return Error::success();
    }

private:
    const sections &sections;
    ogre_doc &doc;

};

void load(const llvm::object::COFFObjectFile &obj, const std::string &path, ogre_doc &s) {
    std::unique_ptr<pdb::IPDBSession> Session;
    if (auto er = loadDataForPDB(pdb::PDB_ReaderType::Native, path, Session))
        return;

    pdb::NativeSession *ns = static_cast<pdb::NativeSession *>(Session.get());
    auto &file = ns->getPDBFile();
    auto stream = file.getPDBDbiStream();
    if (!stream)
        return;

    const pdb::DbiModuleList &modules = stream.get().modules();
    auto sections = collect_sections(obj);
    myvisitor vis(sections, s);

    for (uint32_t i = 0; i <  modules.getModuleCount(); ++i) {
        auto module_i = modules.getModuleDescriptor(i);
        uint16_t stream_i = module_i.getModuleStreamIndex();

        auto stream_data = msf::MappedBlockStream::createIndexedStream(
            file.getMsfLayout(), file.getMsfBuffer(), stream_i,
            file.getAllocator());

        pdb::ModuleDebugStreamRef debug_stream(module_i, std::move(stream_data));

        if (auto er = debug_stream.reload())
            return;

        auto symbols = debug_stream.getSymbolsSubstream();
        codeview::SymbolVisitorCallbackPipeline pipeline;
        codeview::SymbolDeserializer deserializer(nullptr, codeview::CodeViewContainer::Pdb);
        pipeline.addCallbackToPipeline(deserializer);
        pipeline.addCallbackToPipeline(vis);

        codeview::CVSymbolVisitor visitor(pipeline);
        if (auto er = visitor.visitSymbolStream(debug_stream.getSymbolArray(), symbols.Offset))
            return;

    }
}

}  // namespace pdb_loader
}  // namespace loader

#endif // LLVM 3.4

#endif // LLVM_PDB_LOADER_HPP
