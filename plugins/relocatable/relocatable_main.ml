open Core_kernel.Std
open Bap.Std

include Self()

let () =
  Rel_brancher.init ();
  Config.manpage [
    `S "DESCRIPTION";
    `P "Provides a brancher for relocatable files.";
    `P "Relocatable files, e.g. kernel modules or object files,
     don't have notion about virtual addresses. A call instruction
     in such file doesn't contain a destinition address and
     therefore a target symbol is unknown.";

    `P "A knowledge about computation of a destinition address is
     hidden inside relocation entries, that will be used while
     loading file in memory or linking in executable.
     In other words, relocation entry contains file offset where
     to apply relocation, a symbol table index, respect to which
     relocation must be made, and information about address
     computation.";

     `P "Consider next x86 call instruction:";

     `Pre "
         e8 00 00 00 00
         |  ^^^^^^^^^^^
         |  |
         |  |
         |  file offset, referenced by relocation entry.
         |  Bytes at this offset depend of relocation type and
         |  could be used for address computation.
         |
         file offset of a call instruction.";

     `P "A default brancher doesn't take in account relocations at all,
      so it will not find a target destination, because zero address
      doesn't refer to any symbol in memory image.
      But relocatble brancher does. It will process this case as
      needed and produce a correct list of destinitions.";

      `P "So the only thing remains to do is to choose the relocatable
      brancher:";

      `P "bap foo.o --brancher=relocatable";

      `S "SEE ALSO";
      `P "$(b,bap-plugin-llvm)(1)";
    ];
  Config.when_ready (fun _ -> ())
