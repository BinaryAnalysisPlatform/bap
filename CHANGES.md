2.0.0
=====

### Features

- #1016 adds unknown architecture
- #1014 restores postinstall and man pages
- #1013 tweaks the cache plugin
- #1011 tweaks the subroutine ordering in the run plugin
- #1006 extends the test coverage on Travis
- #1005 introduces Bap_main the entry point to BAP
- #1005 new command line interface and library
- #1005 a new `raw` loader for opening unknown files and raw code
- #1005 a new `objdump` command for linearly disassembling binaries
- #1005 a new Byteweight threshold using the Bayes Factors procedure
- #1005 adds more control over byteweight thresholding
- #1005 implements custom thresholding procedures for Byteweight
- #1005 extends the Trie module with iterators and printers
- #1005 new toplevel based on utop
- #1005 new bytecode frontend for debugging
- #1005 adds a central location for regular readers and writers
- #999  tweaks Graphlibs fixpoint to preserve previous solution
- #998  represents rep prefix with while in x86
- #993  prevents Primus from going to deep into PLT entries
- #991  tweaks the print plugin
- #990  represents ARM conditionals with ite expressions when possible
- #998  implements more fine granular view on the image memory
- #979  parametrize Lisp malloc with an initialization strategy
- #960  new subroutine packing algorithm
- #960  new incremental disassembler
- #960  new knowledge representation library
- #960  new representation of program semantics
- #960  new bitvectors library
- #948  disables Primus' taint GC by default

### Bug fixes

- #1013 resolves leaking files in the cache plugin
- #1003 rectifies Primus semantics in case of exceptions
- #1002 fixes bind operator in the Future library
- #1000 fixes instruction properties computation for barriers
- #985  fixes atexit Lisp stub
- #980  fixes a bug in the configure script
- #971  limits continuations at forks in the promiscuous mode
- #970  fixes the argument evaluation order in call-return
- #964  fixes Primus random generators
- #962  fixes the semantics of signed division by zero in x86
- #958  fixes Primus memory semantics with randomized memories
- #955  improves stack handling in Primus for x86
- #950  fixes the taint sanitization procedure

1.6.0
=====

### Features

- #893  adds integration with ida 7
- #892  implements helper functions for creating and manipulating partitions
- #906  makes dead code elimination less conservative
- #914  preserves brancher information in the BIL code of an instruction
- #820  Jane Street 0.11.x library compatibility + minor fixes
- #922  few x86 enhancements
- #923  SSE XMM0 ABI
- #933  enables bap-elf
- #934  adds the compiler option to byteweight
- #932  enables memory sharing between instructions
- #938  removed upper bound for llvm version, compatibility with LLVM 8.0
- #926  enables functions with multiple entry

### Bug fixes

- #907  fixes the free-vars-by-dominators computation algorithm
- #915  fixed building on travis
- #920  fix the i64 import error
- #927  fixes ADT printer
- #939  fixes the order of arguments in the callsites plugin
- #937  fixes TOCTOU bug in bap log
- #941  fixes a dependency bug in primus lisp docs

1.5.0
=====

### Features

- #887 Llvm 7.0 compatibility
- #872 Constant tracking analysis
- #871 Information sensitivity analysis
- #868 IDA Pro brancher.
- #866 Bap-elf compilation for ocaml >= 4.04.1
- #857 Optimization pass
- #855 Better reconstructor, symtab, and brancher performance
- #840 Traps for memory and division operations in Primus
- #833 Support for x86 CDQ, CDQE, CQO, CWD, CWDE, CBW opcodes
- #830 BIL passes
- #821 Interface for binable interval trees
- #815 Recipes enhancements
- #813 Mangling of duplicated subroutines
- #799 Macho loader enhancements
- #798 Primus Lisp enhancements
- #795 Llvm 6.0 compatibility


### Bug fixes

- #863 Fixes calls search in reconstructor
- #856 Fixes the bug that was producing unreachable blocks
- #853 Cleans /tmp/ida every time ida is called
- #847 Fixes type inference
- #841 Several Primus fixes
- #836 Fixes packed x86 instructions
- #832 Fixes function names in docstrings
- #827 Fixes compilations issue on 32 bit machine
- #817 Hardens IDA plugin
- #804 Removes duplicated sections in disassemble output
- #796 Fixes paths comprehension in api plugin
- #792 Restores the section view
- #791 Produces correct callgraphs if there are no calls in a program
- #789 Fixes BIL normalization procedure
- #733 Fixed bugs in x86 shift instructions

1.4.0
=====

### Features

- PR#762 MIPS and MIPS64 lifters
- PR#739 PowerPC and PowerPC64 lifters
- PR#744 LLVM 5.0 compatibility
- PR#734 BARE Binary Analysis Rule Engine
- PR#734 New Taint Analysis Framework
- PR#734 Primus Lisp 2.0 with symbols and methods
- PR#734 Recipes
- PR#734 Primus Test Framework
- PR#734 Dataflow and Abstract Interpretation Framework
- PR#734 Progress Reports and Profilers
- PR#773 New primitives for BML

### Bug fixes

- PR#782 Incorrect error handling in x86 lifter
- PR#734 Failure to decode ICC binaries
- PR#772 Fixes equiv type in Graphlib
- PR#771 Unhardcodes llvm backed in the linear sweep disassembler
- PR#770 Fixes the memory printer
- PR#761 Fixes handling relocations in reconstructor
- PR#759 Fixes race condition in the source merge procedure
- PR#758 Restores the source-type command line option
- PR#755 Proper handling of tail calls in IR lifter
- PR#754 Fixes segment registers in mov instruction
- PR#746 Fixes xor in the BIL simplfication procedure
- PR#728 Fixes flag calculation in the x86 sub instruction
- PR#727 Fixes numerous missed sign extensions in x86 lifter
- PR#725 Adds modulo operation to x86 rot/rol instructions
- PR#724 Fixes operands order in the x86 xadd instruction
- PR#723 Fixes segment duplication

1.3.0
=====

### Features

- PR#700 LLVM-4.0 is now supported
- PR#688 Primus - the microexecution framework
- PR#688 BIL type checker and normalizer
- PR#688 New constant folding with effect analysis
- PR#688 SSA transformation plugin
- PR#688 Dead code elimination plugin
- PR#688 New pretty-printer for BIL and bitvectors
- PR#684 Support for cmdliner 1.0
- PR#682 Severely reduces memory consumption
- PR#680 New thin bitvector representation
- PR#663 Adds topic tags to bundle
- PR#654 New OGRE based loader that supports:
  - Linux Kernel Modules
  - MachO Kernel Bundles (LLVM 3.8+ only)
  - Plain object files
  - Shared libraries
- PR#630 Enhancements in IDA plugin


### Bug fixes

- PR#688 Type errors in the x86 lifter
- PR#688 Type errors in the ARM lifter
- PR#688 Fixes constant folding
- PR#672 Fixes demangler plugin
- PR#665 Fixes symbol cases in objdump
- PR#644 Robustness fixes in x86 lifter

1.2.0
=====

### Features
- PR#609 compilation with ocaml 4.04.0
- PR#621 don't store api files in api plugin
- PR#627 beagle - obfuscated string solver
- binary release (deb, rpm, tgs)
- c-bindings

1.1.0
=====

### Bug fixes
- PR#586 segfault with short or damaged files fed to bap.
- PR#590 llvm 3.8 specific issues
- PR#592 a bug in lifting x86 PSHUFD/PSHUFB instructions
- PR#595 bap exit status
- PR#596 most of the compilation warnings

### Features
- PR#593 bapbundle: it is no longer needed to specify the .plugin extension
- PR#597 API pass will stop processing in case of the error
- PR#599 print backtraces from passes
- PR#600 documented memory interface

1.0.0
=====

- A powerful plugin system
- Split Bap.Std into several libraries.
- The disassembler layer is severely rewritten
- Made project storable and loadable
- Added new injection points
- Added BIL interpreters
- Removed bap-server
- New python interface (see https://BinaryAnalysisPlatform/bap-python)
- New ida integration, that works in both directions
- Multipass disassembling
- llvm-3.8 support (#546)
- new x86 lifter (#549)
- new testsuite with functional tests (#520)
- extensible API/ABI (#448)
