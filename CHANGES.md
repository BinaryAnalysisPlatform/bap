2.3.0
=====

- #1263 fixes PE/COFF sections decoding
- #1265 introduces BIL special encodings and publishes BIL CT parser
- #1266 introduces the BIL code slot and a few convenience functions
- #1268 adds a Primus Lisp fronted
- #1273 rewrites the read-symbols plugin to enable multiple files support
- #1274 updates the raw loader to enable better support for targets
- #1275 overhauls target/value abstraction and introduces roles (4/n)
- #1276 adds the --target parameter to disassemble and compare commands
- #1277 enables writing program semantics (lifting) in Primus Lisp
- #1278 moves to centralized and uniform path handling
- #1286 adds namepspaces (packages) to Primus Lisp
- #1287 adds riscv32 and riscv64 support
- #1288 restores output for non-empty instructions
- #1289 fixes the unit tracking mechanism
- #1290 implements ABI description eDSL
- #1291 adds armv8 (aarch64) lifter
- #1293 implements the array to pointer conversion in C ABI processor
- #1294 introduces bap dependencies command
- #1296 uses ARM EABI for Thumb binaries
- #1295 adds YAML as an alternative output of the specification command
- #1297 adds the missing ux*, sx*, s?bz instructions
- #1298 adds more thumb-specific instructions
- #1304 adds the --show-knowledge option to bap mc
- #1303 preserves the encoding name of the instruction
- #1307 renames the core-theory package to just core
- #1306 adds `bap primus-lisp-documentation` command
- #1309 adds support for big-endian arm and thumb architectures

### Bug Fixes

- #1272 fixes two bugs in the Memory module
- #1280 fixes, cleans, and optimizes KB AVL tree implementation
- #1281 prunes empty segments
- #1282 fixes recipes search paths
- #1285 fixes a missing str dependency in the x86 plugin
- #1308 fixes a bug in the method that computes C padding.
- #1310 removes the old unit tracking module, fixes the modern one

2.2.0
=====

### Features

- #1132 adds the missing toupper and tolower prototypes
- #1134 implements better support for cross memory disassembling
- #1142 Tweak jumping and eval cond
- #1112 adds symbolizer based on radare2
- #1155 removes the buffer tracking
- #1160 makes build_plugin.sh more portable
- #1164 splits oasis multipackages into sub-packages
- #1171 creates a separate package for the strings plugin
- #1170 prevents delay slots from becoming a basic block start
- #1173 publishes and documents the new disassembler engine
- #1177 enables unallocated memories in symbolic executor
- #1119 enables multiple projects in the same knowledge base
- #1187 renovates the LLVM backend
- #1198 adds is-executable and format attributes and fixes glibc rt check
- #1197 adds the specification command
- #1196 implements support for ARM Modified Immediate Constants
- #1209 improves symbolization facilities
- #1212 enables enumeration of objects in the knowledge base
- #1200 optimizes functional tests
- #1217 a new portable and efficient knowledge base representation
- #1217 a REPL for querying and modifying the knowledge base
- #1220 removes section and other symbols from the ELF loader output
- #1221 switches to Fowler-Noll-Vo hash algorithm for hashing names
- #1225 optimizes the merge function for OGRE documents
- #1225 implements bin_io and sexp protocol for OGRE docs
- #1225 uses real names for Knowledge.Name.t sexp-serialization
- #1225 overhauls the target/architecture abstraction (1/n)
- #1226 overhauls the target/architecture abstraction (2/n)
- #1227 overhauls the target/architecture abstraction (3/n)
- #1229 upgrades bap to LLVM 11
- #1230 adds a proper handling of unitialized memory in Primus.Memory.map
- #1178 enables ARM Thumb/Thumb2 and interworking
- #1116 updates to OCaml 4.{10,11}, drops 4.07, switches to core_kernel v0.14
- #1234 adds binding operators to the monads library
- #1235 prints only code regions, use sections for names
- #1237 initializes the default policy in primus taint analyzers
- #1241 makes the taint-attached observation on taint introductions
- #1243 enables intermachine communication
- #1244 tweaks the Taint Engine and partially rewrites the Taint GC
- #1245 propagates stub resolver results to program term attributes
- #1246 protects symbolic executor from segfaults when setting memory inputs
- #1255 x86 floating-point lifter

### Bug fixes

- #1123 fixes an inifinite loop in certain Primus Lisp analysis
- #1129 handles correctly XDG_CACHE_HOME
- #1140 fixes getenv Lisp stub
- #1147 fixes taint propagation for unmodeled subroutines
- #1143 fixes the `fgetc` stub and the `channel-input` primitive return type
- #1128 fix rev16 instruction
- #1165 fixes barrier instructions with delay slots
- #1219 fixes the implementation of strncasecmp and strcmp models
- #1224 fixes a trivial typo in the symbolic fread implementation
- #1222 Fix Pcmp instruction
- #1233 fixes Machine.Observation.watch function and publishes fork/switch
- #1238 fixes improper compartmentalization of project computations
- #1240 fixes-bap-taint-gc fixes the sign handling in the atoi stub
- #1251 fixes Primus Lisp typechecker (was missing errors)

2.1.0
=====

### Features

- #957  switches to OCaml 4.0.{7,8,9} and core_kernel v0.12
- #1024 optimizes the Knowledge run function
- #1026 adds `--show-invalid` and `--stop-on-error` bap to mc/objdump
- #1027 adds the `command` stanza to the recipes grammar
- #1028 improves the build time
- #1039 adds an optional omake backend
- #1042 revamps Primus Lisp type checker
- #1053 tweaks primus-mark-visited to mark called stubs as visited
- #1051 adds liveness analysis
- #1055 caches the dissasembler state
- #1061 optimizes Primus observations
- #1061 adds clocks to the Primus interpreter
- #1061 switches to clock ticks as the default limited in primus-limit
- #1059 adds an ability to lift instruction into intrinsics calls
- #1059 adds IEEE754 Primus library
- #1059 allows referencing any variable in a project from Primus Lisp
- #1059 adds the `lisp-primitive` observation
- #1035 adds stubs for `realloc` in Primus Lisp
- #1071 exposes the commit ID in `bap --version`
- #1075 integrates Primus with the Knowledge base
- #1075 introduces Primus systems and components
- #1075 adds restricted mode to Primus Machine monad
- #1075 adds an option to run Primus on marked subroutines
- #1075 refines the timeline of a Primus machine
- #1075 adds an ability to run multiple instances of Primus
- #1079 switches to odig
- #1084 removes the outdated docker images
- #1086 relaxes variables name restriction allowing any character
- #1093 gives programmatic access to ABI processors
- #1036 adds an ABI pass that redirects stubs to implementations
- #1101 updates to LLVM 10
- #1095 prunes unreachable code in the optimization pass
- #1099 adds support for windows PDB files
- #1105 adds support for modern C runtime
- #1105 adds the default C prototype
- #1105 better error reports, no backtraces unless BAP_DEBUG is set
- #1105 adds `ite`, `branch`, and `repeat` Primus operations
- #1105 complete rewrite of Primus generators (wide generators)
- #1105 completely overhauls the random generators implementation
- #1105 new primus-random plugin that controls Primus randomness
- #1105 adds Primus.Env and Primus.Memory generated observations
- #1105 extends the Priumus.Memory interface
- #1105 extends the Primus.Env interface
- #1105 new less heavy interface for Lisp primitives
- #1105 relaxes Primus Interpreter typing rules
- #1105 makes Primus Lisp interpreter more transparent
- #1105 adds static and global variables to Primus Lisp
- #1105 fixes Lisp msg operator
- #1105 enables overloading based on systems and components
- #1105 adds the Primus.Memory.add_region function
- #1105 allows Primus execution from any basic block
- #1105 makes values unique across different machines
- #1105 adds the primus track visited library
- #1105 adds the primus symbolic executor plugin
- #1105 adds symblolic IO system
- #1105 adds many new stubs
- #1105 extends Primus Lisp's Dictionary interface
- #1105 adds the default limit to Primus Limiter
- #1105 reimplements Primus Lisp memory allocator (malloc)
- #1105 adds the `symbol-of-string` primitive
- #1105 adds the `eval-lisp` bap command
- #1105 fixes the multisystem run observation subscription
- #1105 splits the promiscuous mode into subcomponents
- #1105 adds new primitives to the Primus region library
- #1105 tweaks the core systems
- #1105 fixes memcheck-malloc on strn* operations
- #1105 adds incident deduplication
- #1105 adds x86 non-standard registers intialization
- #1105 extends the run plugin

### Bug fixes

- #1025 compels plugins to respect the Bap_main rules
- #1026 fixes `--show-size` and `--show-kinds` in `bap mc`
- #1027 removes the default command hack in `bap`
- #1037 makes dynamic loading sound
- #1042 fixes the visited attribute attaching in primus-mark-visited
- #1045 prevents cmdliner from fetching plugins path from environment
- #1034 fixes arguments attributes in the `callsites` plugin
- #1032 fixes taint garbage collector
- #1048 fixes the missing filename attribute
- #1049 prevents overwriting of a file in the input channel redirection
- #1054 fixes CFG partitioning based on call destinations information
- #1073 fixes the installation of man pages
- #1083 fixes the entry point in docker images
- #1082 disables broken llvm (< 8.0) for aarch64 targets
- #1085 handles correctly ELF files without sections
- #1102 fixes Bil.eval with non-standard memory sizes

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
