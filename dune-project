(lang dune 3.1)
(using dune_site 0.1)

(name bap)
(license MIT)
(version dev)
(authors "The BAP Team")
(maintainers "Ivan Gotovchits <ivg@ieee.org>")
(source (github BinaryAnalysisPlatform/bap))


(explicit_js_mode)
(formatting disabled)

(use_standard_c_and_cxx_flags false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Package Descriptions                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Meta Packages

(package
 (name bap)
 (synopsis "BAP Meta Package")
 (tags (bap meta))
 (allow_empty)
 (depends
  (bap-core (= :version))
  (bap-beagle (= :version))
  (bap-beagle-strings (= :version))
  (bap-constant-tracker (= :version))
  (bap-microx (= :version))
  (bap-primus-dictionary (= :version))
  (bap-primus-powerpc (= :version))
  (bap-primus-propagate-taint (= :version))
  (bap-primus-random (= :version))
  (bap-primus-region (= :version))
  (bap-primus-support (= :version))
  (bap-primus-systems (= :version))
  (bap-primus-taint (= :version))
  (bap-primus-test (= :version))
  (bap-primus-x86 (= :version))
  (bap-run (= :version))
  (bap-strings (= :version))
  (bap-taint (= :version))
  (bap-taint-propagator (= :version))
  (bap-term-mapper (= :version))
  (bap-trivial-condition-form (= :version))
  (bap-warn-unused (= :version))))

(package
 (name bap-core)
 (synopsis "The core BAP packages")
 (tags (bap meta))
 (allow_empty)
 (description "the meta package to install the core parts of BAP")
 (depends
  (bap-abi (= :version))
  (bap-analyze (= :version))
  (bap-api (= :version))
  (bap-arm (= :version))
  (bap-bil (= :version))
  (bap-build (= :version))
  (bap-bundle (= :version))
  (bap-byteweight (= :version))
  (bap-cache (= :version))
  (bap-callgraph-collator (= :version))
  (bap-callsites (= :version))
  (bap-c (= :version))
  (bap-cxxfilt (= :version))
  (bap-demangle (= :version))
  (bap-dependencies (= :version))
  (bap-disassemble (= :version))
  (bap-dump-symbols (= :version))
  (bap-elementary (= :version))
  (bap-flatten (= :version))
  (bap-frontc (= :version))
  (bap-frontend (= :version))
  (bap-glibc-runtime (= :version))
  (bap-llvm (= :version))
  (bap-mc (= :version))
  (bap-mips (= :version))
  (bap-objdump (= :version))
  (bap-optimization (= :version))
  (bap-patterns (= :version))
  (bap-plugins (= :version))
  (bap-powerpc (= :version))
  (bap-primus-lisp (= :version))
  (bap-primus (= :version))
  (bap-print (= :version))
  (bap-raw (= :version))
  (bap-recipe-command (= :version))
  (bap-recipe (= :version))
  (bap-relation (= :version))
  (bap-relocatable (= :version))
  (bap-report (= :version))
  (bap-riscv (= :version))
  (bap-specification (= :version))
  (bap-ssa (= :version))
  (bap-std (= :version))
  (bap-stub-resolver (= :version))
  (bap-symbol-reader (= :version))
  (bap-systemz (= :version))
  (bap-thumb (= :version))
  (bap-toplevel (= :version))
  (bap-x86 (= :version))))

(package
 (name bap-extra)
 (synopsis "The extra BAP packages")
 (tags (bap meta))
 (allow_empty)
 (description "the meta package to install the core parts of BAP")
 (depends
  (bap (= :version))
  (bap-byteweight-frontend (= :version))
  (bap-elf (= :version))
  (bap-primus-symbolic-executor (= :version))
  (bap-ghidra (= :version))
  (bap-trace (= :version))
  (bap-traces (= :version))
  (bap-radare2 (= :version))))

(package
 (name bap-common)
 (synopsis "A Package on which all BAP packages depend")
 (tags (bap meta))
 (sites
  (lib plugins)
  (share api)
  (share lisp)
  (share primus)
  (share semantics)
  (share signatures)
  (share site_lisp))
 (depends
  (base (and (>= v0.14) (< v0.16)))
   dune
   dune-configurator
   dune-site
  (ocaml (> 4.08.0))
  (stdio (and (>= v0.14) (< v0.16)))))

(package
 (name bap-primus-support)
 (synopsis "Provides essential Primus components")
 (tags (bap meta))
 (allow_empty)
 (depends
  (bap-primus-exploring-scheduler (= :version))
  (bap-primus-greedy-scheduler (= :version))
  (bap-primus-limit (= :version))
  (bap-primus-loader (= :version))
  (bap-primus-mark-visited (= :version))
  (bap-primus-print (= :version))
  (bap-primus-promiscuous (= :version))
  (bap-primus-round-robin-scheduler (= :version))
  (bap-primus-wandering-scheduler (= :version))))

;; Data Packages

(package
 (name bap-signatures)
 (tags (bap data))
 (allow_empty)
 (synopsis "A data package with BAP binary signatures"))

;; Actual Packages

(package
 (name bap-abi)
 (synopsis "BAP plugin and library for ABI-specific transformations")
 (tags (bap bap-pass bap-plugin))
 (depends
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-analyze)
 (synopsis "BAP Knowledge Base REPL")
 (tags (bap bap-command bap-plugin))
 (depends
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (linenoise (and (>= 1.1.0) (< 2.0.0)))
  (monads (= :version))
  (bap-common (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-api)
 (synopsis "BAP pass that adds arguments to subroutines")
 (tags (bap bap-plugin bap-library))
 (depends (bap-common (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
   fileutils
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-arm)
 (synopsis "BAP ARM support package")
 (tags (bap bap-plugin bap-library arm))
 (depends
  (bap-abi (= :version))
  (bap-api (= :version))
  (bap-core-theory (= :version))
  (bap-c (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (bap-traces (= :version))
  (bitvec-order (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (bap-common (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-beagle)
 (synopsis "BAP obfuscated string solver")
 (tags (bap bap-plugin))
 (deprecated_package_names bap-beagle-prey)
 (depends
  (bap-future (= :version))
  (bap-microx (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (bap-strings (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-bil)
 (synopsis "BAP Instruction Language (BIL)")
 (tags (bap bap-plugin))
 (depends
  (bap-core-theory (= :version))
  (bap-future (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (bitvec-order (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (bap-common (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-bml)
 (synopsis "BAP Term Mapping Language (BML)")
 (tags (bap bap-library))
 (depends
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-byteweight)
 (synopsis "BAP function start identification library and pass")
 (tags (bap bap-library bap-plugin))
 (depends
  (bap-signatures (= :version))
  (bap-std (= :version))
  (camlzip (and (>= 1.12) (< 2.0)))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (regular (= :version))
  (uri (>= 4.2.0))))

(package
 (name bap-byteweight-frontend)
 (synopsis "A frontend to the BAP function start identification framework")
 (tags (bap bap-frontend bap-command))
 (depends
  (bap-byteweight (= :version))
  (bap-std (= :version))
  (cmdliner (and (>= 1.0) (< 2.0)))
  (core_kernel (and (>= v0.14) (< v0.16)))
   fileutils
  (bap-common (= :version))
  (ocurl (>= 0.9.0))
  (ppx_bap (= :version))
  (re (and (>= 1.0) (< 2.0)))))

(package
 (name bap-build)
 (synopsis "BAP Build Tool (ocamlbuild+bap)")
 (tags (bap bap-tool ocamlbuild))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
   ocamlbuild
   ocamlfind
  (ppx_bap (= :version))))

(package
 (name bap-bundle)
 (synopsis "A bundler for BAP plugins")
 (tags (bap bap-tool))
 (depends
  (camlzip (and (>= 1.0) (< 2.0)))
  (core_kernel (and (>= v0.14) (< v0.16)))
   fileutils
  (bap-common (= :version))
  (ppx_bap (= :version))
  (uri (>= 4.2.0))))

(package
 (name bap-c)
 (synopsis "The C language support library for BAP")
 (tags (bap bap-library c))
 (depends
  (bap-abi (= :version))
  (bap-api (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-cache)
 (synopsis "BAP cache management")
 (tags (bap bap-plugin))
 (depends
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
   fileutils
   mmap
  (bap-common (= :version))
  (ppx_bap (= :version))
  (regular (= :version))
  (uuidm (>= 0.9.7))))

(package
 (name bap-callgraph-collator)
 (synopsis "Compares binaries callgraphs")
 (tags (bap bap-analysis))
 (depends
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (graphlib (= :version))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (re (and (>= 1.0) (< 2.0)))
  (regular (= :version))))

(package
 (name bap-callsites)
 (synopsis "BAP pass that injects data definitions at call sites")
 (tags (bap bap-plugin bap-pass))
 (depends
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))))

(package
 (name bap-core-theory)
 (synopsis "BAP Machine Code Semantics")
 (description "\
The Core Theory is an intermediate language that is designed to \
express the semantics of computer programs. It focuses on programs \
that are represented in binary machine code and is capable of an \
accurate representation of the architectural and micro-architectural \
details of the program behavior.")
 (tags (bap bap-library))
 (depends
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bitvec (= :version))
  (bitvec-binprot (= :version))
  (bitvec-order (= :version))
  (bitvec-sexp (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-constant-tracker)
 (synopsis "A constant tracking analysis based on BAP Primus")
 (tags (bap bap-analysis bap-plugin bap-primus))
 (depends
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))))

(package
 (name bap-cxxfilt)
 (synopsis "A name demangler that uses system's c++filt")
 (tags (bap bap-plugin))
 (depends
  (bap-demangle (= :version))
  (bap-std (= :version))
  (conf-binutils (>= 0.3))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))))

(package
 (name bap-demangle)
 (synopsis "Provides names service and demangling facilities for BAP")
 (tags (bap bap-library))
 (depends
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))))

(package
 (name bap-dependencies)
 (synopsis "Shows program depenencies")
 (tags (bap bap-command))
 (depends
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-disassemble)
 (synopsis "Disassembles and analyzes binaries using BAP")
 (tags (bap bap-command))
 (depends
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-relation (= :version))
  (bap-std (= :version))
  (bitvec-binprot (= :version))
  (bitvec-order (= :version))
  (bitvec-sexp (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (bap-common (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-dump-symbols)
 (synopsis "A BAP plugin for dumping symbol information")
 (tags (bap bap-plugin))
 (depends
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (graphlib (= :version))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-dwarf)
 (synopsis "BAP DWARF parser")
 (tags (bap bap-library dwarf))
 (depends
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-elementary)
 (synopsis "Floating-point approximations of elementary functions")
 (tags (bap bap-library floating-points))
 (depends
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-std (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-elf)
 (synopsis "BAP ELF parser and program loader")
 (tags (bap bap-plugin elf))
 (depends
  (bap-dwarf (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (ppx_bitstring (and (>= 4.0.0) (< 5.0.0)))
  (regular (= :version))))

(package
 (name bap-flatten)
 (synopsis "A BAP pass that flattens program to TAC")
 (tags (bap bap-plugin))
 (depends
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))))

(package
 (name bap-frontend)
 (synopsis "The main BAP command-line tool")
 (tags (bap bap-command))
 (depends
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
   ocamlfind
  (regular (= :version))))

(package
 (name bap-frontc)
 (synopsis "A BAP C language fronted based on FrontC")
 (tags (bap bap-plugin c))
 (depends
  (bap-c (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (FrontC (>= 4.1.0))
  (bap-common (= :version))))

(package
 (name bap-future)
 (synopsis "A library for asynchronous values")
 (description "\
A library for reasoning about state based dynamic systems. This can \
be seen as a common denominator between Lwt and Async libraries.")
 (tags (bap future))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (bap-common (= :version))))

(package
 (name bap-ghidra)
 (synopsis "The BAP NSA Ghidra backend")
 (tags (bap ghidra disassembler))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ppx_bap (= :version))
  (bap-common (= :version))
  (bap-std (= :version))
  (bap-main (= :version))))

(package
 (name bap-glibc-runtime)
 (synopsis "The BAP Glibc Runtime support package")
 (tags (bap bap-plugin))
 (depends
  (bap-abi (= :version))
  (bap-c (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ogre (= :version))))

(package (name bap-ida)
 (synopsis "BAP IDA Pro integration")
 (tags (bap bap-library ida-pro))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (regular (= :version))
   fileutils
  (bap-common (= :version))
  (bap-std (= :version))
  (re (and (>= 1.0) (< 2.0)))))

(package
 (name bap-ida-plugin)
 (synopsis "BAP IDA Pro integration")
 (tags (bap bap-plugin bap-ida ida-pro))
 (depends
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-knowledge)
 (synopsis "Knowledge Reasoning and Representation Framework")
 (tags (bap bap-library knowledge))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ppx_bap (= :version))
  (bap-common (= :version))
  (monads (= :version))))

(package
 (name bap-llvm)
 (synopsis "BAP LLVM backend")
 (tags (bap bap-plugin llvm))
 (depends
  (bap-std (= :version))
  (conf-bap-llvm (>= 1.8))
  (core_kernel (and (>= v0.14) (< v0.16)))
   mmap
  (monads (= :version))
  (bap-common (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-main)
 (synopsis "The BAP Framework Configuration Library")
 (tags (bap bap-library))
 (depends
  (bap-build (= :version))
  (bap-future (= :version))
  (bap-plugins (= :version))
  (bap-recipe (= :version))
  (base (and (>= v0.14) (< v0.16)))
  (cmdliner (and (>= 1.0) (< 2.0)))
  (bap-common (= :version))
  (stdio (and (>= v0.14) (< v0.16)))))

(package
 (name bap-mc)
 (synopsis "BAP Machine Code Playground (BAP's llvm-mc)")
 (tags (bap bap-command))
 (depends
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-microx)
 (synopsis "BAP Legacy Microexecution Framework")
 (tags (bap bap-plugin))
 (depends
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (bap-common (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-mips)
 (synopsis "BAP MIPS Semantics")
 (tags (bap bap-plugin mips))
 (depends
  (bap-abi (= :version))
  (bap-api (= :version))
  (bap-core-theory (= :version))
  (bap-c (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))
  (regular (= :version))
   zarith))

(package
 (name bap-objdump)
 (synopsis "Extracts symbols from a binary using binutils objdump")
 (tags (bap bap-plugin))
 (depends
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-relation (= :version))
  (bap-std (= :version))
  (bitvec-order (= :version))
  (bitvec-sexp (= :version))
  (bitvec (= :version))
  (conf-binutils (>= 0.3))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (re (and (>= 1.0) (< 2.0)))))

(package
 (name bap-optimization)
 (synopsis "A BAP IR optimization pass")
 (tags (bap bap-plugin bap-pass))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (bap-std (= :version))
  (regular (= :version))
  (graphlib (= :version))))

(package
 (name bap-patterns)
 (synopsis "Applies semantic actions to the matching byte patterns")
 (tags (bap bap-plugin bap-command))
 (depends
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-primus (= :version))
  (bap-relation (= :version))
  (bap-std (= :version))
  (bitvec-binprot (= :version))
  (bitvec-order (= :version))
  (bitvec-sexp (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
   fileutils
  (bap-common (= :version))
  (ppx_bap (= :version))
  (uri (>= 4.2.0))
  (xmlm (>= 1.3.0))))

(package
 (name bap-phoenix)
 (synopsis "Dumps BAP project in the phoenix format")
 (tags (bap bap-plugin))
 (depends
  (bap-common (= :version))
  (bap-std (= :version))
   cmdliner
  (cmdliner (and (>= 1.0) (< 2.0)))
  (core_kernel (and (>= v0.14) (< v0.16)))
   ezjsonm
  (graphlib (= :version))
  (ppx_bap (= :version))
  (regular (= :version))
  (text-tags (= :version))))

(package
 (name bap-piqi)
 (synopsis "Serializes BAP Project in various formats using piqi")
 (tags (bap bap-plugin))
 (depends
  (bap-common (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (piqi (>= 0.7.8))))

(package
 (name bap-plugins)
 (synopsis "BAP Plugin management library")
 (tags (bap bap-library))
 (depends
  (bap-common (= :version))
  (bap-bundle (= :version))
  (bap-future (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
   fileutils
   ocamlfind
  (ppx_bap (= :version))
   uri))

(package
 (name bap-powerpc)
 (synopsis "BAP PowerPC semantics")
 (tags (bap bap-plugin powerpc))
 (depends
  (bap-abi (= :version))
  (bap-api (= :version))
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-c (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))
  (regular (= :version))
   zarith))

(package
 (name bap-primus)
 (synopsis "BAP Microexecution Framework")
 (tags (bap bap-library bap-primus))
 (depends
  (bap-abi (= :version))
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-c (= :version))
  (bap-future (= :version))
  (bap-knowledge (= :version))
  (bap-std (= :version))
  (bap-strings (= :version))
  (bitvec-binprot (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (graphlib (= :version))
  (monads (= :version))
  (parsexp (and (>= v0.14) (< v0.16)))
  (ppx_bap (= :version))
  (regular (= :version))
  (uuidm (>= 0.9.7))))

(package
 (name bap-primus-dictionary)
 (synopsis "Provides dictionary data structure to Primus Lisp")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-primus-exploring-scheduler)
 (synopsis "A BAP Primus scheduler")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-greedy-scheduler)
 (synopsis "A BAP Primus scheduler")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-limit)
 (synopsis "Enables BAP Primus interpreter termination ")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-primus-lisp)
 (synopsis "BAP Primus Lisp runtime")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (regular (= :version))))

(package
 (name bap-primus-loader)
 (synopsis "BAP Primus loader")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ogre (= :version))))

(package
 (name bap-primus-mark-visited)
 (synopsis "A BAP Primus coverage tracker")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus-track-visited (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-primus-powerpc)
 (synopsis "A BAP Primus PowerPC support module")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-primus-print)
 (synopsis "A BAP Primus state and observations printer")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-future (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (bare (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-promiscuous)
 (synopsis "A BAP Primus promiscuous executor")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-propagate-taint)
 (synopsis "A BAP Primus legacy taint engine compatibility layer")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (bap-taint (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-random)
 (synopsis "A BAP Primus value randomizer")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-main (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (bitvec-sexp (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
   zarith))

(package
 (name bap-primus-region)
 (synopsis "A BAP Primus Lisp regions library")
 (tags (bap bap-primus bap-primus-lisp))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-round-robin-scheduler)
 (synopsis "A BAP Primus scheduler")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-symbolic-executor)
 (synopsis "A BAP Primus symbolic executor")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-primus-track-visited (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (regular (= :version))
  (bitvec (= :version))
   zarith
  (z3 (and (>= 4.8.8-1) (< 4.8.13)))))

(package
 (name bap-primus-systems)
 (synopsis "A BAP Primus systems repository")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-taint)
 (synopsis "A BAP Primus taint analysis framework")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-taint (= :version))))

(package
 (name bap-primus-test)
 (synopsis "A BAP Primus testing and program verification framework")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-primus-track-visited)
 (synopsis "A BAP Primus coverage tracker library")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-primus-wandering-scheduler)
 (synopsis "A BAP Primus scheduler")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))))

(package
 (name bap-primus-x86)
 (synopsis "A BAP Primus X86 support package")
 (tags (bap bap-primus))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-x86 (= :version))))

(package
 (name bap-print)
 (synopsis "Prints BAP project in various human-readable formats")
 (tags (bap bap-plugin))
 (depends
  (bap-common (= :version))
  (bap-demangle (= :version))
  (bap-knowledge (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (graphlib (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))
  (re (and (>= 1.0) (< 2.0)))
  (regular (= :version))
  (text-tags (= :version))))

(package
 (name bap-radare2)
 (synopsis "BAP Radare2 integration")
 (tags (bap bap-plugin radare2))
 (depends
  (bap-abi (= :version))
  (bap-arm (= :version))
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-future (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-relation (= :version))
  (bap-std (= :version))
  (bitvec (= :version))
   conf-radare2
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ogre (= :version))
  (ppx_bap (= :version))
  (re (and (>= 1.0) (< 2.0)))
   yojson
   zarith))

(package
 (name bap-raw)
 (synopsis "BAP raw files loader")
 (tags (bap bap-plugin))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))
  (bap-std (= :version))
  (bap-main (= :version))))

(package
 (name bap-recipe)
 (synopsis "Bundles together command-line options and resources")
 (description
   "A recipe is either a single file or a directory (optionally zipped) \
that contains a parametrized specification of command-line parameters \
and support files if necessary.")
 (tags (bap bap-tool command-line))
 (depends
  (bap-common (= :version))
  (parsexp (and (>= v0.14) (< v0.16)))
  (stdio (and (>= v0.14) (< v0.16)))
   stdlib-shims
  (uuidm (>= 0.9.7))))

(package
 (name bap-recipe-command)
 (synopsis "Manages BAP recipes")
 (tags (bap bap-command bap-tool))
 (depends
  (bap-common (= :version))
  (bap-main (= :version))
  (bap-recipe (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-relation)
 (synopsis "A set of relations (bimap) ")
 (tags (bap data-structure))
 (description "\
A relation between two sets is a set of pairs made from the \
elements of these sets. This library implements a bidirectional mapping \
between two sets and computes their matching that defines bijections \
between the sets.")
 (depends (bap-common (= :version))))

(package
 (name bap-relocatable)
 (synopsis "BAP plugin that manages symbol relocations")
 (tags (bap bap-plugin))
 (depends
  (bap-abi (= :version))
  (bap-arm (= :version))
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-powerpc (= :version))
  (bap-relation (= :version))
  (bap-std (= :version))
  (bap-x86 (= :version))
  (bitvec-order (= :version))
  (bitvec-sexp (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-report)
 (synopsis "BAP plugin for reporting progress bars and statuses")
 (tags (bap bap-plugin))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (bap-std (= :version))))

(package
 (name bap-riscv)
 (synopsis "BAP RISCV support package")
 (tags (bap bap-plugin riscv))
 (depends
  (bap-abi (= :version))
  (bap-api (= :version))
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-c (= :version))
  (bap-knowledge (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (ogre (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-run)
 (synopsis "BAP Primus Framework frontend")
 (tags (bap bap-plugin))
 (depends
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (graphlib (= :version))
  (monads (= :version))
  (regular (= :version))))

(package
 (name bap-specification)
 (synopsis "Extracts meta information from binaries")
 (tags (bap bap-command))
 (depends
  (bap-common (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ogre (= :version))))

(package
 (name bap-ssa)
 (synopsis "Translates program into the SSA form")
 (tags (bap bap-plugin))
 (depends
  (bap-common (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-std)
 (synopsis "BAP Standard Library")
 (tags (bap bap-library))
 (depends
  (bap-common (= :version))
  (bap-bundle (= :version))
  (bap-core-theory (= :version))
  (bap-future (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-plugins (= :version))
  (bap-relation (= :version))
   base-unix
  (bitvec-order (= :version))
  (bitvec (= :version))
  (camlzip (and (>= 1.07) (< 2.0)))
   cmdliner
  (core_kernel (and (>= v0.14) (< v0.16)))
   fileutils
  (graphlib (= :version))
   mmap
  (monads (= :version))
  (ocamlfind (and (>= 1.5.6) (< 2.0.0)))
  (ogre (= :version))
  ounit
  (ppx_bap (= :version))
  (regular (= :version))
   result
   uri
  (utop (>= 2.0.0))
   uuidm
   zarith))

(package
 (name bap-strings)
 (synopsis "Text utilities useful in Binary Analysis and Reverse Engineering")
 (tags (bap bap-library))
 (description "\
The library provides several algorithms:

- Detector - that uses a maximum aposteriori likelihood estimator
  (MAP) to detect code that operates with textual data (aka Bayesian
  inference).

- Unscrambler - that is capable of finding all possible words, that
  can be built from a bag of letters in O(1).

- Scanner - a generic algorithm for finding strings of characters (a
  library variant of strings tool)")
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-beagle-strings)
 (synopsis "Finds strings in binaries using microexecution")
 (tags (bap bap-plugin))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (bap-beagle (= :version))
  (ppx_bap (= :version))
  (bap-std (= :version))
  (bap-strings (= :version))
  (regular (= :version))))

(package
 (name bap-stub-resolver)
 (synopsis "Relinks stubs to their implementations")
 (tags (bap bap-plugin))
 (depends
  (bap-abi (= :version))
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-relation (= :version))
  (bap-std (= :version))
  (bitvec-order (= :version))
  (bitvec-sexp (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ogre (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-symbol-reader)
 (synopsis "Reads symbol information from a file")
 (tags (bap bap-plugin))
 (depends
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-relation (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))))

(package
 (name bap-systemz)
 (synopsis "BAP Systemz support package")
 (tags (bap bap-plugins))
 (depends
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ogre (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-taint)
 (synopsis "BAP Taint Analysis Framework")
 (tags (bap bap-library))
 (depends
  (bap-common (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (bap-strings (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (monads (= :version))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-taint-propagator)
 (synopsis "BAP Legacy Taint propagation engine")
 (tags (bap bap-plugin))
 (depends
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-microx (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (graphlib (= :version))
  (monads (= :version))
  (regular (= :version))))

(package
 (name bap-term-mapper)
 (synopsis "BAP program transformation DSL")
 (tags (bap bap-plugin))
 (depends
  (bap-bml (= :version))
  (bap-common (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name bap-thumb)
 (synopsis "BAP Thumb support package")
 (tags (bap bap-plugin))
 (depends
  (bap-arm (= :version))
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-knowledge (= :version))
  (bap-main (= :version))
  (bap-std (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ogre (= :version))
  (ppx_bap (= :version))))

(package
 (name bap-toplevel)
 (synopsis "BAP toplevel, baptop")
 (tags (bap toplevel))
 (allow_empty)
 (description "\
The utop-enhanced bap toplevel interpreter. \
Run `baptop` and start using bap right away. Or use it \
as the interpreter for your BAP scripts, e.g., `baptop myprog.ml`")
 (depends
  (bap-common (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  ocamlfind))

(package
 (name bap-trace)
 (synopsis "BAP plugin for loading and inspecting execution traces")
 (tags (bap bap-plugin))
 (depends
  (bap-common (= :version))
  (bap-future (= :version))
  (bap-plugins (= :version))
  (bap-std (= :version))
  (bap-traces (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ppx_bap (= :version))
  (regular (= :version))
   uri))

(package
 (name bap-traces)
 (synopsis "BAP execution traces abstractions")
 (tags (bap bap-library))
 (depends
  (bap-common (= :version))
  (bap-std (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ppx_bap (= :version))
  (regular (= :version))
  (uri (>= 1.9.0))
   uuidm))

(package
 (name bap-trivial-condition-form)
 (synopsis "Eliminates complex branch conditions")
 (tags (bap bap-plugin))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (bap-std (= :version))))

(package
 (name bap-warn-unused)
 (synopsis "Warns if an unused argument may cause an issue")
 (tags (bap bap-plugin))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (bap-std (= :version))))

(package
 (name bap-x86)
 (deprecated_package_names bap-x86-cpu)
 (synopsis "BAP x86/x86-64 support package")
 (tags (bap bap-plugin bap-library x86 x86-64))
 (depends
  (bap-abi (= :version))
  (bap-api (= :version))
  (bap-common (= :version))
  (bap-core-theory (= :version))
  (bap-c (= :version))
  (bap-demangle (= :version))
  (bap-future (= :version))
  (bap-knowledge (= :version))
  (bap-llvm (= :version))
  (bap-main (= :version))
  (bap-primus (= :version))
  (bap-std (= :version))
  (bitvec (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ogre (= :version))
  (ppx_bap (= :version))
  (regular (= :version))
   zarith))

(package
 (name bare)
 (synopsis "BAP Rule Engine")
 (description "\
BARE is a library that provides the non-linear pattern matching on streams \
of facts that are represented as s-expressions.")
 (tags (bap bap-library))
 (depends
  (bap-common (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (parsexp (and (>= v0.14) (< v0.16)))))

(package
 (name bitvec)
 (synopsis "Fixed-size bitvectors and modular arithmetic, based on Zarith")
 (tags (bap bap-library))
 (depends (zarith (>= 1.4))))

(package
 (name bitvec-binprot)
 (synopsis "Janestreet's Binprot serialization for Bitvec")
 (tags (bap))
 (depends
  (bin_prot (and (>= v0.14) (< v0.16)))
  (bitvec (= :version))
  (ppx_bap (= :version))))

(package
 (name bitvec-order)
 (synopsis "The Base-compatible comparators and orders for Bitvec ")
 (tags (bap))
 (depends
  (base (>= v0.14))
  (bitvec-sexp (= :version))
  (bitvec (= :version))))

(package
 (name bitvec-sexp)
 (synopsis "Sexp serializers for Bitvec")
 (tags (bap sexp))
 (depends
  (sexplib0 (>= v0.14))
  (bitvec (= :version))))

(package
 (name graphlib)
 (synopsis "Generic graph algorithms")
 (description "\
Graphlib is a generic library that extends the well-known OCamlGraph \
library. Graphlib uses its own, richer, Graph interface that \
is isomorphic to OCamlGraph's `Sigs.P` signature for persistant \
graphs. Two functors witness the isomorphism of the interfaces: \
`Graphlib.To_ocamlgraph` and `Graphlib.Of_ocamlgraph`. Thanks to \
these functors, any algorithm written for OCamlGraph can be used on \
`Graphlibs` graph and vice verse.")
 (tags (bap graph))
 (depends
  (bap-common (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
   ocamlgraph
  (ppx_bap (= :version))
  (regular (= :version))))

(package
 (name monads)
 (synopsis "Monads and Monad Transfomers")
 (description "Provides monad transformers for common monads.")
 (tags (bap monad))
 (depends
  (bap-common (= :version))
  (core_kernel (and (>= v0.14) (< v0.16)))
  (ppx_bap (= :version))))

(package
 (name ogre)
 (synopsis "Pure OCaml Open Generic REpresentation NoSQL Database")
 (description "\
OGRE is a NoSQL document-style database that uses sexp for data \
representation and provides a type safe monadic interface for quering \
and updating documents")
 (tags (bap sexp))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (monads (= :version))))

(package
 (name ppx_bap)
 (synopsis "BAP-blessed ppx rewriters")
 (tags (bap ppx))
 (depends
  (ppx_assert (and (>= v0.14) (< v0.16)))
  (ppx_bench (and (>= v0.14) (< v0.16)))
  (ppx_bin_prot (and (>= v0.14) (< v0.16)))
  (ppx_cold (and (>= v0.14) (< v0.16)))
  (ppx_compare (and (>= v0.14) (< v0.16)))
  (ppx_enumerate (and (>= v0.14) (< v0.16)))
  (ppx_fields_conv (and (>= v0.14) (< v0.16)))
  (ppx_hash (and (>= v0.14) (< v0.16)))
  (ppx_here (and (>= v0.14) (< v0.16)))
  (ppxlib (>= 0.15.0))
  (ppx_optcomp (and (>= v0.14) (< v0.16)))
  (ppx_sexp_conv (and (>= v0.14) (< v0.16)))
  (ppx_sexp_value (and (>= v0.14) (< v0.16)))
  (ppx_variants_conv (and (>= v0.14) (< v0.16)))))

(package
 (name regular)
 (synopsis "Regular Data Types")
 (tags (bap data-structure))
 (description "\
Provides functors and typeclasses implementing functionality expected \
for a regular data type, like i/o, containers, printing, etc. \
 \
In particular, the library includes: \
 \
- module Data that adds generic i/o routines for each regular data type. \
- module Cache that adds caching service for data types \
- module Regular that glues everything together \
- module Opaque for regular but opaque data types \
- module Seq that extends Core_kernel's sequence module \
- module Bytes that provides a rich core-like interface for Bytes data type.")
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))
  (ppx_bap (= :version))))

(package
 (name text-tags)
 (synopsis "A library for rich formatting using semantic tags")
 (tags (bap))
 (depends
  (core_kernel (and (>= v0.14) (< v0.16)))
  (bap-common (= :version))))

(generate_opam_files true)
