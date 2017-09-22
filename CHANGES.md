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
- PR#630 Enhancments in IDA plugin


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
- The disassembler layer is severly rewritten
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

0.9.9
=====

1. Graphlib library

   `Graphlib` is a generic library that extends a well known
   OCamlGraph library. `Graphlib` uses its own, more reach and modern,
   `Graph` interface that is isomorphic to OCamlGraph's `Sigs.P`
   signature for persistant graphs. This interface is developed
   according to the Janestreet's style guidlines and depends on
   Core_kernel library.  Other than the new interface, `Graphlib`
   provides several graph implementations, and generic algorithms.  To
   make our algorithms polymorphic over chosen graph representation we
   use first-class modules, instead of functors, that makes library
   syntactically more light weight.

2. Refined IR.

   phi and arg terms were refined. a phi term now is a discriminated
   set of expressions, and arguments are made more like a defintions.

3. SSA form

   A transformation to a semipruned SSA form was added to a library

4. Changed Var interface.

   Variables can now be versioned, that plays well with SSA form. A
   version is used to represent the same variable but at different
   point of time.

5. Extended and fixed Trie data structure

   Several bug-fixes to a Trie data structure were made. Added
   `walk` function, that allows to perform generic queries over a trie.
   Also added prefix and suffix variants of String tries, as well as
   provided a functor to create tries with an array keys.

6. Redesigned `abi` interface.

   Now `abi` works on an IR level.

7. Added argument reconstruction algorithms.

   Two argument reconstructions algorithm, based on a new `abi`
   interface, were added - for ARM gnueabi ABI and System V amd64 ABI.
   Currently only parameters passed via registers are reconstructed.

8. Added `free_vars` algorithm to BIL and BIR terms.

0.9.8
=====

1. BAP IR is introduced

   BAP Intermediate Representation is based on BAP Instruction
   Language and is a semigraphical representation of a program.

   See documentation and following PR's for more information.
   a2a4621df7c5b25d85c04665732423992e8def98
   74cdee48818225e8b43d39803c97471903ef6d1f

2. Refactored structure of the Project
   Module `Project` now a proper entry point to the library.
   Many stuff from bap utility moved there.
   See 96bd334a0d8af17a6dfd21eff9ec710d448f13e8 for more details.

   This is a breaking change. It hides `project` record and removes
   access to some information, that was previously marked as deprecated:
   - symbols as a mapping from memory to string
   - base as a memory.

   Instead of old symbols table we now have a better interface, see
   below. Instead of base, we now represent all memory as an interval
   map (Memmap).


3. New model for symbols

   Previosly symbols were modeled as contiguous chunk of memory,
   marked with name. Moreover, data sharing between different symbols
   weren't allowed. Since this release, symbols can be a noncontiguous,
   and share data. A new interface is implemented in `Symtab` module.

4. Plugins dependency and autoloading

   Plugins now can now specify dependencies to other plugins, that may
   be auto-loaded by the library.
   See db2a175ba8e6708753a06a2428940c857a1910ec

5. Extended BIL helpers
   See 65f472c08d27020a6570b7992b93397346251d1e

6. Exposed ELF library

7. Fixed segment/section/region name hell
   See 9a574498392c6a13606c9d202037daf137bb780c

8. New universal values library

   The library is based on Core_kernel's Univ, but with addition of
   serialization, comparison and pretty-printing.
   See 383003d60baa3434dd4cd8c894e1d8c2e889b4a2

9. Added bap-fsi-benchmark utility

   80382114f395bcf45925ae2e4bc5b9aac5bba4e7

10. Fixed BIL piqi serialization

   2a5c4671468c5a2699b6007a8af3fda8867e8eb8

11. Fixed installation on more recent ubuntu

  By defaulting LLVM version to 3.4 (and more clever
  searching procedure)

12. Lot's of bugfixes and small extensions

0.9.7
=====

1. BAP structure is refactored

   Complex hierarchy is now hidden under one umbrella `bap.mli`.
   `Bap_*` modules are marked as internal and is no more installable
   and, thus, they do not polute the namespace. This will of course,
   break the code that used this internal modules. It is intended
   behavior.

2. New documentation generator

   `camlp4` messes with `mli` files, rendering autogenerated API
   unreadable. To mitigate this, a small `bapdoc` tool was written
   that preprocess file and removes all syntax extensions, and then
   run `ocamldoc` on a preprocessed file. The tool was integrated
   with build system.

3. Disabled peer checking when downloading signatures

4. Fixed assembly pretty-printing

5. Fixed reading PE-64

6. Fixed `Block.dfs` exhaustiveness issue.
   Also `Block.dfs` now has an `order` parameter, allowing to
   choose between post-order and pre-order traversal.

7. Fixed `to_graph` issue, i.e., #181

8. Fixed `bapbuild` double linking, see #193

9. Ordered block destinations, see #191

10. Fixed an issue in x86 lifter, see #198

11. Fixed interaction with IDA, see #189



0.9.6
=====

1. New loader backed with LLVM
   BAP now have another loader (image reader), that
   supports MACH-O, ELF, COFF, PE. This loader is
   backed with LLVM library.

2. Online plugin system

   New extension point is added - "bap.project". Plugins marked with
   this plugin system will not be loaded automatically when
   `Plugins.load` is called, instead, they can be loaded dynamically
   (or online, hence the title), by using `-l` option to the `bap`
   utility. After being loaded the plugin is applied to a `project`
   data structure that contains all information about disassembled
   binary. Plugin can functionally update this data structure, to
   push information to other plugins or back to the `bap` utility.

   In addition to a common way of creating plugins with `oasis`, we
   extended `bapbuild` utility with a new rule the will product a
   `plugin` file. This is just a shared library underneath the hood,
   and you can load a plugin, created with this method directly,
   without installing it anywhere. `bap` utility will try to find the
   plugin, specified with `-l` option in a current folder, then in all
   folders specified in `BAP_PLUGIN_PATH` environment variable, and,
   finally in the system, using `ocamlfind`.

   In order to provide a typesafe way of interacting between plugins,
   we added extensible variants to BAP. But instead of using one from
   the 4.02, we're using universal types, based on that one, that Core
   library provides. First of all this is more portable, second it is
   more explicit and a little bit more safe.

3. New ABI and CPU interfaces

   Modules that implements `CPU` interface are used to describe
   particular CPU in BIL terminology, e.g., it tells which variable
   corresponds to which register, flag, etc. To obtain such module,
   one should use `target_of_cpu` function.

   ABI is used to capture the procedure abstraction, starting from
   calling convetions and stack frame structure and ending with special
   function handling and support for different data-types.

   See d5cab1a5e122719b4a3b1ece2b1bc44f3f93095a for more information
   and examples.

4. Bap-objdump renamed to bap

   bap-objdump has outgrown its name. Actually it was never really a
   bap-objdump at all. From now, it is just an entry point to the `bap` as
   platform. We will later unite `bap` with other utilities, to make them
   subcommands, e.g. `bap byteweight`.

5. Cleanup of BIL modules

   Now there is a separation between BIL fur uns, and BIL fur
   OCaml. For writing BIL programs (as EDSL in OCaml) one should use
   `Bil` module, e.g. `Bil.(x = y)` will evaluate to a BIL
   expression. For using BIL entities as OCaml values, one should use
   corresponding module, e.g. `Exp.(x = y)` will compare to expressions
   and evaluate to a value of type `bool`.

6. Enhanced IDA integration

   IDA intergation is now more robust. We switched to `IDA-32` by default,
   since 64-bit version doesn't support decompiler. Also `bap` utility
   can now output IDA python scripts. And `bap` plugins can annotate project
   with `python` commands, that later will be dumped into the script.

7. In ARM switched to ARMv7 by default
8. Introduce LNF algorithm and Sema library

   A new layer of BAP is started in this release. This would be a third pass
   of decompilation, where the semantic model of program will be built. Currently,
   there is nothing really interesting here, e.g., an implementation of the
   Loop nesting forest, that is not very usable right now. But the next release,
   will be dedicated to this layer. So, stay tuned.

9. Add support for OCamlGraph

   Now we provide a helper utilities for those who would like to use
   ocamlgraph library for analysis.

10. Extended bap-mc utility

   `bap-mc` utility now prints results in plethora of formats,
   including protocol buffers, from the piqi library, that was revived
   by Kenneth Miller.

11. Interval trees, aka memory maps

   For working with arbitrary overlapping memory regions we now have a
   memory map data structure, aka interval trees, segment trees, etc. It
   is based on AVL trees, and performs logarithmic searches.

12. Simplified CI

   We put Travis on a diet. Now only 4 machines with 20 ETA for all test
   suites to pass. (Instead of 8 * 40).


0.9.5
=====

1. removed tag warnings from the ocamlbuild
2. fixed #114
3. moved Bap_plugins out of Bap library
4. plugin library can now load arbitrary files
5. bap-objdump is now pluggable
6. added new extension point in the plugin system
7. updated BAP LICENSE, baptop is now QPLed
8. IDA can now work in a headless mode
9. enhanced symbol resolution algorithm
10. cleaned up image backend interface
11. constraint OPAM file


0.9.4
=====

1. x86 and x86_64 lifter #106
2. New byteweight implementation #99
3. Intra-procedure CFG reconstruction #102
4. IDA integration #103
5. Binary release #108
6. Man pages and documentation #107
7. Unconstraint opam file and extended it with system dependents #109

0.9.3
=====

1. Bitvector (aka Word, aka Addr) now provides all Integer
interface without any monads right at the toplevel of the module.
In other words, now you can write: Word.(x + y).

2. Bitvector.Int is renamed to Bitvector.Int_exn so that it don't
clobber the real Int module

3. All BIL is now consolidated in one module named Bil. This module
contains everything, including constructors for statements, expressions
casts, binary and unary operations. It also includes functional
constructors, that are now written by hand and, thus, don't suffer from
syntactic clashes with keywords. There're also a plenty of other
functions and new operators, available from the new Bap_helpers
module, see later. Old modules, like Expr, Stmt, etc are still
available, they implement Regular interface for corresponding types.

4. New feature: visitor classes to traverse and transform the AST.
Writing a pattern matching code every time you need to traverse or map
the BIL AST is error prone and time-consuming. This visitors, do all the
traversing for you, allowing you to override default behavior. Some
handy algorithms, that use visitors are provided in an internal
Bap_helpers module, that is included into resulting Bil
module. Several optimizations were added to bap-objdump utility, like
constant propogation, inlining, pruning unused variables and resolving
addresses to symbols.

5. Insn interface now provides predicates to query insn classes, this
predicates use BIL if available.

6. Disam interface now provides linear_sweep function.


0.9.2
=====

1. Recursive descent disassembler
2. High-level simple to use interface to BAP
3. New utility `bap-objdump`
4. Enhanced pretty-printing
5. Lots of small fixes and new handy functions
6. Automatically generated documentation.


0.9.1
=====

First release of a new BAP.
