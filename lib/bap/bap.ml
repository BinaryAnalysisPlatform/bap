(** Binary Analysis Platform Library Index.

    This is the "entry point" to the BAP library. Here you will
    find a list of modules, as well as redirection to other subparts
    of the library. This module can be viewed as library index.

    BAP has a layered architecture currently consisting of four
    layers:
    [Types] - an extension of standard library, adding some types
              specific to binary analysis.
    [Image] - a memory model, giving a consistent view on a target
              of binary analysis.
    [Disasm] - a first step of program structure reconstruction,
              representing a program as a graph of blocks of
              instructions.
    [Sema]  - provides semantic analysis.

    Each library resides in each own subfolder, named correspondingly.

    Note: this module not only gathers different layers of BAP under one
    umbrella, but also introduces module aliases, like:
    [module Block = Bap_disasm_block]. This ugly name mangling is used
    to portability and interoperability with other libraries in complex
    environments like [opam]. This ugly implementation detail
    shouldn't bother you, as all modules are aliased to the unprefixed
    form. And you should never use or refer to a module using its
    mangled name. *)

(** {2 Main Module}  *)
module Std = struct

  (** {3 Bap basic types}
      This is the most basic part of the library that defines common
      types and operations, like [arch], [addr], [word] and so on.
      See [Bap_types.Std] for more information. And remember that this
      module is literaly included into your namespace after you have
      opened [Bap.Std]
  *)
  include Bap_types.Std

  (** {3 Image aka Memory model}
      This library provides types that represents program as a memory
      object. This includes:

      - [mem] - a contiguous array of bytes, indexed with absolute
                addresses.
      - [table] - a mapping from a memory region to arbitrary entities.
      - [image] - a program loaded to memory, as it is viewed by
                system loader and OS.
      - [symbol] - some named (not strictly contiguous) part of the
                image
      - [section] - some contiguous part of the [image] that has
                specific access flags.

      Proceed to [Bap_image] module for more information.
  *)
  include Bap_image_std

  (** {3 Disassembler}

      This program tries to reconstruct the program structure.
      It includes two interfaces. An expert interface, that provides
      access to a low-level representation. And a normal one, that
      hides all the complexities of the expert interface, but is
      less efficient.

      This library brings to scope the following types:

      - [disasm] the result of disassembling. Not a program yet, but
                 quite near;
      - [insn] a machine instruction as a first class value. BIL
                 included!
      - [op] instruction operand;
      - [reg], [imm], [fmm] instruction operand types, correspondingly
        register, immediate, floating point immediate.
      - [block] a basic block as a part of the control flow graph.

      Proceed to [Bap_disasm], [Bap_disasm_block] and
      [Bap_disasm_insn] module for more information.

      {4 Architecture specific libraries}

      Also, this library provide architecture specific libraries, that
      tries to represent structurally each particular
      architecture. Currently we have only [Arm] module, that has
      type definitions for each instructions and all other
      platform-specific stuff. There is a believe, that one shouldn't
      need this for program analysis. But you may still find it usefull.
  *)
  include Bap_disasm_std

  (** {3 Sematic analysis}   *)
  include Bap_sema.Std

  (** {3 Auxiliary libraries} *)

  (** {3 Project}
      A big view on a dissassembled binary  *)
  module Project = Bap_project

  (** {4 Dwarf library}
      This library gives an access to debugging information stored
      in a binary program.  *)
  module Dwarf = Bap_dwarf

  (** {4 Elf library}
      Provides an access to [ELF] information.  *)
  module Elf = Bap_elf
  type elf = Elf.t

  (** {4 Binary Signatures Storage}  *)
  module Signatures = Bap_signatures

  (** {4 Byteweight Algorithm implementation}  *)
  module Byteweight = Bap_byteweight
end
