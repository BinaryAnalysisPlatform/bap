open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap_future.Std

module Std : sig

  (** {1 Binary Analysis Platform Library}  *)

  (** {2 Overview}

      BAP has a layered architecture consisting of four
      layers. Although the layers are not really observable from outside
      of the library, they make it easier to learn the library, as
      they introduce new concepts sequentially. On top of this layers,
      the {{!section:project}Project} module is defined, that
      consolidates all information about target of an
      analysis. [Project] module may be viewed as an entry point to
      the library.

      {v
        +-----------------------------------------------------+
        | +--------+   +-----------------------------------+  |
        | |        |   |                                   |  |
        | |        |   |       Foundation Library          |  |
        | |        |   |                                   |  |
        | |        |   +-----------------------------------+  |
        | |   P    |                                          |
        | |        |   +-----------------------------------+  |
        | |   R    |   |                                   |  |
        | |        |   |          Memory Model             |  |
        | |   O    |   |                                   |  |
        | |        |   +-----------------------------------+  |
        | |   J    |                                          |
        | |        |   +-----------------------------------+  |
        | |   E    |   |                                   |  |
        | |        |   |           Disassembly             |  |
        | |   C    |   |                                   |  |
        | |        |   +-----------------------------------+  |
        | |   T    |                                          |
        | |        |   +-----------------------------------+  |
        | |        |   |                                   |  |
        | |        |   |        Semantic Analysis          |  |
        | |        |   |                                   |  |
        | +--------+   +-----------------------------------+  |
        +-----------------------------------------------------+
      v}


      The {{!bfl}Foundation library} defines {{!Bil}BAP Instruction
      language} data types, as well as other useful data structures,
      like {!Value}, {!Trie}, {!Vector}, {!Graph}, etc. The
      {{!section:image}Memory model} layer is responsible for loading
      and parsing binary objects and representing them in computer
      memory. It also defines a few useful data structures that are
      used extensively by later layers, like {!Table} and
      {!Memmap}. The next layer performs
      {{!section:disasm}disassembly} and lifting to BIL. Finally, the
      {{!section:sema}semantic analysis} layer transforms a binary
      into an IR representation, that is suitable for writing analysis.

      Another important point of view is the BAP plugin architecture.
      Similar to GIMP or Frama-C, BAP features a pluggable architecture
      with a number of extension points. For example, even the LLVM
      disassembler is considered a type of plugin.  Currently we
      support three such extension points in BAP:

      - {{!Backend}loaders} - to add new binary object loaders;
      - disassemblers - to add new disassemblers;
      - {{!section:project}program analysis} - to write analysis.

      The latter category of plugins is most widely used. Therefore,
      when we use the term "plugin" without making a distinction, we
      refer to a program analysis plugin. The following figure
      provides an overview of the BAP system.

      {v
        +---------------------------------------------+
        |  +----------------+    +-----------------+  |
        |  |    Loader      |    |  Disassembler   |  |
        |  |    Plugins     |    |    Plugins      |  |
        |  +-------+--------+    +--------+--------+  |
        |          |                      |           |
        |  +-------+----------------------+--------+  |
        |  |                                       |  |
        |  |             BAP Library               |  |
        |  |                                       |  |
        |  +-------+-------------------------------+  |
        |          ^                      ^           |
        |          |                      |           |
        |  +-------+--------+    +--------+--------+  |
        |  |                |    |                 |  |
        |  |  BAP toolkit   |<-->|   BAP Plugins   |  |
        |  |                |    |                 |  |
        |  +----------------+    +-----------------+  |
        +---------------------------------------------+
      v}

      All plugins have full access to the library; an important
      consequence is that they can and should open [Bap.Std]. The BAP
      library uses backend loader and disassembler plugins to provide
      its services. Program analysis plugins are loaded by BAP
      toolkit utilities. These utilities extend plugin functionality
      by providing access to the state of the target of analysis or,
      in our parlance, to the {{!project}project}. (See
      {!section:project}).

      Other than library itself, and the BAP toolkit, there are two
      additional libraries that are bundled with BAP:

      - [bap.plugins] to dynamically load code into BAP;
      - [bap.serialization] to serialize BAP data structures in
        different formats.
  *)


  (** {2:bfl Foundation Library}

      At this layer we define ({{!Bil}Binary Instruction language})
      and few other useful data structures:

      - {{!Arch}arch} - describes computer architecture;
      - {{!Size}size} - word and register sizes;
      - {{!Var}var}  - {{!Bil}BIL} variable;
      - {{!Type}typ} - {{!Bil}BIL} type system;
      - {{!Exp}exp}  - {{!Bil}BIL} expression sub-language;
      - {{!Stmt}stmt} - {{!Bil}BIL} statements;
      - {{!Bitvector}bitvector} - a bitvector data structure
        to represent immediate data, used usually by their aliases
      - {!word} and {!addr};
      - {{!Value}value} - an extensible variant type;
      - {{!Dict}dict} - an extensible record;
      - {{!Vector}vector} - array that can grow;
      - {{!Seq}'a seq} - slightly extended Core [Sequence], aka lazy
        list;
      - {{!Trie}Trie} - prefix trees;
      - {{!Graph}Graph} - graph implementations and library.


      Most of the types implement the {{!Regular}Regular}
      interface. This interface is very similar to Core's
      [Identifiable], and is supposed to represent a type that is as
      common as a built-in type. One should expect to find any
      function that is implemented for such types as [int], [string],
      [char], etc.  Namely, this interface includes:

      - comparison functions: ([<, >, <= , >= , compare, between, ...]);
      - each type defines a polymorphic [Map] with keys of type [t];
      - each type provides a [Set] with values of type [t];
      - hashtable is exposed via [Table] module;
      - hashset is available under [Hash_set] name
      - sexpable and binable interface;
      - [to_string], [str], [pp], [ppo], [pps] functions
      for pretty-printing.

      Most types usually provide much more. For each type, there is a
      module with the same name that implements its interface. For
      example, type [exp] is indeed a type abbreviation for [Exp.t],
      and module [Exp] contains all functions and types related to
      type [exp]. For example, to create a hashtable of statements,
      just type:

      [let table = Stmt.Table.create ()]

      If a type is a variant type (i.e., defines constructors) then for
      each constructor named [Name], there exists a corresponding
      function named [name] that will accept the same number of
      arguments as the arity of the constructor. For example, a
      [Bil.Int] can be constructed with the [Bil.int] function that
      has type [word -> exp]. If a constructor has several arguments
      of the same type we usually disambiguate them with keywords,
      e.g., [Bil.Load of (exp,exp,endian,size)] has function
      {{!Bil.load}Bil.load} with type:
      [mem:exp -> addr:exp -> endian -> size -> exp]

      {3:value Value}

      {{!Value}Universal values} can be viewed as extensible variants on
      steroids. Not only they maybe extended, but they also can be
      serialized, compared with user-defined comparison function and
      even pretty printed.


      {3:dict Dict}

      Like {{!Value}value} is an extensible sum type, {{!Dict}dict}
      can be viewed as extensible product type. Dict is a sequence of
      values of type {!value}, with {{!Value.Tag}tags} used as field
      names. Of course, fields are unique.

      {3:vector Vector}

      Vector is an implementation of C++ STL like vectors with
      logarithmic push back.

      {3:tries Tries}

      The Foundation library also defines a prefix tree data structure
      that proves to be useful for binary analysis applications.
      {{!module:Trie}Trie}s in BAP is a functor that derives a
      polymorphic trie data structure for a given
      {{!modtype:Trie.Key}Key}.

      For convenience we support instantiating tries for most of
      our data structures. For example, {{!Bitvector}Word} has several
      {{!Bitvector.Trie}tries} inside.

      For common strings, there's {!Trie.String}.


  *)

  (** {2:image Memory model}

      This layer provides everything you need to work with binary
      objects:

      - {{!Memory}mem} - a contiguous array of bytes, indexed with
       absolute addresses;

      - {{!Table} 'a table} - a mapping from a memory regions to
       arbitrary data (no duplicates or intersections);

      - {{!Memmap}a memmap} - a mapping from memory region to
        arbitrary data with duplicates and intersections allowed, aka
        segment tree or interval map;

      - {{!Image}image} - represents a binary object with all its
       symbols, segments, sections and other meta information.

      The [Image] module uses the plugin system to load binary
      objects. In order to add new loader, one should implement the
      {{!Backend}Backend.t} loader function and register it with the
      {{!Image.register_backend}Image.register_backend} function. *)


  (** {2:disasm Disassembler}

      This layer consists of disassemblers and lifters. They are
      tightly integrated, but in general we can disassemble all
      supported {{!Arch.t}architectures}. Currently we lift only arm,
      x86 and x86_64.


      There are two interfaces to disassemblers:

      - {{!Disasm}Disasm} - a regular interface that hides all
       complexities, but may not always be very flexible.
      - {{!Disasm_expert}Disasm_expert} - an expert interface that
      provides access to a low-level representation. It is very
      flexible and fast, but harder to use.

      To disassemble files or data with the regular interface, use
      one of the following functions:

      - {{!disassemble}disassemble} - to disassemble a region of
        memory;
      - {{!disassemble_image}disassemble_image} - to disassemble a
        loaded binary object;
      - {{!disassemble_file}disassemble_file} or
        {{!disassemble_file_exn}disassemble_file} - to disassemble
        file.

      All these functions perform disassembly by recursive descent,
      reconstruct the control flow graph, and perform lifting. The
      result of disassembly is represented by the abstract value of
      type {{!Disasm}disasm}. Two main data structures that are used
      to represent disassembled program are:

      - {{!Insn}insn} - a machine instruction;
      - {{!Block}block} - a basic block, i.e., a linear sequence of
        instructions.

      The following figure shows the relationship between basic data
      structures of the disassembled program.

      {v
        +-----------------+
        | +-------------+ |
        | |   disasm    | |
        | +-------------+ |
        |        |        |
        |        | *      |
        | +-------------+ |
        | |    block    | |
        | +-------------+ |
        |        |        |
        |        | *      |
        | +-------------+ |
        | |     insn    | |
        | +-------------+ |
        |        |        |
        |        | *      |
        | +-------------+ |
        | |     stmt    | |
        | +-------------+ |
        +-----------------+
      v}


      A disassembled program is represented as a set of
      interconnected {{!Block}basic blocks}. You can navigate between
      blocks using {{!Block_traverse.succs}Block.succs} and
      {{!Block_traverse.preds}Block.preds} functions, or you can
      transform a set of blocks into a real {{!Block.Cfg}graph} using
      the {{!Block.to_graph}Block.to_graph} function. Sometimes it
      is enough to traverse program using
      {{!Block.dfs}depth-first search}.

      Each block is a container to a sequence of machine
      instructions. It is guaranteed that there's at least one
      instruction in the block, thus the
      {{!Block_accessors.leader}Block.leader} and
      {{!Block_accessors.terminator}Block.terminator} functions are
      total.

      Each {{!Insn}machine instruction} is represented by its
      [opcode], [name] and [array] of operands (these are machine and
      disassembler specific), a set of predicates (describing
      instruction semantics on a very high level), and a sequence of
      {{!Bil}BIL} statements that precisely define the semantics of
      the instruction.

      Modules of type {{!CPU}CPU} provide a high level abstraction of
      the CPU and allow one to reason about instruction semantics
      independently from the target platform. The module type
      {{!Target}Target} brings [CPU] and [ABI] together. To get an
      instance of this module, you can use the
      {{!target_of_arch}target_of_arch} function. For accessing all
      information about target platform, use the following modules
      that expose low-level and platform-specific details:

      - {{!ARM}ARM}
      - {{!IA32}IA32}
      - {{!AMD64}AMD64}


      If you do not need cfg reconstruction, you can use
      {{!linear_sweep}linear_sweep} function to disassemble a given
      memory region. If you need more granularity, then you can use
      the expert interface accordingly:

      - {{!Disasm_expert.Basic}Basic} - provides access to a low-level
        disassembler on top of which all other disassemblers are
        built;
      - {{!Disasm_expert.Recursive}Recursive} - an interface to a
        recursive descent algorithm.
  *)

  (** {2:sema Semantic Analysis}

      On semantic level the disassembled program is lifted into the
      intermediate representation (IR) suitable for writing analysis.

      IR is closely related to BIL. In fact it even reuses expression
      sub-language of BIL. But unlike BIL, IR is flat, (i.e., it
      doesn't contain recursive statements), and unstructured (no
      [while], [if], only jumps). Thus IR is much more low-level, so
      it is harder to read, but easier to analyze programmatically.

      The program in IR is build of terms. In fact the program itself
      is also a term. There're only 7 kinds of terms:

      - {{!Program}program} - the program in whole;
      - {{!Sub}sub} - subroutine;
      - {{!Arg}arg} - subroutine argument;
      - {{!Blk}blk} - basic block;
      - {{!Def}def} - definition of a variable;
      - {{!Phi}phi} - phi-node in the SSA form;
      - {{!Jmp}jmp} - a transfer of control.

      Unlike expressions and statements in BIL, IR's terms are
      {e concrete entities}.  Concrete entity is such entity that can
      change in time and space, as well as come in and out of
      existence.  Contrary, {e abstract entity} is eternal and
      unchangeable.  {e Identity} denotes the sameness of a concrete
      entity as it changes in time.  Abstract entities don't have an
      identity since they are immutable.  Program is built of concrete
      entities called terms.  Terms have {e attributes} that can change in
      time, without affecting the identity of a term.  Attributes are
      abstract entities.  In each particular point of space and time a
      term is represented by a snapshot of all its attributes,
      colloquially called {e value}.  Functions that change the value of a
      term in fact returns a new value with different set of
      attributes.  For example, [def] term has two attributes: left
      hand side (lhs), that associates definition with abstract
      variable, and right hand side (rhs) that associates [def] with
      an abstract expression. Suppose, that the definition was:

      {[
        # let d_1 = Def.create x Bil.(var y + var z);;
        val d_1 : Def.t = 00000001: x := y + z
      ]}

      To change the right hand side of a definition we use
      [Def.with_rhs] that returns the {e same} definition but with
      {e different} value:

      {[
        # let d_2 = Def.with_rhs d_1 Bil.(int Word.b1);;
        val d_2 : Def.t = 00000001: x := true
      ]}

      [d_1] and [d_2] is different values

      {[
        # Def.equal d_1 d_2;;
        - : bool = false
      ]}  of the same term {[
        # Term.same d_1 d_2;;
        - : bool = true
      ]}

      The identity of this terms is denoted by the term identifier
      ([tid]). In the textual representation term identifiers are
      printed as ordinal numbers.

      Terms, can contain other terms. But unlike BIL expressions or
      statements, this relation is not truly recursive, since the
      structure of program term is fixed: [arg], [phi], [def], [jmp]
      are leaf terms; [sub] can only contain [arg]'s or [blk]'s; [blk]
      consists of [phi], [def] and [jmp] sequences of terms, as
      pictured in the figure below.  Although, the term structure is
      closed to changes, you still can extend particular term with
      attributes, using [set_attr] and [get_attr] functions of the
      {{!Term}Term} module. This functions are using {{!Value}extensible
      variant} type to encode attributes.

      {v
        +--------------------------------------------------------+
        |                +-------------------+                   |
        |                |      program      |                   |
        |                +---------+---------+                   |
        |                          |*                            |
        |                +---------+---------+                   |
        |                |        sub        |                   |
        |                +---------+---------+                   |
        |                          |                             |
        |        +-----------------+---------------+             |
        |        |*                                |*            |
        |  +-----+-------+                 +-------+-------+     |
        |  |    arg      |                 |      blk      |     |
        |  +-------------+                 +-------+-------+     |
        |                                          |             |
        |           +---------------+--------------+             |
        |           |*              |*             | *           |
        |     +-----+-----+   +-----+-----+   +----+-----+       |
        |     |    phi    |   |    def    |   |   jmp    |       |
        |     +-----------+   +-----------+   +----------+       |
        +--------------------------------------------------------+
      v}

  *)

  (** {2:project Working with project}

      There're two general approaches to obtain a value of type
      {{!Project}project}:
      - create it manually using one of the [Project.from_*] function;
      - to write a plugin to a [bap] utility

      Although the first approach is simplistic and gives you a full
      control, we still recommend to use the latter, as [bap] utility
      will provide you integration with different tools, like IDA, as
      well as interaction with a user and other plugins.

      To write a program analysis plugin (or pass in short) you need to
      implement a function with one of the following interfaces:

      - [project -> project] and register it with
        {{!Project.register_pass}register_pass};
      - [project -> unit] and register it with
         {{!Project.register_pass'}register_pass'};
      - [string array -> project -> project] and register it with
        {{!Project.register_pass_with_args}register_pass_with_args};
      - [string array -> project -> unit] and register it with
        {{!Project.register_pass_with_args'}register_pass_with_args'}.

      Once loaded from the [bap] utility (see [man bap]) this function
      will be invoked with a value of type {{!Project.t}project} that
      provides access to all information gathered over the binary so
      far. If the registered function returns a non [unit] type, then it
      can functionally update the project state, e.g., add
      annotations, discover new symbols, make corrections, and even
      change the architecture and re-disassemble everything.

      {3 Example}

      The following plugin prints all sections in a file:

      {[
        open Core_kernel.Std
        open Bap.Std
        open Format

        let print_sections p =
          Project.memory p |> Memmap.to_sequence |> Seq.iter ~f:(fun (mem,x) ->
              Option.iter (Value.get Image.section x) ~f:(fun name ->
                  printf "Section: %s@.%a@." name Memory.pp mem))

        let () = Project.register_pass' "print-sections" print_sections
      ]}

      {3 Exchanging information}

      For exchanging information in a type safe manner, we use
      {{!Value}universal values}. Values can be attached to a
      particular memory region, IR terms, or put into the [storage]
      dictionary. For the first case we use the {{!Memmap}memmap} data
      structure.  It is an interval tree containing all the memory
      regions that are used during analysis. For the [storage] we use
      [Dict] data structure.

      {3 Memory marks}

      By default the memory is marked with the following marks:

      - {{!Image.section}section} -- for regions of memory that had a
      particular name in the original binary. For example, in ELF,
      sections have names that annotate a corresponding memory
      region. If project was created from memory object, then the
      overall memory will be marked as a ["bap.user"] section.

      - {{!Image.segment}segment} -- if the binary data was loaded
      from a binary format that contains segments, then the
      corresponding memory regions are be marked. Segments provide
      access to permission information.  *)



  (** {1:api BAP API}  *)

  (** This module refers to an information bundled with an application.
      Use [include Self()] syntax to bring this definitions to the
      scope.

      It is designed to be used inside a plugin, but can be used in
      a standalone program as well (this is usefull, for debugging
      plugins, by running them as a standalone applications).

      If run in a standalone mode, then field [name] would be set to
      [Sys.executable_name] and [argv] to [Sys.argv].
  *)
  module Self() : sig
    (** [name of a plugin]  *)
    val name : string

    (** [version number]  *)
    val version : string

    (** A short, one-line description  *)
    val doc : string


    (** [args name] returns an array of arguments designated for a
        plugin with a given [name].

        The arguments will be extracted from [Sys.argv] array by
        removing all arguments that doesn't start with
        [--name-]. Then, from all command arguments that are left, the
        [--name-] prefix is substituted with [--]. For example, if
        [argv] contained [ [| "bap"; "-lcallgraph"; "--callgraph"
        "--callgraph-help"|]] then pass that registered itself under
        [callgraph] name will receive the following array of arguments
        [ [| "callgraph"; --help |] ]. That means, that plugins can't
        accept arguments that are anonymous or short options *)
    val argv : string array

    val debug   : ('a,Format.formatter,unit) format -> 'a
    val info    : ('a,Format.formatter,unit) format -> 'a
    val warning : ('a,Format.formatter,unit) format -> 'a
    val error   : ('a,Format.formatter,unit) format -> 'a

    (** This module allows plugins to access BAP configuration variables.

        When reading the values for the configuration variables, the
        decreasing order of precedence for the values is:
        - Command line arguments
        - Environment variables
        - Configuration file
        - Default fallback value

        Example usage:

        {[
          let path = Config.(param string ~doc:"a path to file"
                               ~default:"input.txt" ~name:"path")
          let debug = Config.(flag (* ... *) )

          (* ... *)

          let main () =
            let (!) = Config.parse () in
            do_stuff !path !debug (* ... *)
        ]}
    *)
    module Config : sig
      (** Version number  *)
      val version : string

      (** A directory for bap specific read-only architecture
          independent data files.  *)
      val datadir : string

      (** A directory for bap specific object files, libraries, and
          internal binaries that are not intended to be executed directly
          by users or shell scripts *)
      val libdir : string

      (** A directory for bap specific configuration files  *)
      val confdir : string

      (** An abstract parameter type that can be later read using a reader *)
      type 'a param

      (** Parse a string to an 'a *)
      type 'a parser = string -> [ `Ok of 'a | `Error of string ]

      (** Converts an 'a to a string *)
      type 'a printer = Format.formatter -> 'a -> unit

      (** Interconversion between string and 'a type *)
      type 'a converter = 'a parser * 'a printer

      (** Create a parameter *)
      val param :
        'a converter -> default:'a ->
        ?docv:string -> ?doc:string -> name:string -> 'a param

      (** Create a boolean parameter that is set to true if user
          mentions it in the command line arguments *)
      val flag :
        ?docv:string -> ?doc:string -> name:string -> bool param

      (** Reads a value from a parameter *)
      type 'a reader = 'a param -> 'a

      (** Parse command line arguments and return a param reader *)
      val parse : unit -> 'a reader

      (** The type for a block of man page text.

          - [`S s] introduces a new section [s].
          - [`P t] is a new paragraph with text [t].
          - [`Pre t] is a new preformatted paragraph with text [t].
          - [`I (l,t)] is an indented paragraph with label [l] and text [t].
          - [`Noblank] suppresses the blank line introduced between two blocks.

          Except in [`Pre], whitespace and newlines are not significant
          and are all collapsed to a single space. In labels [l] and text
          strings [t], the syntax ["$(i,italic text)"] and ["$(b,bold
          text)"] can be used to respectively produce italic and bold
          text. *)
      type manpage_block = [
        | `I of string * string
        | `Noblank
        | `P of string
        | `Pre of string
        | `S of string
      ]

      (** Create a manpage for the plugin *)
      val manpage : manpage_block list -> unit

      (** [bool] converts values with {!bool_of_string}. *)
      val bool : bool converter

      (** [char] converts values by ensuring the argument has a single char. *)
      val char : char converter

      (** [int] converts values with {!int_of_string}. *)
      val int : int converter

      (** [nativeint] converts values with {!Nativeint.of_string}. *)
      val nativeint : nativeint converter

      (** [int32] converts values with {!Int32.of_string}. *)
      val int32 : int32 converter

      (** [int64] converts values with {!Int64.of_string}. *)
      val int64 : int64 converter

      (** [float] converts values with {!float_of_string}. *)
      val float : float converter

      (** [string] converts values with the identity function. *)
      val string : string converter

      (** [enum l p] converts values such that unambiguous prefixes of
          string names in [l] map to the corresponding value of type ['a].

          {b Warning.} The type ['a] must be comparable with
          {!Pervasives.compare}.

          @raise Invalid_argument if [l] is empty. *)
      val enum : (string * 'a) list -> 'a converter

      (** [file] converts a value with the identity function and
          checks with {!Sys.file_exists} that a file with that name exists. *)
      val file : string converter

      (** [dir] converts a value with the identity function and checks
          with {!Sys.file_exists} and {!Sys.is_directory}
          that a directory with that name exists. *)
      val dir : string converter

      (** [non_dir_file] converts a value with the identity function and checks
          with {!Sys.file_exists} and {!Sys.is_directory}
          that a non directory file with that name exists. *)
      val non_dir_file : string converter

      (** [list sep c] splits the argument at each [sep] (defaults to [','])
          character and converts each substrings with [c]. *)
      val list : ?sep:char -> 'a converter -> 'a list converter

      (** [array sep c] splits the argument at each [sep] (defaults to [','])
          character and converts each substring with [c]. *)
      val array : ?sep:char -> 'a converter -> 'a array converter

      (** [pair sep c0 c1] splits the argument at the {e first} [sep] character
          (defaults to [',']) and respectively converts the substrings with
          [c0] and [c1]. *)
      val pair : ?sep:char -> 'a converter -> 'b converter -> ('a * 'b) converter

      (** {!t2} is {!pair}. *)
      val t2 : ?sep:char -> 'a converter -> 'b converter -> ('a * 'b) converter

      (** [t3 sep c0 c1 c2] splits the argument at the {e first} two [sep]
          characters (defaults to [',']) and respectively converts the
          substrings with [c0], [c1] and [c2]. *)
      val t3 : ?sep:char -> 'a converter -> 'b converter -> 'c converter ->
        ('a * 'b * 'c) converter

      (** [t4 sep c0 c1 c2 c3] splits the argument at the {e first} three [sep]
          characters (defaults to [',']) respectively converts the substrings
          with [c0], [c1], [c2] and [c3]. *)
      val t4 : ?sep:char -> 'a converter -> 'b converter -> 'c converter ->
        'd converter -> ('a * 'b * 'c * 'd) converter

    end

  end

  type 'a printer = Format.formatter -> 'a -> unit

  (** Signature for integral type.  *)
  module type Integer = sig
    type t
    val zero : t
    val one  : t

    val succ : t -> t
    val pred : t -> t
    val abs  : t -> t
    val neg  : t -> t

    val add     : t -> t -> t
    val sub     : t -> t -> t
    val mul     : t -> t -> t
    val div     : t -> t -> t
    val modulo  : t -> t -> t
    val lnot    : t -> t
    val logand  : t -> t -> t
    val logor   : t -> t -> t
    val logxor  : t -> t -> t
    val lshift  : t -> t -> t
    val rshift  : t -> t -> t
    val arshift : t -> t -> t

    (** A common set of infix operators  *)
    val ( ~-)  : t -> t
    val ( + )  : t -> t -> t
    val ( - )  : t -> t -> t
    val ( * )  : t -> t -> t
    val ( / )  : t -> t -> t
    val (mod)  : t -> t -> t
    val (land) : t -> t -> t
    val (lor)  : t -> t -> t
    val (lxor) : t -> t -> t
    val (lsl)  : t -> t -> t
    val (lsr)  : t -> t -> t
    val (asr)  : t -> t -> t
  end


  (** Monad is an interface, that is pervasives through many data
      types. Such types are called monadic. Monad interface is
      practical for both data (list, option, etc), and codata (Future,
      Lwt, Deferred, Lazy, Continuation). The interpretation of each
      monad interface is different for each data type. But in general
      monad interface consists of two functions:
        - [return : 'a -> 'a t]
        - [bind : 'a t -> ('a -> 'b t) -> 'b t].

      This is a minimal definition. Many functions can be inferred
      based on this interface, see [Monad.S] for the full list.


      [Option] monad, as well as [Or_error] and [Result] monads, is suitable
      for building control flow. They can be seen as a reification of
      exception. A chain of such monads link with the [bind] function,
      will break as soon as monadic zero value occurs.

      [Future], as well as [Lwt] or [Async], provides a safe access to
      codata, i.e., to a value that is defined somewhere outside of
      the main program, or to a value that is represented by an
      effectful computation, not by inductive data. [Lwt] and [Async]
      are also so called [IO] monads, that reifies side-effects,
      mostly [IO]. [State] monad is another example of reification of
      computation effect. The [State] monad is useful to build purely
      functional computation with side-effects. The side-effect, e.g.,
      writing to a memory cell, is reified into an OCaml value, and
      practically becomes a first class value, i.e., it can be stored,
      printed, etc.
  *)
  module Monad : sig
    module type Basic = Monad.Basic
    module type Basic2 = Monad.Basic2

    module type Infix = Monad.Infix
    module type Infix2 = Monad.Infix2

    module type S = Monad.S
    module type S2 = Monad.S2

    (** State monad interface.

        State monad is an interface to a computation that has a
        limited side effect. The side effect is defined by the
        following two functions:
        - [put s]  -- that will store value [s]
        - [get ()] -- that will extract the value.


        There're few functions derived on [get] and [put], like
        [gets], [update], [modify], but the minimal algebra is
        [put] and [set].


        The value of type [('a,'s) t] bears a computation that
        has an access to the state of type ['s], and evaluates
        to a value of type ['a]. It can be said, that ['s] reifies
        effect of computation of value ['a].

    *)
    module type State = sig

      (** [('a,'s) t] computation that evaluates to ['a] and state ['s] *)
      type ('a,'s) t

      (** ['a result] is a type that represents result of computation.*)
      type 'a result

      include Monad.S2 with type ('a,'s) t := ('a,'s) t

      (** [put s] creates a computation that has effect [s] and unit
          value.  This operation effectively updates the state by
          overriding the existent one with the new one. An imperative
          counterpart of this operation is a statement that performs
          side effect, e.g., {[put x]} is somewhat equivalent to
          {[let state = ref init
            let put x = state := x
          ]}

          except that [state] is not hidden in the language heap, but is
          reified into a value of type ['s].
      *)
      val put : 's -> (unit,'s) t

      (** [get ()] creates a computation that evalates to a value,
          that holds the state. This operation extracts the state, and
          gives an access for it. The [get ()] is somewhat equivalent
          to the imperative operator [(!)], i.e.,
          {[
            let state = ref init
            let get () = !state
          ]} *)
      val get : unit -> ('s,'s) t

      (** [gets f] is a computation whose value is [f state]) *)
      val gets : ('s -> 'r) -> ('r,'s) t
      (** [update f] is [get () >>= fun s -> put (f s)]  *)
      val update : ('s -> 's) -> (unit,'s) t

      (** [modify c f] is a computation with state [f], where
          [s] is [c >>= get ()], i.e., an effect of computation [c].*)
      val modify : ('a,'s) t -> ('s -> 's) -> ('a,'s) t


      (** Running state computations.  *)

      (** [run c init] runs computation [c] with initial state
          [init]. Result contains a pair of the computation result
          and total effect.  *)
      val run : ('a,'s) t -> 's -> ('a * 's) result

      (** [eval c init] contains a value computed by computation [c]
          under initial state [init].   *)
      val eval : ('a,'s) t -> 's -> 'a result

      (** [exec c init] contains an final effect produced by a
          computation [c] under initial state [init].  *)
      val exec : ('a,'s) t -> 's -> 's result
    end

    module Make(M : Basic) : S with type 'a t := 'a M.t
    module Make2(M : Basic2) : S2 with type ('a,'s) t := ('a,'s) M.t

    (** State monad.
        See {!modtype:State} for more info.
    *)
    module State : State with type 'a result = 'a


    (** Monad transformers.

        [module IO = Monad.T.Inner(Outer)] is a monad
        [Inner] wrapped into monad [Outer]. For example, monad
        [module OLwt = Monad.T.Option(Lwt)] will
        allow to work with optional values in the [Lwt] IO monad,
        e.g.,

        {[
          open OLwt
          let require p = if p then return (Some ()) else return None
          let authenticate user passwd =
            read_credentials >>= fun (u,p) ->
            require (user = u) >>= fun () ->
            require (passwd = p)
        ]}


        Not all monands are transformable into anothers. For example,
        in general it is not possible to wrap IO monad into other monad
        (but it is possible to in the opposite direction). State monad
        here is a special case, although it is very similiar to the IO
        monad it still can be wrapped into some other monad. However,
        this wrapping is more like intersection, as the returned monad
        actually behaves like an intersection of the outer and inner
        monad.
    *)

    module T : sig

      (** Option Monad Transformer  *)
      module Option : sig
        module Make (M : S ) : S  with type 'a t = 'a option M.t
        module Make2(M : S2) : S2 with type ('a,'b) t = ('a option,'b) M.t
      end

      (** Or_error Monad Transformer  *)
      module Or_error : sig
        module Make (M : S ) : S  with type 'a t = 'a Or_error.t M.t
        module Make2(M : S2) : S2 with type ('a,'b) t = ('a Or_error.t,'b) M.t
      end

      (** Result Monad Transformer.

          We do not provide [Make2] because its result would be a
          non-existent [Monad.S3].*)
      module Result : sig
        module Make(M : S) : S2 with type ('a,'e) t = ('a,'e) Result.t M.t
      end

      (** Result Monad Transformer.

          [module STM = Monad.T.State.Make(M)] is not a monad [M], but a
          state monad which build computations that are interleaved
          with computations of monad [M]. For example, let's take
          the [Future] as the outer monad:

          [module STF = Monad.T.State.Make(Future)] will build a
          computation that can access values that are defined in the
          future, as a consequence, [STF.run] result will be a value that
          would be defined in the future. The computation built with
          [STF] monad will not try to access any future values until [run] is
          called. Once it is called, each step of the computation will
          first wait until the future occurs (i.e., future value is
          defined) and then perform update of the state.

          As an another example, given some [IO] monad, a computation
          built with [monad STIO = Monad.T.State.Make(IO)] will not
          perform any I/O until it is actually ran or evaluated. And
          the result of the run would be [('a,'s) IO.t], that actually
          may not perform actual IO (i.e., system calls) until it is
          run (this, of course, depends on the implementation of IO,
          as [Core.Async] will not perform any operations until the
          scheduler is run, but [Lwt] will trigger the I/O and defer
          until it is finished).

          Note: We do not provide [Make2] because its result would be a
          module of non-existent [Monad.S3] type.
      *)
      module State : sig
        module Make(M : S) : State with type 'a result = 'a M.t
      end
    end
  end

  (** Lazy sequence  *)
  module Seq : module type of Seq
    with type 'a t = 'a Sequence.t
  (** type abbreviation for ['a Sequence.t]  *)
  type 'a seq = 'a Seq.t [@@deriving bin_io, compare, sexp]


  (** Prefix tries.

         Trie is a mutable table, that can be seen as a specialized
         form of a hash table.

         Use [Trie.Make] functor to create modules, implementing this
         signature.  Also look at already predefined modules, like
         [String] (see below), [Bitvector.Trie], [Bil.Trie],
         [Insn.Trie], etc.

  *)
  module type Trie = sig
    (** trie can store arbitrary data  *)
    type 'a t [@@deriving bin_io, sexp]

    (** a key type that is used to lookup data  *)
    type key

    (** [create ()] creates new empty trie  *)
    val create : unit -> 'a t

    (** [add trie ~key ~data] adds [data] associated with [key], if
        [trie] already has some data associated with the [key], then
        it will be overwritten *)
    val add : 'a t -> key:key -> data:'a -> unit

    (** [change trie key f] if trie has [data] associated with [key] then
        [f] will be called with [Some data], otherwise it will be called
        with [None]. If [f] returns [None] then there will be no data
        associated with [key], if [f] returns [Some thing], then [thing]
        will be bound to [key] *)
    val change : 'a t -> key -> ('a option -> 'a option) -> unit

    (** [find trie key] finds data associated with [key]  *)
    val find : 'a t -> key -> 'a option

    (** [walk trie key ~init ~f] walks down the tree starting from the
        root and ending with the last token of the key. Function [f]
        is fold over values associated with all substrings of the key,
        starting from a zero substring. *)
    val walk : 'a t -> key -> init:'b -> f:('b -> 'a option -> 'b) -> 'b

    (** [remove trie key] removes value bound with [key] if any.  *)
    val remove : 'a t -> key -> unit

    (** [longest_match trie k] find the value associated with a
        longest substring of a key [k]. Returns a pair - a length of
        matched key and data, associated with that key. *)
    val longest_match : 'a t -> key -> (int * 'a) option

    (** [length trie] returns the amount of entries in the [trie]  *)
    val length : 'a t -> int

    (** [pp pp_val] creates a printer for a given value printer
        [pp_val]. Example:

        [let int_trie = String.Trie.pp pp_int]

        will create a printer for a [String.Trie] that is populated by
        integers.  *)
    val pp : 'a printer -> 'a t printer
  end

  (** Constructs a trie  *)
  module Trie : sig
    (** Key requirements.
        Key is a sequence of tokens of the specified length.
        It is better to use contiguous data structures, like
        arrays as keys, otherwise you can end up with a slow
        implementation (i.e., don't use lists or sequences as
        keys, use strings, bitstrings, arrays, etc). *)
    module type Key = sig
      (** the type of key  *)
      type t

      (** type of token must implement bin_prot, be comparable and
          sexpable *)
      type token [@@deriving bin_io, compare, sexp]

      (** [length key] return the amount of tokens in a [key]  *)
      val length : t -> int

      (** [nth_token key n] the [n]'th token of key. Should be O(1) *)
      val nth_token : t -> int -> token

      (** [hash_token tok] efficient hash function for the [token] type.
          If nothing efficient came to mind, just use [Hashtbl.hash]. *)
      val token_hash : token -> int
    end

    (** Create a trie for a given [Key]  *)
    module Make(Key : Key) : Trie with type key = Key.t

    (** Minimum required interface for a token data type  *)
    module type Token = sig
      type t  [@@deriving bin_io, compare, sexp]
      val hash : t -> int
    end

    (** Prefix and suffix tries for specified token types.  *)
    module Array : sig
      module Prefix(Tok : Token) : Trie with type key = Tok.t array
      module Suffix(Tok : Token) : Trie with type key = Tok.t array
    end

    (** Predefined prefix and suffix string tries.    *)
    module String : sig
      module Prefix : Trie with type key = string
      module Suffix : Trie with type key = string
    end
  end

  (** Type to represent machine word  *)
  type word [@@deriving bin_io, compare, sexp]

  (** A synonym for [word], that should be used for words
      that are addresses  *)
  type addr = word [@@deriving bin_io, compare, sexp]

  (** Type safe operand and register sizes.  *)
  module Size : sig
    (** Defines possible sizes for operations operands  *)
    type all = [
      | `r8
      | `r16
      | `r32
      | `r64
      | `r128
      | `r256
    ] [@@deriving variants]

    type 'a p = 'a constraint 'a = [< all]
                     [@@deriving bin_io, compare, sexp]

    type t = all p
      [@@deriving bin_io, compare, sexp]

    (** {3 Lifting from int} *)

    (** [of_int n] return [Ok `rn] if [`rn] exists, [Error]
        otherwise.  *)
    val of_int : int -> t Or_error.t

    (** [of_int_exn n] the same as [of_int], but raises exception
        instead of returning [Error] *)
    val of_int_exn : int -> t

    (** [of_int_opt n] the same as [of_int] but uses [option] type
        instead of [Or_error.t] *)
    val of_int_opt : int -> t option

    (** [addr_of_int n] return [Ok `rn] if [`rn] exists, [Error]
        otherwise.  *)
    val addr_of_int : int -> [ `r32 | `r64 ] Or_error.t

    (** [addr_of_int_exn n] the same as [addr_of_int], but raises exception
        instead of returning [Error] *)
    val addr_of_int_exn : int -> [ `r32 | `r64 ]

    (** [addr_of_int_opt n] the same as [addr_of_int] but uses [option] type
        instead of [Or_error.t] *)
    val addr_of_int_opt : int -> [ `r32 | `r64 ] option

    val addr_of_word_size : Word_size.t -> [ `r32 | `r64 ]

    val word_of_addr_size : [ `r32 | `r64 ] -> Word_size.t

    val to_addr_size : t -> [ `r32 | `r64 ] Or_error.t

    (** [in_bits size] returns size in bits. *)
    val in_bits  : 'a p -> int

    (** [in_bytes sz] returns size in bytes  *)
    val in_bytes : 'a p -> int

    include Regular with type t := t
  end

  (** size of operand  *)
  type size = Size.t
    [@@deriving bin_io, compare, sexp]

  (** size of address  *)
  type addr_size = [ `r32 | `r64 ] Size.p
    [@@deriving bin_io, compare, sexp]

  (** just a fancy type abbreviation  *)
  type nat1 = int
    [@@deriving bin_io, compare, sexp]


  (** Bitvector -- a type for representing binary values.

      {2 Overview }

      A numeric value with a 2-complement binary representation. It is
      good for representing addresses, offsets and other numeric values.

      Each value is attributed by a its bit-width. All arithmetic
      operations over values are done modulo their widths. It is an
      error to apply arithmetic operation to values with different
      widths. Default implementations will raise a [Width] exception,
      however there exists a family of modules that provide arithmetic
      operations lifted to an [Or_error.t] monad. It is suggested to use
      them, if you know what kind of operands you're expecting.


      {2 Clarifications endianness and bit-ordering }

      Bitvector should be considered as an number with an arbitrary
      width. That means, that as with all numbers it is subject to
      endianness. When we iterate over bitvector using some container
      interface we always start from the byte with the lower
      address. Depending on endianness it will be either least
      significant bytes (little-endian), or most significant
      (big-endian). Sometimes id does matter, sometimes it doesn't. In a
      latter case you can just use a default native-endian
      interface. But in a former case, please consider using explicit
      modules, either [Bytes_LE] or [Bytes_BE], even if you know that
      your system is [LE]. Things change.

      Bits are always numbered from right to left, with least
      significant bit having a zero index, and most significant having
      index equal to [width - 1]. That means, they're endianness
      agnostic.

      {2 Clarification on size-morphism }

      Size-monomorphic operations (as opposed to size-polymorphic
      comparison) doesn't allow to compare two operands with different
      sizes, and either raise exception or return [Error]. If we would
      have type safe interface, with type [t] defined as [type 'a t],
      where ['a] stands for size, then size-monomorphic operations will
      have type ['a t -> 'a t -> _], and size-polymorphic ['a t -> 'b t -> _].

      By default, size-polymorphic comparison is used (for rationale of
      this decision look at the implementation of a hash function). To
      understand the ordering relation one can think that a lexical
      ordering is specified on a tuple [(x,n)], where [x] is the number
      and [n] is the size. For example, the following sequence is in an
      ascending order:

      {[ 0x0:1, 0x0:32, 0x0:64, 0x1:1, 0x1:32, 0xD:4, 0xDEADBEEF:32]}.

      A size-monomorphic interfaced is exposed in a [Mono] submodule. So
      if you want a monomorphic map, then just use [Mono.Map] module.
      Note, [Mono] submodule doesn't provide [Table], since we cannot
      guarantee that all keys in a hash-table have equal size.

      {2 Clarification on signs}

      By default all numbers represented by a bitvector are considered
      unsigned. This includes comparisons, e.g., [of_int (-1) ~width:32]
      is greater than zero. If you need to perform signed operation, you
      can use [signed] operator to temporary cast your value to signed.
      We use temporary to emphasize that, the signedness property won't
      propagate to the result of the operation, e.g. result of the
      following expression: [Int_exn.(signed x / y)] will not be signed.

      If any operand of a binary operation is signed, then a signed
      version of an operation is used.

      Remember to use explicit casts, whenever you really need a signed
      representation. Examples:
      {[
        let x = of_int ~-6 ~width:8
        let y = to_int x          (* y = 250 *)
        let z = to_int (signed x) (* z = ~-6 *)
        let zero = of_int 0 ~width:8
        let p = x < zero          (* p = false *)
        let q = signed x < zero   (* p = true *)
      ]}

      {2 Clarification on string representation }

      As a part of [Identifiable] interface bitvector provides a pair of
      complement functions: [to_string] and [of_string], that provides
      facilities to store bitvector as a human readable string, and to
      restore it from string. The format of the representation is the
      following (in EBNF):
      {[
        repr  = [sign], base, digit, {digit}, ":", size | true | false;
        sign  = "+" | "-";
        base  = "0x" | "0b" | "0o";
        size  = dec, {dec};
        digit = dec | oct | hex;
        dec   = ?decimal digit?;
        oct   = ?octal digit?;
        hex   = ?hexadecimal digit?;
      ]}

      Examples:
      [0x5D:32, 0b0101:16, 5:64, +5:8, +0x5D:16, true, false.].

      Form [false] is a shortcut for [0:1], as well as [true] is [1:1].

      If [base] is omitted base-10 is assumed. The output format is
      lways ["0x", hex, {hex}] in an unsigned form. *)
  module Bitvector : sig

    (** [word] is an abbreviation to [Bitvector.t]  *)
    type t = word

    (** {2 Common Interfaces}

        Bitvector is a value, first of all, so it supports a common set of
        a value interface: it can be stored, compared, it can be a key in
        a dictionary, etc. Moreover, being a number it can be compared
        with zero and applied to a common set of integer operations.
    *)

    (** Bitvector implements a common set of operations that are
        expected from integral values.  *)
    include Regular with type t := t
    include Integer with type t := t

    (** A comparable interface with size-monomorphic comparison. *)
    module Mono : Comparable with type t := t

    (** [Width] exception is raised when size-monomorphic operation is
        applied to operands with different sizes. *)
    exception Width [@@deriving sexp]

    (** Specifies the order of bytes in a word. *)
    type endian =
      | LittleEndian (** least significant byte comes first  *)
      | BigEndian    (** most  significant byte comes first  *)
      [@@deriving bin_io, compare, sexp]

    (** {2 Constructors} *)
    val of_string : string -> t
    val of_bool  : bool -> t
    val of_int   : width:int -> int -> t
    val of_int32 : ?width:int -> int32 -> t
    val of_int64 : ?width:int -> int64 -> t

    (** {2 Some predefined constant constructors }  *)

    (** [b0 = of_bool false] is a zero bit  *)
    val b0 : t
    (** [b1 = of_bool true] is a one bit  *)
    val b1 : t

    (** {2 Helpful shortcuts }  *)

    (** [one width] number one with a specified [width], is a shortcut for
        [of_int 1 ~width]*)
    val one: int -> t
    (** [zero width] zero with a specified [width], is a shortcut for
        [of_int 0 ~width]*)
    val zero: int -> t

    (** [ones width] is a number with a specified [width], and all bits
        set to 1. It is a shortcut for [of_int (lnot 0) ~width]*)
    val ones : int -> t

    (** [of_binary ?width endian num] creates a bitvector from a string
        interpreted as a sequence of bytes in a specified order.

        The result is always positive.

        [num] argument is copied

        [width] defaults to [String.length num]
    *)
    val of_binary : ?width:int -> endian -> string -> t

    (** {2 Conversions to built-in integers }  *)
    val to_int   : t -> int   Or_error.t
    val to_int32 : t -> int32 Or_error.t
    val to_int64 : t -> int64 Or_error.t
    val string_of_value : ?hex:bool -> t -> string

    (** [signed t] casts t to a signed type, so that any operations
        applied on [t] will be signed *)
    val signed : t -> t

    (** [is_zero bv] is true iff all bits are set to zero. *)
    val is_zero : t -> bool

    (** [is_ones bv] is true if the least significant bit is equal to one  *)
    val is_one : t -> bool

    (** [bitwidth bv] return a bit-width, i.e., the amount of bits *)
    val bitwidth : t -> int

    (** [extract bv ~hi ~lo] extracts a subvector from [bv], starting
        from bit [hi] and ending with [lo]. Bits are enumerated from
        right to left (from least significant to most), starting from
        zero. [hi] maybe greater then [size].

        [hi] defaults to [width bv - 1]
        [lo] defaults to [0].

        Example:

        [extract (of_int 17 ~width:8) ~hi:4 ~lo:3]
        will result in a two bit vector consisting of the forth and
        third bits, i.e., equal to a number [2].

        [lo] and [hi] should be non-negative numbers. [lo] must be less
        then a [width bv] and [hi >= lo]. *)
    val extract : ?hi:int -> ?lo:int -> t -> t Or_error.t

    (** [extract_exn bv ~hi ~lo] is the same as [extract], but will raise
        an exception on error.  *)
    val extract_exn : ?hi:int -> ?lo:int -> t -> t

    (** [concat b1 b2] concatenates two bitvectors  *)
    val concat : t -> t -> t

    (** [b1 @. b2] is [concat b1 b2] *)
    val (@.): t -> t -> t

    (** [succ n] returns next value after [n]. Of course it is not
        guaranteed that [succ n > n]*)
    val succ : t -> t

    (** [pred n] returns a value preceding [n]  *)
    val pred : t -> t

    (** [nsucc m n] is [Fn.apply_n_times ~n succ m], but more
        efficient.  *)
    val nsucc : t -> int -> t

    (** [npred m n] is [Fn.apply_n_times ~n pred addr], but more
        efficient.  *)
    val npred : t -> int -> t

    (** [a ++ n] is [nsucc a n]  *)
    val (++) : t -> int -> t

    (** [a -- n] is [npred a n]  *)
    val (--) : t -> int -> t

    (** {2 Iteration over bitvector components }  *)

    (** [enum_bytes x order] returns a sequence of bytes of [x] in a
        specified [order].  Each byte is represented as a [bitvector]
        itself. *)
    val enum_bytes : t -> endian ->    t seq

    (** [enum_bytes x order] returns bytes of [x] in a specified [order],
        with bytes represented by [char] type *)
    val enum_chars : t -> endian -> char seq

    (** [enum_bits x order] returns bits of [x] in a specified [order].
        [order] defines only the ordering of words in a bitvector, bits
        will always be in MSB first order. *)
    val enum_bits  : t -> endian -> bool seq


    (** {3 Comparison with zero}

        Note, we're not including [With_zero] interface, since
        it refers to the `Sign` module, that is available only
        in core_kernel >= 113.33.00.
    *)

    val validate_positive     : t Validate.check
    val validate_non_negative : t Validate.check
    val validate_negative     : t Validate.check
    val validate_non_positive : t Validate.check
    val is_positive     : t -> bool
    val is_non_negative : t -> bool
    val is_negative     : t -> bool
    val is_non_positive : t -> bool


    (** {2 Arithmetic raised into [Or_error] monad }

        All binary integer operations are only well defined on operands
        with equal sizes.

        Module [Int] provides a set of integer operations that do not
        raise exceptions, but return values raised to an Or_error
        monad.

        Example:

        [Z.(i16 v1 + i16 v2 / int 16 v3)],

        or just:

        [Z.(!$v1 + !$v2 / !$v3)]. *)
    module Int_err : sig
      (** [!$v] lifts [v] to an Or_error monad. It is, essentially, the
          same as [Ok v] *)
      val (!$): t -> t Or_error.t

      (** The following lifter will check that their operand has a
          corresponding width. *)
      val i1 :  t -> t Or_error.t
      val i4 :  t -> t Or_error.t
      val i8 :  t -> t Or_error.t
      val i16 : t -> t Or_error.t
      val i32 : t -> t Or_error.t
      val i64 : t -> t Or_error.t

      (** [int w v] will be [Ok] if [v] has width [w] *)
      val int : int -> t -> t Or_error.t

      (** [of_word_size w] creates a lifter for a specified word size
          [w], i.e. either [i64] or [i32]  *)
      val of_word_size : Word_size.t -> t -> t Or_error.t

      include Integer with type t = t Or_error.t
      include Monad.Infix with type 'a t := 'a Or_error.t
    end

    (** Arithmetic that raises exceptions.

        This module exposes a common integer interface with
        operations not lifted into [Or_error] monad, but raising
        [Width] exception if operands sizes mismatch.
    *)
    module Int_exn : Integer with type t = t

    (** Prefix trees for bitvectors.

        Bitvector comes with 4 predefined prefix trees:

        - [Trie.Big.Bits] - big endian prefix tree, where each
        token is a bit, and bitvector is tokenized from msb to lsb.

        - [Trie.Big.Byte] - big endian prefix tree, where each token
        is a byte, and bitvector is tokenized from most significant
        byte to less significant

        - [Trie.Little.Bits] - is a little endian bit tree.

        - [Trie.Little.Byte] - is a little endian byte tree. *)
    module Trie : sig
      module Big : sig
        module Bits : Trie  with type key = t
        module Bytes : Trie with type key = t
      end
      module Little : sig
        module Bits : Trie with type key = t
        module Bytes : Trie with type key = t
      end
    end
  end

  (** Expose [endian] constructors to [Bap.Std] namespace  *)
  type endian = Bitvector.endian =
      LittleEndian | BigEndian
    [@@deriving sexp, bin_io, compare]

  (** Shortcut for bitvectors that represent words  *)
  module Word : module type of Bitvector
    with type t = word
     and type endian = endian
     and type comparator_witness = Bitvector.comparator_witness

  (** Shortcut for bitvectors that represent addresses  *)
  module Addr : sig
    include module type of Bitvector
    with type t = addr
     and type endian = endian
     and type comparator_witness = Bitvector.comparator_witness

    (** [memref ?disp ?index ?scale base] mimics a memory reference syntax
        in gas assembler,   [dis(base,index,scale)]
        assembler operation. It returns address at
        [base + index * scale + dis].

        @param disp stands for displacement and defaults to [0]
        @param index defaults for [0]
        @param scale defaults to [`r8]

        All operations are taken modulo {% $2^n$ %},
        where [n = bitwidth base]. *)
    val memref : ?disp:int -> ?index:int -> ?scale:size -> addr -> addr
  end

  (** The type of a BIL expression.

      Each BIL expression is either an immediate value of a given
      width, or a chunk of memory of a give size. The following
      predefined constructors are brought to the scope:

      - {{!bool_t}bool_t};
      - {{!reg8_t}reg8_t};
      - {{!reg16_t}reg16_t};
      - {{!reg32_t}reg32_t};
      - {{!reg64_t}reg64_t};
      - {{!reg128_t}reg128_t};
      - {{!reg256_t}reg256_t};
      - {{!mem32_t}mem32_t};
      - {{!mem64_t}mem64_t}.
  *)
  module Type : sig
    (** type is either an immediate value of memory reference *)
    type t =
      (** [Imm n] - n-bit immediate   *)
      | Imm of nat1
      (** [Mem (a,t)] memory with a specifed addr_size *)
      | Mem of addr_size * size
      [@@deriving variants]

    (** BIL type is regular  *)
    include Regular with type t := t
  end

  (** short abbreviation for a type  *)
  type typ = Type.t
    [@@deriving bin_io, compare, sexp]

  val bool_t  : typ             (** one bit             *)
  val reg8_t  : typ             (** 8-bit width value   *)
  val reg16_t : typ             (** 16-bit width value  *)
  val reg32_t : typ             (** 32-bit width value  *)
  val reg64_t : typ             (** 64-bit width value  *)
  val reg128_t: typ             (** 128-bit width value *)
  val reg256_t: typ             (** 256-bit width value *)

  (** [mem32_t size] creates a type for memory with [32]-bit addresses
      and elements of size [size].  *)
  val mem32_t : size -> typ

  (** [mem64_t size] creates a type for memory with [64]-bit addresses
      and elements of size [size].  *)
  val mem64_t : size -> typ

  (** bil variable   *)
  type var
    [@@deriving bin_io, compare, sexp]

  (** BIL variable.

      BIL variables are regular values. Variables can have
      indices. Usually the index is used to represent the same
      variable but at different time or space (control flow path).
      This is particulary useful for representing variables in SSA
      form.

      By default, comparison functions takes indices into account. In
      order to compare two variables regardless their index use [same]
      function, or compare with [base x].

      A variable can represent a physical register or be just a virtual
      variable.

      {2 Printing}

      A default pretty printer doesn't print zero indices.
  *)
  module Var : sig

    type t = var

    (** implements [Regular] interface  *)
    include Regular with type t := t

    (** [create ?register ?fresh name typ] creates a variable with
        a given [name] and [typ]e.

        A newly created variable has version equal to 0.

        If [fresh] is [true] (defaults to [false]), then a unique salt
        is mixed to the name of variable, making it unique.

        If [is_virtual] is [true] (defaults to [false]), then a
        variable is virtual, i.e., it doesn't correspond to some
        physical register or memory location and was added to a program
        artificially.
    *)
    val create : ?is_virtual:bool -> ?fresh:bool -> string -> typ -> t

    (** [name var] returns a name assosiated with variable  *)
    val name : t -> string

    (** [typ var] returns a type assosiated with variable  *)
    val typ : t -> typ

    (** [is_physical v] is [true] if a variable [v] represents a
        physical register or memory location.  *)
    val is_physical : t -> bool


    (** [is_virtual v] is [true] if [v] is not physical  *)
    val is_virtual : t -> bool

    (** [ var ver] returns a variable, that is identical to
        [var], but with version equal to [ver] *)
    val with_index : t -> int -> t

    (** [index v] returns variable's index  *)
    val index : t -> int

    (** [base var] returns an original variable. Essentially,
        identical to [with_index var 0] *)
    val base : t -> t

    (** [same x y] compares variables ignoring indices, i.e., for
        variables [x] and [y] the [same x y] is [true] iff [equal
        (base x) (base y)] *)
    val same : t -> t -> bool
  end



  (** Main BIL module

      This module defines BIL language and is useful to write BIL
      programs and expressions.

      Example:
      {[Bil.([
          v := src lsr i32 1;
          r := src;
          s := i32 31;
          while_ (var v <> i32 0) [
            r := var r lsl i32 1;
            r := var r lor (var v land i32 1);
            v := var v lsr i32 1;
            s := var s - i32 1;
          ];
          dst := var r lsl var s;
        ])]}
      where [i32] is defined as
      [let i32 x = Bil.int (Word.of_int ~width:32 x)]
      and [v,r,s] are some variables of type [var]; and
      [src, dst] are expressions of type [exp].
  *)
  module Bil : sig
    module Types : sig
      (** Different forms of casting *)
      type cast =
        | UNSIGNED (** 0-padding widening cast. *)
        | SIGNED   (** Sign-extending widening cast. *)
        | HIGH     (** Narrowning cast. Keeps the high bits. *)
        | LOW      (** Narrowing cast. Keeps the low bits. *)
        [@@deriving bin_io, compare, sexp]

      (** Binary operations implemented in the BIL *)
      type binop =
        | PLUS    (** Integer addition. (commutative, associative) *)
        | MINUS   (** Subtract second integer from first. *)
        | TIMES   (** Integer multiplication. (commutative, associative) *)
        | DIVIDE  (** Unsigned integer division. *)
        | SDIVIDE (** Signed integer division. *)
        | MOD     (** Unsigned modulus. *)
        | SMOD    (** Signed modulus. *)
        | LSHIFT  (** Left shift. *)
        | RSHIFT  (** Right shift, zero padding. *)
        | ARSHIFT (** Right shift, sign extend. *)
        | AND     (** Bitwise and. (commutative, associative) *)
        | OR      (** Bitwise or. (commutative, associative) *)
        | XOR     (** Bitwise xor. (commutative, associative) *)
        | EQ      (** Equals. (commutative) (associative on booleans) *)
        | NEQ     (** Not equals. (commutative) (associative on booleans) *)
        | LT      (** Unsigned less than. *)
        | LE      (** Unsigned less than or equal to. *)
        | SLT     (** Signed less than. *)
        | SLE     (** Signed less than or equal to. *)
        [@@deriving bin_io, compare, sexp]

      (** Unary operations implemented in the IR *)
      type unop =
        | NEG (** Negate. (2's complement) *)
        | NOT (** Bitwise not. *)
        [@@deriving bin_io, compare, sexp]

      (** BIL expression variants  *)
      type exp =
        | Load    of exp * exp * endian * size (** load from memory *)
        | Store   of exp * exp * exp * endian * size (** store to memory  *)
        | BinOp   of binop * exp * exp  (** binary operation  *)
        | UnOp    of unop * exp         (** unary operation *)
        | Var     of var                (** variable *)
        | Int     of word               (** immediate value *)
        | Cast    of cast * nat1 * exp  (** casting  *)
        | Let     of var * exp * exp    (** let-binding  *)
        | Unknown of string * typ       (** unknown or undefined value *)
        | Ite     of exp * exp * exp    (** if-then-else expression  *)
        | Extract of nat1 * nat1 * exp  (** extract portion of word  *)
        | Concat  of exp * exp          (** concatenate two words  *)
        [@@deriving bin_io, compare, sexp]

      type stmt =
        | Move    of var * exp  (** assign value of expression to variable *)
        | Jmp     of exp        (** jump to absolute address *)
        | Special of string     (** Statement with semantics not expressible in BIL *)
        | While   of exp * stmt list (** while loops  *)
        | If      of exp * stmt list * stmt list (** if/then/else statement  *)
        | CpuExn  of int                         (** CPU exception *)
        [@@deriving bin_io, compare, sexp]
    end

    (** include all constructors into Bil namespace *)
    open Types
    include module type of Types with type cast = cast
                                  and type binop = binop
                                  and type unop = unop
                                  and type exp = exp
                                  and type stmt = stmt
    type t = stmt list
      [@@deriving bin_io, compare, sexp]


    include Printable with type t := t
    include Data      with type t := t

    (** Infix operators  *)
    module Infix : sig
      val (:=) : var -> exp -> stmt

      (** {2 Arithmetic operations} *)
      val ( + )   : exp -> exp -> exp
      val ( - )   : exp -> exp -> exp
      val ( * )   : exp -> exp -> exp
      val ( / )   : exp -> exp -> exp
      val ( /$ )  : exp -> exp -> exp
      val ( mod ) : exp -> exp -> exp
      val ( %$ )  : exp -> exp -> exp

      (** {2 Bit operations} *)
      val ( lsl ) : exp -> exp -> exp
      val ( lsr ) : exp -> exp -> exp
      val ( asr ) : exp -> exp -> exp
      val ( land) : exp -> exp -> exp
      val ( lor ) : exp -> exp -> exp
      val ( lxor) : exp -> exp -> exp
      val lnot    : exp -> exp

      (** {2 Equality tests} *)
      val ( = )   : exp -> exp -> exp
      val ( <> )   : exp -> exp -> exp
      val ( < )   : exp -> exp -> exp
      val ( > )   : exp -> exp -> exp
      val ( <= )   : exp -> exp -> exp
      val ( >= )   : exp -> exp -> exp
      val ( <$ )  : exp -> exp -> exp
      val ( >$ )  : exp -> exp -> exp
      val ( <=$ ) : exp -> exp -> exp
      val ( >=$ ) : exp -> exp -> exp

      (** {2 Misc operations} *)
      (** [a ^ b] contatenate [a] and [b]  *)
      val ( ^ )   : exp -> exp -> exp
    end
    include module type of Infix

    (** {2 Functional constructors}  *)

    (** [move v x] evaluate [x] and assign its value to [v]  *)
    val move : var -> exp -> stmt

    (** [jmp t] evaluate expression [t] to absolute address and
        transfer control to that address *)
    val jmp : exp -> stmt

    val special : string -> stmt
    val while_ : exp -> stmt list -> stmt
    val if_ : exp -> stmt list -> stmt list -> stmt
    val cpuexn : int -> stmt
    val unsigned : cast
    val signed : cast
    val high : cast
    val low : cast
    val plus : binop
    val minus : binop
    val times : binop
    val divide : binop
    val sdivide : binop
    val modulo : binop
    val smodulo : binop
    val lshift : binop
    val rshift : binop
    val arshift : binop
    val bit_and : binop
    val bit_or  : binop
    val bit_xor : binop
    val eq : binop
    val neq : binop
    val lt : binop
    val le : binop
    val slt : binop
    val sle : binop
    val neg : unop
    val not : unop
    val load : mem:exp -> addr:exp -> endian -> size -> exp
    val store : mem:exp -> addr:exp -> exp -> endian -> size -> exp
    val binop : binop -> exp -> exp -> exp
    val unop : unop -> exp -> exp
    val var : var -> exp
    val int : Bitvector.t -> exp
    val cast : cast -> nat1 -> exp -> exp
    val let_ : var -> exp -> exp -> exp
    val unknown : string -> typ -> exp
    val ite : if_:exp -> then_:exp -> else_:exp -> exp
    val extract : hi:nat1 -> lo:nat1 -> exp -> exp
    val concat : exp -> exp -> exp

    (** {2:bil_helpers BIL Helper functions}  *)

    (** [is_referenced x p] is [true] if [x] is referenced in some
        expression or statement in program [p], before it is
        assigned. *)
    val is_referenced : var -> stmt list -> bool

    (** [is_assigned x p] is [true] if there exists such [Move]
        statement, that [x] occures on the left side of it. If
        [strict] is true, then only unconditional assignments are
        accounted. By default, [strict] is [false] *)
    val is_assigned : ?strict:bool -> var -> stmt list -> bool

    (** [prune_unreferenced ?physicals ?virtuals ?such_that p] remove
        all assignments to variables that are not used in the program
        [p]. This is a local optimization.  The variable is
        unreferenced if it is not referenced in its lexical scope, or if
        it is referenced after the assignment. A variable is pruned
        only if it matches to one of the user specified kind,
        described below (no variable matches the default values, so
        by default nothing is pruned):

        [such_that] matches a variable [v] for which [such_that v] is
        [true];

        [physicals] matches all physical variables (i.e., registers
        and memory locations). See {!Var.is_physical} for more
        information. Note: passing [true] to this option is in general
        unsound, unless you're absolutely sure, that physical
        variables will not live out program [p];

        [virtuals] matches all virtual variables (i.e., such variables
        that were added to a program artificially and are not
        represented physically in a program). See {!Var.is_virtual}
        for more information on virtual variables.



    *)
    val prune_unreferenced :
      ?such_that:(var -> bool) ->
      ?physicals:bool ->
      ?virtuals:bool ->
      stmt list -> stmt list

    (** [normalize_negatives p] transform [x + y] to [x - abs(y)] if [y < 0] *)
    val normalize_negatives : stmt list -> stmt list

    (** [substitute x y p] substitutes each occurrence of expression [x] by
        expression [y] in program [p]. The mnemonic to remember the
        order is to recall the sed's [s/in/out] syntax. *)
    val substitute : exp -> exp -> stmt list -> stmt list

    (** [substitute_var x y p] substitutes all free occurences of
        variable [x] in program [p] by expression [y]. A variable is
        free if it is not bounded in a preceding statement or not bound
        with let expression.  *)
    val substitute_var : var -> exp -> stmt list -> stmt list

    (** [free_vars bil] returns a set of free variables in program
        [bil]. Variable is considered free if it is not bound in a
        preceding statement or is not bound with [let] expression *)
    val free_vars : stmt list -> Var.Set.t

    (** [fold_consts] evaluate constant expressions.
        Note: this function performs only one step, and has no loops,
        it is supposed to be run using a fixpoint combinator.
    *)
    val fold_consts : stmt list -> stmt list

    (** [fixpoint f] applies transformation [f] until fixpoint is
        reached. If the transformation orbit contains non-trivial cycles,
        then the transformation will stop at an arbitrary point of a
        cycle. *)
    val fixpoint : (stmt list -> stmt list) -> (stmt list -> stmt list)

    (** Result of a computation.*)
    type result


    (** An interface to a memory storage.

        A storage is a mapping from addresses to bytes. For
        consistency and efficiency bytes are still reprented with
        bitvectors.

        Storages should not take care of aliasing or endiannes, as they
        are byte addressable. All memory operations are normalized by
        Bili. *)
    class type storage = object('s)

      (** [load a] loads a byte from a a given address  [a]  *)
      method load : addr -> word option

      (** [save a w] stores byte [w] at address [a]  *)
      method save : addr -> word -> 's
    end


    (** Predefined storage classes  *)
    module Storage : sig
      (** linear storage literally implements operational
          semantics, but has O(N) lookup and uses space
          very ineffectively, as it is implemented as a list
          of assignments. *)
      class linear : storage

      (** sparse storage is slightly more efficient storage,
          in comparison with linear. It uses balanced tree
          data structure, and provides logarithmic lookup and
          update method. *)
      class sparse : storage
    end


    (** Value of a result.
        We slightly diverge from an operational semantics by allowing
        a user to provide its own storage implementation.

        In operational semantics a storage is represented
        syntactically as
        {v
            v1 with [v2,ed] : nat <- v3,
        v}
        where v1 may be either a [Bot] value, representing an empty
        memory (or an absence of knowledge), or another storage. So a
        well typed memory object is defined inductively as:

        {v
          Inductive memory :=
           | bot : memory
           | store : (mem : memory) (addr : value) (data : value).
        v}

        That is equivalent to an assoc list. Although we provide an
        assoc list as storage variant (see {!Storage.linear}), the
        default storage is implemented slightly more effective, and
        uses linear space and provides $log(N)$ lookup and update
        methods. Users are encouraged to provide more efficient
        storage implementations, for interpreters that rely heave on
        memory throughput.
    *)
    type value =
      | Imm of word             (** immediate value  *)
      | Mem of storage          (** memory storage   *)
      | Bot                     (** undefined value  *)

    (** Result of computation.

        Result of an expression evaluation depends on a context.
        Thus, each result has a unique identifier, associated with it,
        that is usually provided by a context. The result is a
        concrete value, that is created whenever an expression is
        evaluated under a given context. Since, context is changed
        during the evaluation (at least because a new result is
        allocated), two consecutive evaluations of the same expression
        will give different results. (This property is preserved by
        Expi.context class, that provides methods for creating values
        of type result).

        Since [Result.Id] is a regular type, it is possible to
        associate arbitrary information (like taint information,
        formulae, etc) with each result, using associative data
        structures, like maps and hash tables.*)
    module Result : sig

      (** result identifier  *)
      type id

      type t = result

      (** State monad that evaluates to result  *)
      type 'a r = (result,'a) Monad.State.t

      (** State monad that evaluates to unit  *)
      type 'a u = (unit,'a) Monad.State.t

      (** [undefined id] creates result with a a given [id] and
          undefined value *)
      val undefined : id -> t

      (** [storage s id] creates result with a a given [id] and
          storage [s] as a value *)
      val storage : storage -> id -> t

      (** [word w id] creates result with a a given [id] and
          word [w] as a value *)
      val word : word -> id -> t

      (** returns result identifier  *)
      val id : t -> id

      (** returns result value  *)
      val value : t -> value

      (** Result identifier.  *)
      module Id : sig
        include Regular with type t = id
        val zero : t
        val succ : t -> t
      end

      module Value : Printable with type t = value
      include Printable with type t := t
    end

    (** Tries on BIL.

        Bil provides two prefix tries trees.

        The default one is not normalized and will compare bil statements
        literally. This means that comparison is sensitive to variable
        names and immediate values. Depending on your context it may be
        find or not. For example, two [SP] variables may compare as different
        if one of them was obtained from different compilation (and met
        the other one through some persistant storage, e.g., file on hard
        disk). Moreover, BIL obtained from different lifters will have
        different names for the same registers. All this issues are
        addressed in normalized [Trie]. *)
    module Trie : sig
      type normalized_bil

      (** [normalize ?subst bil] normalize BIL. If [subst] is provided,
          then substitute each occurence of the fst expression to the
          snd expression before the normalization. The effect of
          normalization is the following:

          1. All immediate values are compared equal
          2. All variables are compared nominally
          3. BIL is simplified to reduce the syntactic differences
          (but the comparison is still syntactic, and (x + 2) will
          be compared differently to (2 + x).
      *)
      val normalize : ?subst:(exp * exp) list -> stmt list -> normalized_bil

      module Normalized : Trie with type key = normalized_bil
      include Trie with type key = stmt list
    end
  end

  type bil   = Bil.t       [@@deriving bin_io, compare, sexp]
  type binop = Bil.binop   [@@deriving bin_io, compare, sexp]
  type cast  = Bil.cast    [@@deriving bin_io, compare, sexp]
  type exp   = Bil.exp     [@@deriving bin_io, compare, sexp]
  type stmt  = Bil.stmt    [@@deriving bin_io, compare, sexp]
  type unop  = Bil.unop    [@@deriving bin_io, compare, sexp]
  type value               [@@deriving bin_io, compare, sexp]
  type dict                [@@deriving bin_io, compare, sexp]


  (** Base class for evaluation contexts.

      All interpreters evaluate terms under a given context,
      wrapped into a state monad. All context must be structural
      subtypes of the [Context.t].

      The base context is just a mapping from variables to values.

      Other than a type [Context.t] this module has n class [t] that
      provides a logarithmic implementation for lookup and update
      methods.

      Since context, for any interpreter must be a structural subtype
      of [Context.t] it is not required that this particular should be used.
      Any implementation that has matching interface will work.
  *)
  module Context : sig
    class t : object('s)
      method lookup : var -> Bil.result option
      method update : var -> Bil.result -> 's
      method bindings : (var * Bil.result) Sequence.t
    end
  end

  module Type_error : sig
    type t [@@deriving bin_io, compare, sexp]

    val bad_mem : t
    val bad_imm : t
    val bad_cast : t
    val bad_type : exp:typ -> got:typ -> t

    include Regular with type t := t
  end

  (** A BIL type error  *)
  type type_error = Type_error.t [@@deriving bin_io, compare, sexp]


  (** BIL Interpreter.*)
  module Expi : sig
    open Bil.Result
    (**

       An extensible interpreter for BIL expressions.

       Note: before diving into the deepness of Expi module consider
       [Exp.eval] function, that expose an easy interface to concrete
       evaluation of expressions.

       Expi implements an operational semantics described in [1].


       [1]: https://github.com/BinaryAnalysisPlatform/bil
    *)


    (** Context for expression evaluation.

        Context provides a unique identifier for each freshly created
        value.  *)
    class context : object('s)
      inherit Context.t

      (** creates a fresh new result, containing an undefined value,
          and returns it with a modified context. *)
      method create_undefined : 's * Bil.result
      (** creates a fresh new result, containing a given word,
          and returns it with a modified context. *)
      method create_word : word -> 's * Bil.result
      (** creates a fresh new result, containing a given storage,
          and returns it with a modified context. *)
      method create_storage : Bil.storage -> 's * Bil.result
    end

    (** Expression interpreter.

        Expi is a base class for all other interpreters (see {!bili}
        and {!biri}, that do all the hard work. Expi recognizes a
        language defined by [exp] type. It evaluates arbitrary
        expressions under provided {{!Context}context}.


        To create new interpreter use operator [new]:

        {v
        let expi = new expi;;
        val expi : _#Expi.context expi = <obj>
        v}

        Note: The type [_#Expi.context] is weakly polymorphic subtype of
        [Expi.context][1]. Basically, this means, that the type is not
        generalized and will be instantiated when used and fixed
        afterwards.

        {v
        let r = expi#eval_exp Bil.(int Word.b0 lor int Word.b1);;
        val r : _#Expi.context Bil.Result.r = <abstr>
        v}

        The returned value is a state monad parametrized by a subtype
        of class [Expi.context]. The state monad is a chain of
        computations, where each computation is merely a function from
        state to a state paired with the result of computation. The
        state is accessible inside the computation and can be
        changed.

        To run the computation use [Monad.State.eval] function, that
        accepts a state monad and an initial value. Here we can
        provide any subtype of [Expi.context] as an initial
        value. Let start with a [Expi.context] as first approximation:

        {v
        let x = Monad.State.eval r (new Expi.context);;
        val x : Bil.result = [0x3] true
        v}

        The expression evaluates to [true], and the result is tagged
        with an identifier [[0x3]]. The [Exp.context] assigns a unique
        identifier for each freshly created result. Tag [[0x3]] means
        that this was the third value created under provided context.

        If the only thing, that you need is just to evaluate an
        expression, then you can just use [Exp.eval] function:

        {v
        Exp.eval Bil.(int Word.b0 lor int Word.b1);;
        - : Bil.value = true
        v}

        The main strength of [expi] is its extensibility. Let's write
        a expression evaluator that will record a trace of evaluation:

        {[
          class context = object
            inherit Expi.context
            val events : (exp * Bil.result) list = []
            method add_event exp res = {< events = (exp,res) :: events >}
            method events = List.rev events
          end
        ]}

        {[
          class ['a] exp_tracer = object
            constraint 'a = #context
            inherit ['a] expi as super
            method! eval_exp e =
              let open Monad.State in
              super#eval_exp e >>= fun r ->
              get () >>= fun ctxt ->
              put (ctxt#add_event e r) >>= fun () ->
              return r
          end;;
        ]}

        Note : We made our [exp_tracer] class polymorphic as a
        courtesy to our fellow programmer, that may want to reuse it.
        We can define it by inheriting from [expi] parametrized with
        our context type, like this: [inherit [context] expi]

        Also, there is no need to write a [constraint], as it will be
        inferred automatically.

        Now, let's try to use our tracer. We will use
        [Monad.State.run] function, that returns both, the evaluated
        value and the context. (We can also use [Monad.State.exec], if
        we're not interested in value at all):

        {v
        let expi = new exp_tracer;;
        val expi : _#context exp_tracer = <obj>
        # let r = expi#eval_exp Bil.(int Word.b0 lor int Word.b1);;
        val r : _#context Bil.Result.r = <abstr>
        # let r,ctxt = Monad.State.run r (new context) ;;
        val r : Bil.result = [0x3] true
        val ctxt : context = <obj>
        ctxt#events;;
        - : (exp * Bil.result) list =
        [(false, [0x1] false); (true, [0x2] true); (false | true, [0x3] true)]
        v}

        [1]: The weakness of the type variable is introduced by
        a value restriction and can't be relaxed since it is invariant
        in state monad.
    *)
    class ['a] t : object
      constraint 'a = #context

      (** {2 Interaction with environment} *)


      (** creates an empty storage. If you want to provide
          your own implementation of storage, then it is definitely
          the right place.  *)
      method empty  : Bil.storage

      (** a variable is looked up in a context *)
      method lookup : var -> 'a r

      (** a variable is bind to a value.*)
      method update : var -> Bil.result -> 'a u

      (** a byte is loaded from a given address  *)
      method load   : Bil.storage -> addr -> 'a r

      (** a byte is stored to a a given address  *)
      method store  : Bil.storage -> addr -> word -> 'a r

      (** {2 Error conditions}  *)

      (** a given typing error has occured  *)
      method type_error : type_error -> 'a r

      (** we can't do this!  *)
      method division_by_zero : unit -> 'a r

      (** called when storage doesn't contain the addr  *)
      method undefined_addr : addr -> 'a r

      (** called when context doesn't know the variable  *)
      method undefined_var  : var  -> 'a r

      (** {2 Evaluation methods}  *)
      method eval_exp : exp -> 'a r
      method eval_var : var -> 'a r
      method eval_int : word -> 'a r
      method eval_load : mem:exp -> addr:exp -> endian -> size -> 'a r
      method eval_store : mem:exp -> addr:exp -> exp -> endian -> size -> 'a r
      method eval_binop : binop -> exp -> exp -> 'a r
      method eval_unop  : unop -> exp -> 'a r
      method eval_cast  : cast -> nat1 -> exp -> 'a r
      method eval_let : var -> exp -> exp -> 'a r
      method eval_ite : cond:exp -> yes:exp -> no:exp -> 'a r
      method eval_concat : exp -> exp -> 'a r
      method eval_extract : nat1 -> nat1 -> exp -> 'a r
      method eval_unknown : string -> typ -> 'a r
    end
  end

  (** Expression {{!Expi}interpreter}  *)
  class ['a] expi : ['a] Expi.t

  (** BIL Interpreter.

      [bili] extends [expi] with methods for evaluating BIL
      statements, thus allowing one to interpret BIL AST. To
      interpret BIL in the intermediate representation use
      {{!Biri}biri}.

      Also, if you don't need to change the default behavior
      of the interpreter, then you may use {!Stmt.eval} that
      exposes an easier interface for BIL evaluation. For example,

      {v
      let x = Var.create "x" bool_t;;
      val x : var = x
      let ctxt = Stmt.eval [Bil.(x := int Word.b0)] (new Bili.context);;
      val ctxt : Bili.context = <obj>
      ctxt#bindings |> Seq.to_list;;
      - : (var * Bil.result) list = [(x, [0x1] false)]
      v}
  *)
  module Bili : sig


    open Bil.Result

    (** [Bili.context] extends [Expi.context] with PC (Program
        Counter).  *)
    class context : object('s)
      inherit Expi.context
      method pc : Bil.value
      method with_pc : Bil.value -> 's
    end

    (** Base class for BIL interpreters   *)
    class ['a] t : object
      constraint 'a = #context
      inherit ['a] expi
      method eval : stmt list -> 'a u
      method eval_stmt : stmt -> 'a u
      method eval_move : var -> exp -> 'a u
      method eval_jmp : exp -> 'a u
      method eval_while : cond:exp -> body:stmt list -> 'a u
      method eval_if : cond:exp -> yes:stmt list -> no:stmt list -> 'a u
      method eval_cpuexn : int -> 'a u
      method eval_special : string -> 'a u
    end
  end

  (** BIL {{!Bili}interpreter} *)
  class ['a] bili : ['a] Bili.t


  (** [Regular] interface for BIL expressions *)
  module Exp : sig
    type t = Bil.exp

    (** All visitors provides some information about the current
        position of the visitor *)
    class state : object

      (** a stack of expr, that are parents for the currenly visiting
          expression *)
      val exps_stack  : exp  list

      (** is [true] if currently visiting entry is executed conditionally *)
      val under_condition : bool
    end


    class ['a] visitor : object
      inherit state

      method enter_exp : t -> 'a -> 'a
      method visit_exp : t -> 'a -> 'a
      method leave_exp : t -> 'a -> 'a

      (** [Load (src,addr,endian,size)]  *)
      method enter_load : mem:t -> addr:t -> endian -> size -> 'a -> 'a
      method visit_load : mem:t -> addr:t -> endian -> size -> 'a -> 'a
      method leave_load : mem:t -> addr:t -> endian -> size -> 'a -> 'a

      (** [Store (dst,addr,src,endian,size)]  *)
      method enter_store : mem:t -> addr:t -> exp:t -> endian -> size -> 'a -> 'a
      method visit_store : mem:t -> addr:t -> exp:t -> endian -> size -> 'a -> 'a
      method leave_store : mem:t -> addr:t -> exp:t -> endian -> size -> 'a -> 'a

      (** [BinOp (op,e1,e2)]  *)
      method enter_binop : binop -> t -> t -> 'a -> 'a
      method visit_binop : binop -> t -> t -> 'a -> 'a
      method leave_binop : binop -> t -> t -> 'a -> 'a

      (** [Unop (op,e)]  *)
      method enter_unop : unop -> t -> 'a -> 'a
      method visit_unop : unop -> t -> 'a -> 'a
      method leave_unop : unop -> t -> 'a -> 'a

      (** [Cast(kind,size,e)]  *)
      method enter_cast : cast -> nat1 -> t -> 'a -> 'a
      method visit_cast : cast -> nat1 -> t -> 'a -> 'a
      method leave_cast : cast -> nat1 -> t -> 'a -> 'a

      (** [Let (v,t,body)]  *)
      method enter_let : var -> exp:t -> body:t -> 'a -> 'a
      method visit_let : var -> exp:t -> body:t -> 'a -> 'a
      method leave_let : var -> exp:t -> body:t -> 'a -> 'a

      (** [Ite (cond,yes,no)]  *)
      method enter_ite : cond:t -> yes:t -> no:t -> 'a -> 'a
      method visit_ite : cond:t -> yes:t -> no:t -> 'a -> 'a
      method leave_ite : cond:t -> yes:t -> no:t -> 'a -> 'a

      (** [Extract (hi,lo,e)]  *)
      method enter_extract : hi:nat1 -> lo:nat1 -> t -> 'a -> 'a
      method visit_extract : hi:nat1 -> lo:nat1 -> t -> 'a -> 'a
      method leave_extract : hi:nat1 -> lo:nat1 -> t -> 'a -> 'a

      (** [Concat(e1,e2)]  *)
      method enter_concat : t -> t -> 'a -> 'a
      method visit_concat : t -> t -> 'a -> 'a
      method leave_concat : t -> t -> 'a -> 'a

      (** {2 Leaves} *)
      (** [Int w]  *)
      method enter_int : word -> 'a -> 'a
      method visit_int : word -> 'a -> 'a
      method leave_int : word -> 'a -> 'a

      (** [Var v]  *)
      method enter_var : var -> 'a -> 'a
      method visit_var : var -> 'a -> 'a
      method leave_var : var -> 'a -> 'a

      (** [Unknown (str,typ)]  *)
      method enter_unknown : string -> typ -> 'a -> 'a
      method visit_unknown : string -> typ -> 'a -> 'a
      method leave_unknown : string -> typ -> 'a -> 'a
    end

    (** A visitor with shortcut.
        Finder is a specialization of a visitor, that uses [return] as its
        folding argument. At any time you can stop the traversing by
        calling [return] function of the provided argument (which is by
        itself is a record with one field - a function accepting argument
        of type ['a option]).*)
    class ['a] finder : object
      inherit ['a option return] visitor
      method find : t -> 'a option
    end


    (** Exp mapper.
        By default performs deep identity mapping. Non-leaf methods
        deconstructs terms, calls corresponding methods on its parts
        and the constructs it back. So if you're overriding a non-leaf
        method, then make sure that you called the parent method if
        you want a normal traversal.

        A usual template for method overriding is:
        {[
          object(self)
            inherit mapper as super
            method map_X arg=
              let x = super#map_X arg in
              do_mapping x
          end
        ]}
    *)
    class mapper : object
      inherit state
      method map_exp : t -> t
      method map_load : mem:t -> addr:t -> endian -> size -> t
      method map_store : mem:t -> addr:t -> exp:t -> endian -> size -> t
      method map_binop : binop -> t -> t -> t
      method map_unop : unop -> t -> t
      method map_cast : cast -> nat1 -> t -> t
      method map_let : var -> exp:t -> body:t -> t
      method map_ite : cond:t -> yes:t -> no:t -> t
      method map_extract : hi:nat1 -> lo:nat1 -> t -> t
      method map_concat : t -> t -> t
      method map_int : word -> t
      method map_var : var -> t
      method map_sym : var -> var
      method map_unknown : string -> typ -> t
    end


    (** [fold visitor ~init exp] traverse the [exp] tree with
        provided [visitor]. For example, the following will collect
        all address that are accessed with a load operation:
        [{
        let collect_load_addresses = Exp.fold ~init:[] (object
            inherit [word list] Bil.visitor
            method! enter_load ~mem ~addr _ _  addrs =
            match addr with
            | Bil.Int addr -> addr :: addrs
            | _ -> addrs
        end)
        }]
        See also {!Bil.fold} and {!Stmt.fold}
    *)
    val fold : 'a #visitor -> init:'a -> t -> 'a

    (** [iter visitor exp] iterates over all terms of the [exp] using
        provided visitor. See also {!Bil.iter} and {!Stmt.iter}  *)
    val iter : unit #visitor -> t -> unit

    (** [find finder exp] returns [Some thing] if finder finds some
        [thing]. See also {!Bil.find} and {!Stmt.find} *)
    val find : 'a #finder -> t -> 'a option

    (** [map mapper exp] maps [exp] tree using provided [mapper].
        See also {!Bil.map} *)
    val map  : #mapper -> t -> t

    (** [exists finder exp] is [true] if [finder] finds
        something. See also {!Bil.exists} and {Stmt.exists}  *)
    val exists : unit #finder -> t -> bool

    (** [is_referenced x exp] true if [exp] contains [Var x] on one of
        its leafs. See also {!Bil.is_referenced} and {!Stmt.is_referenced}  *)
    val is_referenced : var -> t -> bool

    (** [normalize_negatives exp] returns an exp where all negative
        additions are substituted by subtractions. See
        {!Bil.normalize_negatives} for more details  *)
    val normalize_negatives : t -> t

    (** [fold_consts] performs one step of constant evaluation. In
        order to perform all possible reductions one should use
        {!fixpoint} function, provided later. Example:
        [let x = Bil.var (Var.create "x" reg32_t)]
        [fixpoint fold_consts Bil.(x lxor x lxor x lxor x)]

        will yield [0x0:32], but without
        a fixpoint, the result would be just:

        [fold_constants Bil.(x lxor x lxor x lxor x)]
        [(0x0:32 ^ x) ^ x].

        See also {!Bil.fold_consts} *)
    val fold_consts : t -> t

    (** [fixpoint f] applies transformation [f] to [t] until it
        reaches a fixpoint, i.e., such point [x] that
        [f x] = [f (f x)].
        See also {!Bil.fixpoint} and {!Stmt.fixpoint}
    *)
    val fixpoint : (t -> t) -> (t -> t)

    (** [free_vars exp] returns a set of all unbound variables, that
        occurs in the expression [exp]. *)
    val free_vars : t -> Var.Set.t


    val eval : exp -> Bil.value

    include Regular with type t := t
    val pp_adt : t printer
  end

  (** [Regular] interface for BIL statements  *)
  module Stmt : sig
    type t = Bil.stmt


    (** All visitors provides some information about the current
        position of the visitor *)
    class state : object
      (** the stack of stmts that was already visited, with the last on
          the top. Not including the currently visiting stmt. *)
      val preds : stmt list

      (** stmts that are not yet visited  *)
      val succs : stmt list

      (** a stack of stmts that are parents for the currently visiting
          entity. The top one is the one that we're currently visiting. *)
      val stmts_stack : stmt list

      (** is [true] if we're visiting expression that is a jump target *)
      val in_jmp : bool

      (** is [true] if we're visiting expression that is on the left or
          right side of the assignment. *)
      val in_move : bool

      (** is [true] if currently visiting expression or statement is
          executed under loop.  *)
      val in_loop : bool
    end

    (** Visitor.
        Visits AST providing lots of hooks.

        For each AST constructor [C] the visitor provides three methods:
        [enter_C], [visit_C], [leave_C]. The default implementation for
        [enter_C] and [leave_C] is to return its argument. The default
        implementation for [visit_C] is the following:
        1. call [enter_C]
        2. visit all children
        3. call [leave_C].

        It is recommended to override [enter_C] method if you only need
        to visit [C] constructor without changing a way you're visiting
        the tree.

        For example, to collect all resolved jumps one could write the
        following function:

        {[
          let collect_calls bil = (object(self)
            inherit [Word.t list] visitor
            method! enter_int x js = if in_jmp then x :: js else js
          end)#run bil []
        ]}

         The default entry point of the visitor is method [run], but
         you can use any other method as well, for example, if you do
         not have a statement at all and want to visit expression.
    *)
    class ['a] visitor : object
      inherit ['a] Exp.visitor
      inherit state
      (** Default entry point *)
      method run : t list -> 'a -> 'a

      (** {2 Statements}  *)
      method enter_stmt : t -> 'a -> 'a
      method visit_stmt : t -> 'a -> 'a
      method leave_stmt : t -> 'a -> 'a

      (** [Move(var,exp)]  *)
      method enter_move : var -> exp -> 'a -> 'a
      method visit_move : var -> exp -> 'a -> 'a
      method leave_move : var -> exp -> 'a -> 'a

      (** [Jmp exp]  *)
      method enter_jmp : exp -> 'a -> 'a
      method visit_jmp : exp -> 'a -> 'a
      method leave_jmp : exp -> 'a -> 'a

      (** [While (cond,bil)]  *)
      method enter_while : cond:exp -> t list -> 'a -> 'a
      method visit_while : cond:exp -> t list -> 'a -> 'a
      method leave_while : cond:exp -> t list -> 'a -> 'a

      (** [If (cond,yes,no)]  *)
      method enter_if : cond:exp -> yes:t list -> no:t list -> 'a -> 'a
      method visit_if : cond:exp -> yes:t list -> no:t list -> 'a -> 'a
      method leave_if : cond:exp -> yes:t list -> no:t list -> 'a -> 'a

      (** [CpuExn n]  *)
      method enter_cpuexn : int -> 'a -> 'a
      method visit_cpuexn : int -> 'a -> 'a
      method leave_cpuexn : int -> 'a -> 'a

      (** [Special string]  *)
      method enter_special : string -> 'a -> 'a
      method visit_special : string -> 'a -> 'a
      method leave_special : string -> 'a -> 'a
    end


    (** A visitor with shortcut.
        Finder is a specialization of a visitor, that uses [return] as its
        folding argument. At any time you can stop the traversing by
        calling [return] function of the provided argument (which is by
        itself is a record with one field - a function accepting argument
        of type ['a option]).

        For example, the following function will check whether [x]
        variable is assigned (i.e., occurs on the left of the
        assignment operator) in the provided scope.
        {[
          let is_assigned x = find (object(self)
              inherit [unit] finder
              method! enter_move y _rhs cc =
                if Var.(x = y) then cc.return (Some ()); cc
            end)
        ]}

        There're three [find] functions in the library, that accepts
        an object of type [finder]:

        - [Bil.finder] searches in the [stmt list] aka [bil]
        - [Stmt.finder] searches in [stmt]
        - [Exp.finder] searches in [exp].

        In addition, you can use this object directly, using one of
        the two provided entry points.  *)
    class ['a] finder : object
      inherit ['a option return] visitor
      method find : t list -> 'a option
    end


    (** AST transformation.
        mapper allows one to map AST, performing some limited
        amount of transformations on it. Mapper provides extra
        flexibility by mapping [stmt] to [stmt list], thus allowing
        to remove statements from the output (by mapping to empty list) or
        to map one statement to several. This is particularly useful when
        you map [if] or [while] statements. *)
    class mapper : object
      inherit Exp.mapper
      inherit state

      (** Default entry point.
          But again, you can use any method as an entry  *)
      method run : t list -> t list
      method map_stmt : t -> t list
      method map_move : var -> exp -> t list
      method map_jmp : exp -> t list
      method map_while : cond:exp -> t list -> t list
      method map_if : cond:exp -> yes:t list -> no:t list -> t list
      method map_cpuexn : int -> t list
      method map_special : string -> t list
    end


    (** [constant_folder] is a class that implements the [fold_consts]  *)
    class constant_folder : mapper


    (** [fold ~init visitor stmt] folds a [stmt] with a visitor. See
        {!Bil.fold} and {!Exp.fold} for more details.  *)
    val fold : 'a #visitor -> init:'a -> t -> 'a

    (** [iter visitor stmt] iters over a [stmt] with a visitor. See
        {!Bil.iter} and {!Exp.iter} for more details.  *)
    val iter : unit #visitor -> t -> unit


    (** [map mapper bil] applies [mapper] to the program [bil] *)
    val map : #mapper -> t list -> t list

    (** [find finder stmt] performs a lookup into the Bil statement. See
        {!Bil.find} and {!Exp.find} for more details.  *)
    val find : 'a #finder -> t -> 'a option


    (** [exists finder stmt] is [true] iff [find finder stmt <> None].
        See {!Bil.exists} and {!Exp.exists} for more details.  *)
    val exists : unit #finder -> t -> bool

    (** [is_referenced x stmt] is true is [x] is used in the [stmt]
        in any place other then right hand side of the assignment. E.g.,
        [is_referenced x Bil.(x := var x)] is [true], but
        [is_referenced x Bil.(x := var y)] is [false].
        see {!Bil.is_referenced} for more details.
    *)
    val is_referenced : var -> t -> bool

    (** [fixpoint f x] applies transformation [f] until it reaches
        fixpoint. See {!Bil.fixpoint} and {Exp.fixpoint}  *)
    val fixpoint : (t -> t) -> (t -> t)

    (** [free_vars stmt] returns a set of all unbound variables, that
        occurs in the [stmt]. *)
    val free_vars : t -> Var.Set.t


    val eval : t list -> (#Bili.context as 'a) -> 'a

    include Regular with type t := t
    val pp_adt : t printer
  end

  (** Architecture  *)
  module Arch : sig
    type x86 = [
      | `x86
      | `x86_64
    ] [@@deriving bin_io, compare, enumerate, sexp]

    type arm = [
      | `armv4
      | `armv5
      | `armv6
      | `armv7
    ] [@@deriving bin_io, compare, enumerate, sexp]

    type armeb = [
      | `armv4eb
      | `armv5eb
      | `armv6eb
      | `armv7eb
    ] [@@deriving bin_io, compare, enumerate, sexp]

    type thumb = [
      | `thumbv4
      | `thumbv5
      | `thumbv6
      | `thumbv7
    ] [@@deriving bin_io, compare, enumerate, sexp]

    type thumbeb = [
      | `thumbv4eb
      | `thumbv5eb
      | `thumbv6eb
      | `thumbv7eb
    ] [@@deriving bin_io, compare, enumerate, sexp]

    type aarch64 = [
      | `aarch64
      | `aarch64_be
    ]
      [@@deriving bin_io, compare, enumerate, sexp]

    type ppc = [
      | `ppc
      | `ppc64
      | `ppc64le
    ]
      [@@deriving bin_io, compare, enumerate, sexp]

    type mips = [
      | `mips
      | `mipsel
      | `mips64
      | `mips64el
    ]
      [@@deriving bin_io, compare, enumerate, sexp]

    type sparc = [
      | `sparc
      | `sparcv9
    ]
      [@@deriving bin_io, compare, enumerate, sexp]

    type nvptx = [
      | `nvptx
      | `nvptx64
    ]
      [@@deriving bin_io, compare, enumerate, sexp]

    type hexagon = [`hexagon]
      [@@deriving bin_io, compare, enumerate, sexp]

    type r600 = [`r600]
      [@@deriving bin_io, compare, enumerate, sexp]

    type systemz = [`systemz]
      [@@deriving bin_io, compare, enumerate, sexp]

    type xcore = [`xcore]
      [@@deriving bin_io, compare, enumerate, sexp]

    type t = [
      | aarch64
      | arm
      | armeb
      | thumb
      | thumbeb
      | hexagon
      | mips
      | nvptx
      | ppc
      | r600
      | sparc
      | systemz
      | x86
      | xcore
    ] [@@deriving bin_io, compare, enumerate, sexp]

    (** [of_string s] will try to be clever and to capture all
        commonly known synonyms, e.g., [of_string "i686"] will
        work    *)
    val of_string : string -> t option

    (** [addr_size arch] returns an address size for a a given [arch]  *)
    val addr_size : t -> addr_size

    (** [endian arch] returns a word endianness of the [arch]  *)
    val endian : t -> endian

    (** [arch] type implements [Regular]  interface  *)
    include Regular with type t := t
  end

  (** architecture  *)
  type arch = Arch.t
    [@@deriving bin_io, compare, sexp]


  (** Universal Values.

      This module creates an extensible variant type, that resembles
      extensible variant types, introduced in 4.02, but even more safe
      and more extensible, and, what really matters,
      serializable. Basically you should think of [Value.t] as a union
      type, aka sum type, that can be extended in any place, including
      your plugin code. Where extending is adding new constructor. To
      add new constructor, you need to register it, e.g.,

      {[
        let function_signature = Value.Tag.register (module String)
            ~name:"function_signature"
            ~uuid:"2175c28c-08ca-4052-8385-3a01e1c6ab6f"
      ]}

      This is merely equivalent to adding a branch

      {[
        | Function_signature of string
      ]}

      to existing union type. The main difference is that the [name]
      shouldn't be unique (in fact [name] doesn't bear any semantic
      meaning, it basically for pretty-printing). On the other hand
      the [uuid] parameter must be unique across the universe, space
      and time. To get the UUID with such properties, you can use
      [uuidgen] program that is usually available on Linux and Mac OS.

      [name] and [uuid] must be strings, known at compile time, in
      other words it must be string literal, not just an arbitrary
      string, created dynamically. This is made intentionally, in
      order to prevent the abuse of the system.

      The [(module String)] syntax creates a value from the module
      [String], (so called first-class module). The module should
      implement [Value.S] signature, that requires pretty-printing,
      comparison function and serialization.

      {[
        module type S = sig
          type t with bin_io, compare, sexp

          val pp : Format.formatter -> t -> unit
        end
      ]}


      The good news is that, most of the types in [Core] and [Bap] do
      conform with the requirements. Usually, one can implement the
      requirements very easily by using type-driven syntax extensions
      (although, you still need to implement pretty-printing function
      yourself):

      {[
        module Loc = struct
          type t = string * int * int
          with bin_io, compare, sexp

          let pp ppf (file,line,col) =
            Format.fprintf ppf "%s:%d:%d" file line col
        end

        let loc = Value.Tag.register (module Loc)
            ~name:"loc"
            ~uuid:"400e190e-ce21-488d-87b1-c101709621a8"
      ]}

      The returned value, is a tag that can be used to constructed
      values of that branch, and to deconstruct (extract) them. You
      may think of it as a cipher key, that is used to package data
      into the value container, and later to unpack it:

      {[
        # let main_pos = Value.create loc ("test.c", 20, 2);;
        val main_pos : value = test.c:20:2
      ]}

      You may see, that OCaml pretty-prints the value. That's neat!
      Also, you may see, that the returned expression has type
      [value]. That means that it can be used uniformly with other
      values, for example, you can put them in one container, e.g.,

      {[
        # let main_t = Value.create function_signature
              "void main(int argc, const char *argv[])";;
        val main_t : value = void main(int argc, const char *argv[])
      ]}

      {[
        # let main = [main_pos; main_t];;
        val main : value list = [
            test.c:20:2;
            void main(int argc, const char *argv[])
          ]
      ]}

      To extract value you can use [Value.get] function:

      {[
        # Value.get loc main_pos;;
        - : Loc.t option = Some ("test.c", 20, 2)
      ]}

      This will require an extra allocation of an [option] container,
      and in a performance critical context it may be unacceptable.
      For this special case you can use a more efficient:

      {[if Value.is loc then Value.get_exn loc main_pos]}.

      Underneath the hood, the values of type [value] is just a pair
      of an original value and runtime type information. For
      performance reasons the RTTI is usually just an integer. But
      for serialization we use persistent UUID for storing RTTI type.
      To get it, one can use [Value.typeid] function.

      The comparison of two values of type [value] is actually a
      multi-method, as it has the following behavior:

      1. If both values has the same type, then use [compare]
         function, that was provided for this type.
      2. If values are of different types, that are known to
         the type system, then compare them using RTTI
      3. If at least one of the values is of the unknown type,
         (i.e., type wasn't registered in the type system), then
         use polymorphic compare on a tuple of UUID and binary
         representation of the values.

      This algorithm implies that ordering may change a little bit
      between different compiler versions and different programs, as
      RTTI is generated from scratch at every program start. If it
      really matters (usually it doesn't), then you should use
      [typeid] as key. In that case the ordering would be stable
      across space time. In any case it is not recommended to use data
      structures where [value]s are used as keys. For this case, we
      provide {{!Dict}Dict} data structure, that is a heterogeneous
      dictionary of values.

      {2 Thread safety}

      The only thread unsafe function is [register], that should be
      called in the module initialization time. In general programs
      modules are initialized in a single thread, so this shouldn't be
      an issue.  The implementation by itself doesn't call [register].
  *)
  module Value : sig

    (** a universal value  *)
    type t = value [@@deriving bin_io, compare, sexp]


    (** Tag constructor of type ['a]  *)
    type 'a tag

    (** A required interface for the type to be lifted to value. *)
    module type S = sig
      (** In order to construct a value with the a given type you must
          provide an implementation for marshaling functions,
          comparison function and pretty-printing.  *)
      type t [@@deriving bin_io, compare, sexp]
      val pp : Format.formatter -> t -> unit
    end

    (** uninhabited type  *)
    type void

    (** literal string. Don't look at the right hand side of a type
        equation, this is just a way to say that a string should be a
        literal not a value. Compiler will automatically coerce your
        string literals to this type. *)
    type literal = (void,void,void) format

    (** persistent type identifier  *)
    type typeid [@@deriving bin_io, compare, sexp]

    (** [create cons x] creates a value using constructor [cons] and
        argument [x] *)
    val create : 'a tag -> 'a -> t

    (** [is cons v] true if value [v] was constructed with constructor
        [cons], i.e., it is true only when [is_cons t (create t x)] *)
    val is  : 'a tag -> t -> bool

    (** [get cons] extracts a value associated with a constructor [cons]
        (Essentially, performs a pattern match on the specified variant
        branch) *)
    val get : 'a tag -> t -> 'a option

    (** [get_exn t v] extracts value created with [t] from the
        variant. Raises unspecified exception if variant [v] wasn't
        created with [t].  *)
    val get_exn : 'a tag -> t -> 'a

    (** [tagname value] returns a constructor name of the [value]  *)
    val tagname : t -> string

    (** [typeid value] returns a type identifier of the [value]  *)
    val typeid : t -> typeid

    (** Variants of values.  *)
    module Tag : sig
      type 'a t = 'a tag
      (** [register ~name ~uuid (module T)] creates a new variant
          constructor, that accepts values of type [T.t]. Module [T]
          should implement [Binable.S] and [Sexpable.S] interfaces,
          provide [compare] and pretty-printing [pp] functions. This
          functions will be used to print, compare and serialize
          values.

          [uuid] and [name] parameters must be string literals, i.e.,
          they must be known at compile time. This is to prevent the
          abuse of type system.

          The returned value of type [T.t tag] is a special key that
          can be used with [create] and [get] functions to pack and
          unpack values of type [T.t] into [value]. *)
      val register : name:literal -> uuid:literal ->
        (module S with type t = 'a) -> 'a tag

      (** [name cons] returns a name of a constructor.  *)
      val name : 'a t -> string

      val same : 'a t -> 'b t -> bool
      val same_witness : 'a t -> 'b t -> ('a,'b) Type_equal.t option
      val same_witness_exn : 'a t -> 'b t -> ('a,'b) Type_equal.t
      val typeid : 'a t -> typeid
    end


    (** Runtime parallel match.  *)
    module Match : sig
      (** This module can be used to handle several cases in parallel
          instead of using a sequence of nested matches or if/then/else
          chains.

          The combinators in the module are designed to be used as follows:

          {[
            let lift v = Match.(begin
                switch v @@
                case memory_load   (fun x -> `Load x)  @@
                case memory_store  (fun x -> `Store x) @@
                case register_read (fun x -> `Read x)  @@
                default (fun () -> `Unknown)
              end)
          ]}

          Note: in the example, the whole expression will build and
          then match. In case when performance matter, and when there
          is more then one match, it is recommended to evaluate a
          matching object first, and return a function, that matches
          values. For this there is a [select] combinator:

          {[
            let lift =
              Match.(begin
                  select @@
                  case memory_load   (fun x -> `Load x)  @@
                  case memory_store  (fun x -> `Store x) @@
                  case register_read (fun x -> `Read x)  @@
                  default (fun () -> `Unknown)
                end)
          ]}

      *)
      type 'a t

      (** [switch x matcher] applies [matcher] to value [x]  *)
      val switch : value -> 's t -> 's

      (** [select matcher x] applies [matcher] to value [x].
          [select] is the same as [Fn.flip switch].      *)
      val select : 's t -> value -> 's

      (** [case tag action matcher] adds an [action] to [matcher] that
          will be invoked for values with a a given [tag] *)
      val case : 'a tag -> ('a -> 's) -> 's t -> 's t

      (** [default def] creates an empty matcher with default handler [def]. *)
      val default : (unit -> 's) -> 's t
    end

    (** Persistent type identifiers.  *)
    module Typeid : Identifiable with type t = typeid

    (** Although values of type [value] implements regular interface
        it is recommended to used [dict] data structure instead of
        those, that are provided by [Regular] interface.x *)
    include Regular with type t := t
  end

  type 'a tag = 'a Value.tag

  (** Universal Heterogeneous Map.  *)
  module Dict : sig
    (** The dictionary can store values of arbitrary type. Only one
        value of a a given tag can be stored in the map. For example, if
        you have tag [cconv] (calling convention) then it is
        guaranteed that in map there is zero or one value with this
        tag. *)

    (** type of map *)
    type t = dict [@@deriving bin_io, compare, sexp]

    (** an empty instance  *)
    val empty : t

    (** [is_empty map] true if is empty. *)
    val is_empty : t -> bool

    (** [set map tag x] inserts or update  *)
    val set : t -> 'a tag -> 'a -> t

    (** [mem map tag] checks membership  *)
    val mem : t -> 'a tag -> bool

    (** [find map tag] lookups value  *)
    val find : t -> 'a tag -> 'a option

    (** [add map tag x] adds new value  *)
    val add : t -> 'a tag -> 'a -> [`Ok of t | `Duplicate]

    (** [change map tag f] changes value.  *)
    val change : t -> 'a tag -> ('a option -> 'a option) -> t

    (** [remove map tag] returns a map without a value associated
        with [tag]  *)
    val remove : t -> 'a tag -> t

    (** [to_sequence dict] is a sequence of all tid value
        entries  *)
    val to_sequence : t -> (Value.typeid * value) seq

    (** [data dict] is a sequence of all dict elements  *)
    val data : t -> value seq
  end

  (** {{!Vector}Resizable array}  *)
  type 'a vector

  (** Resizable Array.

      Resizable arrays with a logarithmic push_back in the style of
      C++. A user need to provide a default value (c.f.,
      DefaultConstructible requirement in C++ version). *)
  module Vector : sig
    (** a type of vector holding elements of type ['a]  *)
    type 'a t = 'a vector [@@deriving bin_io, compare, sexp]

    (** [create ?capacity default] creates an empty vector with a a given
        [capacity]. It is guaranteed that the default value will never
        be seen by the user unless he put it into the vector explicitely
        with [append] or [set].
    *)
    val create : ?capacity:int -> 'a -> 'a t

    (** [append xs x] appends [x] to the end of [xs]  *)
    val append : 'a t -> 'a -> unit

    (** [nth vec n] returns [n]'th element of vector [vec] *)
    val nth : 'a t -> int -> 'a option

    (** [get vec n] like [nth] but raises exception if index is out of
        bounds *)
    val get : 'a t -> int -> 'a

    (** [set vec n x] sets [n]'th element of a vector [vec] to [x] if
        [n < length vec] then raises exception *)
    val set : 'a t -> int -> 'a -> unit

    (** [map_to_array xs ~f] copies data from [xs] to an array applying
        [f] to each element. See also [to_array] function from
        [Container.S1] interface *)
    val map_to_array : 'a t -> f:('a -> 'b) -> 'b array


    val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
    val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
    val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

    val index : ?equal:('a -> 'a -> bool) -> 'a t -> 'a -> int option
    val index_exn : ?equal:('a -> 'a -> bool) -> 'a t -> 'a -> int
    val index_with : ?equal:('a -> 'a -> bool) -> default:int -> 'a t -> 'a -> int


    (** implements common accessors for the array, like [find], [fold],
        [iter], etc  *)
    include Container.S1 with type 'a t := 'a t

    val pp : 'a printer -> 'a t printer
  end


  (** BAP IR.

      Program is a tree of terms.
  *)
  type 'a term [@@deriving bin_io, compare, sexp]

  type program [@@deriving bin_io, compare, sexp]
  type sub [@@deriving bin_io, compare, sexp]
  type arg [@@deriving bin_io, compare, sexp]
  type blk [@@deriving bin_io, compare, sexp]
  type phi [@@deriving bin_io, compare, sexp]
  type def [@@deriving bin_io, compare, sexp]
  type jmp [@@deriving bin_io, compare, sexp]
  type nil [@@deriving bin_io, compare, sexp]

  type tid [@@deriving bin_io, compare, sexp]
  type call [@@deriving bin_io, compare, sexp]

  (** target of control transfer  *)
  type label =
    | Direct of tid             (** direct jump  *)
    | Indirect of exp           (** indirect jump  *)
    [@@deriving bin_io, compare, sexp]

  (** control transfer variants  *)
  type jmp_kind =
    | Call of call              (** call to subroutine          *)
    | Goto of label             (** jump inside subroutine      *)
    | Ret  of label             (** return from call to label   *)
    | Int  of int * tid         (** interrupt and return to tid *)
    [@@deriving bin_io, compare, sexp]

  (** argument intention  *)
  type intent =
    | In                        (** input argument  *)
    | Out                       (** output argument *)
    | Both                      (** input/output    *)
    [@@deriving bin_io, compare, sexp]

  type ('a,'b) cls

  (** {4 Term type classes}  *)

  val program_t : (nil, program) cls (** program  *)
  val sub_t : (program, sub) cls (** sub  *)
  val arg_t : (sub, arg) cls     (** arg  *)
  val blk_t : (sub, blk) cls     (** blk  *)
  val phi_t : (blk, phi) cls     (** phi  *)
  val def_t : (blk, def) cls     (** def  *)
  val jmp_t : (blk, jmp) cls     (** jmp  *)

  (** BIR Interpreter  *)
  module Biri : sig
    open Bil.Result

    (** Biri evaluates terms in the context of a whole program (since
        terms may contain calls and jumps).
        Biri also tracks for current position inside block, the block
        and preceding block.
    *)

    class context : ?main : sub term -> program term ->  object('s)
        inherit Expi.context
        method program : program term
        method main : sub term option
        method trace : tid list
        method enter_term : tid -> 's
        method set_next : tid option -> 's
        method next : tid option
      end

    (** base class for BIR interpreters  *)
    class ['a] t : object
      constraint 'a = #context
      inherit ['a] expi

      (** called for each term, just after the position is updated,
          but before any side effect of term evaluation had occurred.*)
      method enter_term : 't 'p . ('p,'t) cls -> 't term -> 'a u


      method eval : 't 'p . ('p,'t) cls -> 't term -> 'a u

      (** called after all side effects of the term has occurred  *)
      method leave_term : 't 'p . ('p,'t) cls -> 't term -> 'a u

      (** Evaluates a subroutine with the following algorithm:

          0. next <- first block of subroutine and goto 1
          1. eval all in and in/out arguments and goto 2
          2. if next is some blk then eval it and goto 2 else goto 3
          3. if next is some sub then eval it and goto 2 else goto 4
          4. eval all out and in/out arguments.
      *)
      method eval_sub : sub term -> 'a u

      (** evaluate argument by first evaluating its right hand side,
          and then assigning the result to the left hand side.*)
      method eval_arg : arg term -> 'a u

      (** evaluate all terms in a given block, starting with phi
          nodes, then proceeding to def nodes and finally evaluating
          all jmp terms until either jump is taken or jump condition
          is undefined.
          After the evaluation the context#next will point next
          destination.  *)
      method eval_blk : blk term -> 'a u

      (** evaluate definition by assigning the result of the right
          hand side to the definition variable  *)
      method eval_def : def term -> 'a u

      (** based on trace select an expression and assign its
          value to the left hand side of phi node.   *)
      method eval_phi : phi term -> 'a u

      (** evaluate condition, and if it is false, then do nothing,
          otherwise evaluate jump target (see below) *)
      method eval_jmp : jmp term -> 'a u

      (** evaluate label, using [eval_direct] or [eval_indirect], based
          on the label variant *)
      method eval_goto : label -> 'a u

      (** evaluate target label, using [eval_direct] or
          [eval_indirect], based on the label variant.
          Ignores return label.  *)
      method eval_call : call -> 'a u

      (** evaluate label, using [eval_direct] or [eval_indirect], based
          on the label variant *)
      method eval_ret  : label -> 'a u

      (** ignore arguments and set context#next to None  *)
      method eval_exn  : int -> tid -> 'a u

      (** set context#next to the a given tid  *)
      method eval_direct : tid -> 'a u

      (** ignore argument and set context#next to None  *)
      method eval_indirect : exp -> 'a u
    end
  end

  (** BIR {{!Biri}interpreter}  *)
  class ['a] biri : ['a] Biri.t


  (** {3 Some predefined tags} *)

  type color = [
    | `black
    | `red
    | `green
    | `yellow
    | `blue
    | `magenta
    | `cyan
    | `white
    | `gray
  ] [@@deriving bin_io, compare, sexp]


  (** Color something with a color  *)
  val color : color tag


  (** print marked entity with the specified color.  (the same
      as color, but pretty printing function will output ascii escape
      sequence of corresponding color.  *)
  val foreground : color tag


  (** print marked entity with specified color. See [foreground].  *)
  val background : color tag

  (** A human readable comment *)
  val comment : string tag

  (** A command in python language *)
  val python : string tag

  (** A command in shell language *)
  val shell : string tag

  (** Mark something as marked *)
  val mark : unit tag

  (** Give a weight *)
  val weight : float tag

  (** The real virtual address of a target  *)
  val target_addr : addr tag

  (** Symbolic name of a target  *)
  val target_name : string tag

  (** Name of a subroutine  *)
  val subroutine_name : string tag

  (** Address of a subroutine entry point  *)
  val subroutine_addr : addr tag

  (** A name of a file  *)
  val filename : string tag

  (** an image loaded into memory  *)
  type image

  (** opaque memory  *)
  type mem [@@deriving sexp_of]

  (** a table from memory to ['a]  *)
  type 'a table [@@deriving sexp_of]

  (** interval trees from memory regions to ['a] *)
  type 'a memmap [@@deriving sexp_of]

  (** Iterators lifted into monad  *)
  module type Memory_iterators = sig
    type t
    type 'a m
    val fold     : ?word_size:size -> t -> init:'b -> f:(word -> 'b -> 'b m) -> 'b m
    val iter     : ?word_size:size -> t -> f:(word -> unit m) -> unit m
    val foldi    : ?word_size:size -> t -> init:'b -> f:(addr -> word -> 'b -> 'b m) -> 'b m
    val iteri    : ?word_size:size -> t -> f:(addr -> word -> unit m) -> unit m
    val exists   : ?word_size:size -> t -> f:(addr -> word -> bool m) -> bool m
    val for_all  : ?word_size:size -> t -> f:(addr -> word -> bool m) -> bool m
    val count    : ?word_size:size -> t -> f:(addr -> word -> bool m) -> int m
    val find_if  : ?word_size:size -> t -> f:(addr -> word -> bool m) -> word option m
    val find_map : ?word_size:size -> t -> f:(addr -> word -> 'a option m) ->
      'a option m
  end

  (** Memory region  *)
  module Memory : sig
    type t = mem [@@deriving sexp_of]

    val create
      : ?pos:int                    (** defaults to [0]  *)
      -> ?len:int                    (** defaults to full length  *)
      -> endian
      -> addr
      -> Bigstring.t -> t Or_error.t

    val of_file : endian -> addr -> string -> t Or_error.t

    (** [view word_size ~from ~words mem] returns a new memory
        that represents the specified region of memory [mem]. [copy]
        function performs deep copy.

        @param addr  defaults [min_addr mem]
        @param words defaults to the end of the memory region.
    *)
    val view : ?word_size:size -> ?from:addr -> ?words:int -> t -> t Or_error.t

    (** [range mem a0 a1] returns a view on [mem] starting from
        address [a0] and ending at [a1], bounds inclusive   *)
    val range : t -> addr -> addr -> t Or_error.t

    (** [merge m1 m2] takes two memory regions, that either intersects or
        share edges (i.e., difference between [min_addr] of one of the
        blocks and [max_addr] of another is less then or equal to one, and
        returns memory blocks that spans memory starting from the address
        {[min (min_addr m1) (min_addr m2)]} and ending with address
        {[max (max_addr m1) (max_addr m2)]}.

        Will return an error, if either the above state precondition
        doesn't hold, or if this two memory blocks doesn't share the same
        underlying memory (i.e., bases), or if they have different
        endianness.
    *)
    val merge : t -> t -> t Or_error.t

    (** [first_byte m] returns first byte of [m] as a memory  *)
    val first_byte : t -> t
    (** [last_byte m] returns last byte of [m] as a memory  *)
    val last_byte : t -> t

    (** returns the order of bytes in a word  *)
    val endian : t -> endian

    (** [get word_size mem addr] reads memory value from the specified
        address. [word_size] default to [`r8] *)
    val get : ?disp:int -> ?index:int -> ?scale:size -> ?addr:addr -> t -> word Or_error.t

    (** [m^n] dereferences a byte at address [n]  *)
    val (^) : t -> addr -> word Or_error.t

    (** [m^.n] dereferences a byte at address [n]  *)
    val (^!) : t -> addr -> word

    (** [{max,min}_addr] function specify upper and lower bounds of the memory *)
    val max_addr : t -> addr
    val min_addr : t -> addr

    (** [length] returns the length of the memory in bytes *)
    val length : t -> int

    (** [contains mem addr] returns true if [mem] contains address [addr]  *)
    val contains : t -> addr -> bool

    (** [compare_with mem addr] compares memory with [addr]  *)
    val compare_with : t -> addr -> [
        | `addr_is_inside
        | `addr_is_below
        | `addr_is_above
      ]

    (** A set of low level input operations.
        Note: it is more effective to use above head iterators, instead
        of this low level interface, since iterators do not need to check
        every memory access.  *)
    module Input : sig
      (** [reader mem ~pos_ref] defines a set of functions with a
          common interface. Each function accepts a memory [mem] and a
          [pos_ref] - a reference to a address that should be read. This
          reference will be updated for the amount of bytes that was
          actually read.

          @return a word lifted into a monad.
      *)
      type 'a reader = t -> pos_ref : addr ref -> 'a Or_error.t
      val word   : word_size:size -> word reader
      val int8   : word reader
      val uint8  : word reader
      val int16  : word reader
      val uint16 : word reader
      val int32  : word reader
      val int64  : word reader
    end

    (** {2 Printing and outputing}  *)

    include Printable with type t := t

    (** [hexdump t out] outputs hexdump (as per [hexdump -C]) of the
        memory to formatter [out]  *)
    val hexdump: t -> string

    (** a set of iterators, with identity monad.  *)
    include Memory_iterators with type t := t
                              and type 'a m = 'a

    (** iterators lifter to the Or_error monad  *)
    module With_error : Memory_iterators with type t := t
                                          and type 'a m = 'a Or_error.t

    (** lifts iterators to monad [M]  *)
    module Make_iterators( M : Monad.S )
      : Memory_iterators with type t := t
                          and type 'a m = 'a M.t


    (** {2 Interfacing with C}

        The following interfaces is supposed to be used only for the
        purposes of exposing memory to c programs. *)

    (** [to_buffers mem] creates a buffer representing the memory [mem].
        It is not specified whether the returned buffer has some sharing
        with underlying implementation. In other words the returned buffer
        shouldn't be modified.

        Since it is not guaranteed that memory is contiguous, a sequence of
        buffers is returned, with each buffer representing a contiguous
        part of memory.

    *)
    val to_buffer : t -> Bigsubstring.t


    (** Tries over memory  *)
    module Trie : sig
      module R8  : Trie with type key = t
      module R16 : Trie with type key = t
      module R32 : Trie with type key = t
      module R64 : Trie with type key = t
    end
  end

  (** Table.

      Tables are used to partition memory region into a set of
      non-intersecting areas. Each area is assosiated with arbitrary
      value of type ['a] bound to the type of the table.

      All operations over tables are purely applicative, i.e. there is
      no observable side-effects. Although, they employ some kind of
      caching underneath the hood, so that they perform better if
      they're  build once and used many times.

      Tables can be also linked. For example, if you have two tables
      mapping the same memory region to a different sets of values, you
      can create a mapping from one set of values to another. See [link]
      function for mode details. *)
  module Table : sig
    type 'a t = 'a table [@@deriving sexp_of]
    type 'a hashable = 'a Hashtbl.Hashable.t

    (** creates an empty table  *)
    val empty : 'a t

    (** creates a table containing one bindins  *)
    val singleton : mem -> 'a -> 'a t

    (** [add table mem v] returns a new table with added mapping from a
        mem region [mem] to a data value [v] *)
    val add : 'a t -> mem -> 'a -> 'a t Or_error.t

    (** returns a new table with all mappings from the mem region
        [mem] removed *)
    val remove : 'a t -> mem -> 'a t

    (** [change tab mem ~f] function [f] is applied to a set of all memory
        regions that intersects with [mem]. If function [f] evaluates to
        [`remap (new_mem,y)] then all memory regions that have had
        intersections with [mem] will be removed from the new map and
        memory region [new_mem] will be mapped to [y]. If [f] evaluates to
        [`remove], then the regions will be removed, and nothing will be
        added. If it evaluates to [`skip] then the table will be returned
        unchanged.  Intersections are passed sorted in an ascending order.
    *)
    val change : 'a t -> mem -> f:((mem * 'a) seq -> [
        | `rebind of mem * 'a         (** add new mapping instead  *)
        | `update of ((mem * 'a) -> 'a) (** update all bindings      *)
        | `remove                    (** remove all bindings      *)
        | `ignore])                  (** don't touch anything     *)
      -> 'a t

    (** [length table] returns a number of entries in the table  *)
    val length : 'a t -> int

    (** [find table mem] finds an element mapped to the memory region [mem]  *)
    val find : 'a t -> mem -> 'a option

    (** [find_addr tab addr] finds a memory region that contains a
        specified [addr]   *)
    val find_addr : 'a t -> addr -> (mem * 'a) option

    (** [intersections table mem] returns all mappings in a [table] that
        have intersections with [mem] *)
    val intersections : 'a t -> mem -> (mem * 'a) seq

    (** [fold_intersections table mem] folds over all regions
        intersecting with [mem] *)
    val fold_intersections : 'a t -> mem -> init:'b -> f:(mem -> 'a -> 'b -> 'b) -> 'b

    (** [has_intersections tab mem] is true iff some portion of [mem] is
        is already mapped in [tab]. *)
    val has_intersections : 'a t -> mem -> bool

    (** [mem table mem] is true if table contains mem region [mem]  *)
    val mem : _ t -> mem -> bool

    (** [next table elt] returns element next to [elt], if any *)
    val next : 'a t -> mem -> (mem * 'a) option

    (** [next table elt] returns element preceding to [elt], if any *)
    val prev : 'a t -> mem -> (mem * 'a) option

    (** [min tab] return the lowest binding  *)
    val min : 'a t -> (mem * 'a) option

    (** [max tab] return the highest binding  *)
    val max : 'a t -> (mem * 'a) option

    (** Relation multiplicity.
        For a given type ['a] creates type ['m]
    *)
    type ('a,'m) r

    (** {2 Table relations}  *)

    (** [0..*]  *)
    val many : ('a, 'a seq) r

    val at_least_one : ('a, 'a * 'a seq) r

    (** [1..1]     *)
    val one : ('a, 'a) r

    (** [0..1]  *)
    val maybe_one : ('a, 'a option) r


    (** [link relation t t1 t2] takes two tables and returns a mapping
        from elements of one table to elements of other table.

        Parameter [t] specifies a [hashable] typeclass of the type ['a]. If
        type ['a] implements [Hashable] interface, then you can obtain it
        with [hashable] function, e.g. [Int.hashable] with return the
        appropriate type class. If ['a] doesn't implement [Hashable], then
        it can be implemented manually.

        Relation specifies the multiplicity of the relation between
        entities from table [t1] to entities from table [t2], and is
        summarized below:

        - [one_to_many] means that a particular region from table [t1] can
        span several memory regions from table [t2]. Example: segments
        to symbols relation.

        - [one_to_one] means that for each value of type ['a] there is
        exactly one value of type ['b]. This relation should be used with
        caution, since it is quantified over _all_ values of type
        ['a]. Indeed, it should be used only for cases, when it can be
        guaranteed, that it is impossible to create such value of type
        ['b], that has no correspondence in table [t2]. Otherwise,
        [one_to_maybe_one] relation should be used. Example: llvm
        machine code to assembly string relation.

        - [one_to_maybe_one] means that for each value in table [t1] there
        exists at most one value in table [t2]. Example: function to
        symbol relation.

        {3 Examples}

        {[
          let mc_of_insn  = link one_to:one Insn.hashable insns mcs
          let syms_of_sec = link one_to:many Sec.hashable  secs syms
        ]} *)

    val link : one_to:('b,'r) r -> 'a hashable -> 'a t -> 'b t -> 'a -> 'r


    (** [rev_map arity t tab] creates a reverse mapping from values of
        typeclass [t] stored in table [tab] to memory regions.

        Note. not every mapping is reversable, for example, trying to obtain
        a reverse of surjective mapping as a one-to-one mapping will
        result in an error. But surjective mappings can be reversed
        using [~one_to:many] mapping. A particular example of surjective
        mapping is [symbol] tables, in a case when functions can occupy
        several non-contiguous regions of memory.

        For example, to create a mapping from a function symbol to
        sequence of memory regions with it code:

        {[rev_map one_to:many Sym.hashable tab]}

    *)
    val rev_map : one_to:(mem,'r) r -> 'a hashable -> 'a t -> ('a -> 'r) Or_error.t

    (** {2 Iterators}

        This section provides a common set of iterators. Note: name
        iterator is used in a functional meaning, i.e., an iterator is a
        function that takes a data structure and another function, and
        applies it to all elements in some manner.

        All iterators share some common part of interface that was lifted
        to a ['a ranged] type. When you see

        [('a t -> f:('a -> bool) -> bool) ranged]

        just mentally substitute it with:

        [?start -> ?until -> 'a t -> f:('a -> bool) -> bool].

        In other words ['f ranged] just prepends [?start -> ?until ->] to
        function with type ['f] (do not forget that ['f] can be an arrow
        type).

        [start] and [until] parameters allows to narrow iteration to some
        subset of table. If they are unspecified then iteration would be
        performed on all table entries in an ascending order of
        addresses. If they are specified, then if [start <= until], then
        iteration will be performed in the same order but on a specified
        subset. In the case, when [start > until], iteration will be
        performed in a decreasing order.
    *)
    type 'a ranged
      = ?start:mem   (** defaults to the lowest mapped region *)
      -> ?until:mem   (** defaults to the highest mapped area  *)
      -> 'a

    val exists   : ('a t -> f:(mem -> 'a -> bool) -> bool) ranged
    val for_all  : ('a t -> f:(mem -> 'a -> bool) -> bool) ranged
    val exists   : ('a t -> f:(      'a -> bool) -> bool) ranged
    val for_all  : ('a t -> f:(      'a -> bool) -> bool) ranged

    val count    : ('a t -> f:('a -> bool) -> int) ranged
    val find_if  : ('a t -> f:('a -> bool) -> 'a option) ranged
    val find_map : ('a t -> f:('a -> 'b option) -> 'b option) ranged
    val fold  : ('a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b) ranged
    val iter  : ('a t -> f:('a -> unit) -> unit) ranged

    val find_mapi : ('a t -> f:(mem -> 'a -> 'b option) -> 'b option) ranged
    val foldi: ('a t -> init:'b -> f:(mem -> 'a -> 'b -> 'b) -> 'b) ranged
    val iteri : ('a t -> f:(mem -> 'a -> unit) -> unit) ranged

    val map : ('a t -> f:('a -> 'b) -> 'b t) ranged
    val mapi : ('a t -> f:(mem -> 'a -> 'b) -> 'b t) ranged

    (** removes all mappings that do not satisfy the predicate  *)
    val filter : ('a t -> f:('a -> bool) -> 'a t) ranged
    val filter_map : ('a t -> f:('a -> 'b option) -> 'b t) ranged

    val filteri : ('a t -> f:(mem -> 'a -> bool) -> 'a t) ranged
    val filter_mapi : ('a t -> f:(mem -> 'a -> 'b option) -> 'b t) ranged


    (** [to_sequence tab] converts the table [t] to a
        sequence of key-value pairs.  *)
    val to_sequence : ('a t -> (mem * 'a) seq) ranged


    (** [regions table] returns in an ascending order of addresses all
        memory regions mapped in a [table] *)
    val regions : ('a t -> mem seq) ranged

    (** [regions table] returns in an ascending order of addresses all
        elements mapped in a [table] *)
    val elements : ('a t -> 'a seq) ranged

    val pp : 'a printer -> 'a t printer
  end

  (** A locations of a chunk of memory  *)
  module Location : sig
    type t = {
      addr : addr;
      len  : int;
    } [@@deriving bin_io, compare, fields, sexp]
  end

  (** memory location  *)
  type location = Location.t [@@deriving bin_io, compare, sexp]

  (** A backend interface.

      This interface must be implemented by a backend plugin, and
      registered with [Image.register] function in order to be
      accessible for loading images.*)

  module Backend : sig

    (** memory access permissions  *)
    type perm = R | W | X | Or of perm * perm
      [@@deriving bin_io, compare, sexp]

    (** A named contiguous part of file with permissions.
        Also, known as segment in ELF.    *)
    module Segment : sig
      type t = {
        name: string;
        perm: perm;         (** segment's permissions  *)
        off: int;
        location : location;
      } [@@deriving bin_io, compare, fields, sexp]
    end

    (** Symbol definition, that can span several non-contiguous parts of
        memory *)
    module Symbol : sig
      type t = {
        name : string;
        is_function : bool;
        is_debug : bool;
        locations : location * location list;
      } [@@deriving bin_io, compare, fields, sexp]
    end

    (** Just a named region of memory.  *)
    module Section : sig
      type t = {
        name : string;
        location : location;
      } [@@deriving bin_io, compare, fields, sexp]
    end

    (** A Img from a backend perspective.  *)
    module Img : sig
      type t = {
        arch     : arch;
        entry    : addr;
        segments : Segment.t * Segment.t list;
        symbols  : Symbol.t list;
        sections  : Section.t list;
      } [@@deriving bin_io, compare, fields, sexp]
    end

    (** the actual interface to be implemented  *)
    type t = Bigstring.t -> Img.t option
  end


  (** Binary Image.  *)
  module Image : sig
    (** {2 Type definitions}  *)

    type t = image [@@deriving sexp_of]            (** image   *)

    (** segment *)
    type segment [@@deriving bin_io, compare, sexp]
    (** symbol  *)
    type symbol [@@deriving bin_io, compare, sexp]

    type path = string

    (** {2 Constructing}  *)

    (** constructing an image can result in actual image and a set
        (hopefully empty) of errors occured in a process of decoding an
        image, that do not prevent us from actually creating an image. So,
        this information messages can be considered as warnings. *)
    type result = (t * Error.t list) Or_error.t

    (** [create ?backend filename] creates an image of the file specified
        specified by the [filename]. If [backend] is equal to "auto", then
        all backends are tried in order. If only one backend can read this
        file (i.e., there is no ambiguity), then image is returned. If
        [backend] is not specifed, then the LLVM backend is used. *)
    val create : ?backend:string -> path -> result

    (** [of_string ?backend ~data] creates an image from the specified
        [data]. See {!create} for [backend] parameter. *)
    val of_string : ?backend:string -> string -> result

    (** [of_bigstring ?backend ~data] creates an image from the specified
        [data]. See {!create} for [backend] parameter. *)
    val of_bigstring : ?backend:string -> Bigstring.t -> result


    (** {2 Attributes}  *)

    val entry_point : t -> addr
    val filename : t -> string option
    val arch: t -> arch
    val addr_size : t -> addr_size
    val endian : t -> endian

    val data : t -> Bigstring.t

    (** {2 Tables }  *)
    val words : t -> size -> word table
    val segments : t -> segment table
    val symbols : t -> symbol table

    (** {2 Tags}  *)
    val segment : segment tag
    val symbol  : string tag
    val section : string tag

    (** returns memory, annotated with tags  *)
    val memory : t -> value memmap

    (** {2 Mappings }  *)
    val memory_of_segment  : t -> segment -> mem
    (** [memory_of_symbol sym]: returns the memory of symbol in acending order. *)
    val memory_of_symbol   : t -> symbol -> mem * mem seq
    val symbols_of_segment : t -> segment -> symbol seq
    val segment_of_symbol  : t -> symbol -> segment

    (** Image Segments.
        Segment is a contiguous region of memory that has
        permissions. The same as segment in ELF.    *)
    module Segment : sig
      type t = segment
      include Regular with type t := t
      val name : t -> string
      val is_writable   : t -> bool
      val is_readable   : t -> bool
      val is_executable : t -> bool
    end

    (** Symbol.  *)
    module Symbol : sig
      type t = symbol
      include Regular with type t := t
      val name : t -> string
      val is_function : t -> bool
      val is_debug : t -> bool
    end

    (** {2 Backend Interface}  *)

    (** [register_backend ~name backend] tries to register backend under
        the specified [name]. *)
    val register_backend : name:string -> Backend.t -> [ `Ok | `Duplicate ]

    (** lists all registered backends  *)
    val available_backends : unit -> string list

  end

  (** Memory maps.
      Memory map is an assosiative data structure that maps memory
      regions to values. Unlike in the Table, memory
      regions in the Memmap can intersect in an arbitrary ways. This
      data structure is also known as Interval Tree or Segmented Tree.

      Underneath the hood it is implemented using augumented AVL tree,
      so that all operations are logarithmic. *)
  module Memmap : sig

    (** memory map, aka interval trees  *)
    type 'a t = 'a memmap [@@deriving sexp_of]

    (** [empty] map  *)
    val empty : 'a t

    (** [singleton] a memory map containing only one memory region  *)
    val singleton : mem -> 'a -> 'a t

    (** [min_addr map] is a minimum addr mapped in [map] *)
    val min_addr : 'a t -> addr option

    (** [max_addr map] is a maximum addr mapped in [map] *)
    val max_addr : 'a t -> addr option

    (** [min_binding map] is a minimum binding mapped in [map] *)
    val min_binding : 'a t -> (mem * 'a) option

    (** [max_binding map] is a maximum binding mapped in [map] *)
    val max_binding : 'a t -> (mem * 'a) option

    (** [add map mem tag] adds a new memory region [mem] tagged with
        [tag]. If the same region was already in the [map] it will be
        tagged with the [tag] again, even if it has had the same tag. *)
    val add : 'a t -> mem -> 'a -> 'a t

    (** [dominators map mem] an ordered sequence of all memory regions,
        containing [mem]. A memory region [(x,y)] contains region [(p,q)],
        iff [p >= x && q <= y], where memory regions are depicted using
        closed intervals. *)
    val dominators : 'a t -> mem -> (mem * 'a) seq

    (** [intersections map mem] an ordered sequence of all memory regions,
        that intersects with [mem]. Memory region [(x,y)] intersects with
        region [(p,q)] iff there exists such [z] that

        [z >= p || z <= q && z >= x && z <= y].

        In other words if there exists such byte that belongs to both memory
        regions. *)
    val intersections : 'a t -> mem -> (mem * 'a) seq

    (** [intersects map mem] is true if [intersections map mem] is not empty *)
    val intersects : 'a t -> mem -> bool

    (** [dominates map mem] if there is a non empty set of dominators  *)
    val dominates : 'a t -> mem -> bool

    (** [contains map addr] true if there exists such memory region [mem],
        that [Memory.contains mem addr] *)
    val contains : 'a t -> addr -> bool

    (** [lookup map addr] returns an ordered sequence of all memory
        containing the [addr] *)
    val lookup : 'a t -> addr -> (mem * 'a) seq

    (** [map m f] returns a new map with each tag mapped
        with function [f] *)
    val map : 'a t -> f:('a -> 'b) -> 'b t

    (** [mapi m f] the same as [map], but [f] is called with two
        arguments: [mem] and [tag], where [mem] is a memory region,
        and [tag] is a [tag] assosiated with that region. *)
    val mapi : 'a t -> f:(mem -> 'a -> 'b) -> 'b t

    (** [filter map f] returns a map that contains only those elements
        for which [f] evaluated to [true] *)
    val filter : 'a t -> f:('a -> bool) -> 'a t

    (** [filter_map m f] creates a new map by applying a function [f] to
        each tag. If [f] returns [Some x] then this region will be mapped
        to [x] in a new map, otherwise it will be dropped. *)
    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

    (** [filter_mapi] is like [filter_map] but use function also accepts
        would assosiated memory region *)
    val filter_mapi : 'a t -> f:(mem -> 'a -> 'b option) -> 'b t

    (** [remove map mem] removes all bindings to [mem]  *)
    val remove : 'a t -> mem -> 'a t

    (** [remove_intersections map mem] removes all bindings that
        that intersects with [mem] *)
    val remove_intersections : 'a t -> mem -> 'a t

    (** [remove_dominators map mem] removes all bindings that are
        dominators to [mem] *)
    val remove_dominators : 'a t -> mem -> 'a t

    (** [to_sequence map] converts the memmap ['a t] to a sequence of
        key-value pairs *)
    val to_sequence : 'a t -> (mem * 'a) seq

    include Container.S1 with type 'a t := 'a t

    val pp : 'a printer -> 'a t printer
  end


  (** Symbolizer defines a method for assigning symbolic names to addresses  *)
  type symbolizer

  (** Rooter defines a method for finding function starts in a program  *)
  type rooter

  (** Brancher defines a method for resolving branch instruction   *)
  type brancher

  (** Reconstructor  defines a method for reconstructing symbol tables   *)
  type reconstructor

  (** value of type [disasm] is a result of the disassembling of a
      memory region.  *)
  type disasm

  (** values of type [insn] represents machine instructions decoded
      from the a given piece of memory *)
  type insn [@@deriving bin_io, compare, sexp_of]

  (** [block] is a region of memory that is believed to be a basic block
      of control flow graph to the best of our knowledge. *)
  type block [@@deriving compare, sexp_of]

  type cfg [@@deriving compare]

  (** a jump kind.
      A jump to another block can be conditional or unconditional.
  *)
  type jump = [
    | `Jump     (** unconditional jump                  *)
    | `Cond     (** conditional jump                    *)
  ] [@@deriving compare, sexp]
  (** This type defines a relation between two basic blocks.  *)
  type edge = [jump | `Fall] [@@deriving compare, sexp]


  (** Kinds of instructions  *)
  module Kind : sig
    type branch = [
      | `Conditional_branch
      | `Unconditional_branch
      | `Indirect_branch
    ] [@@deriving bin_io, compare, enumerate, sexp]

    type affecting_control = [
      | branch
      | `Return
      | `Call
      | `Barrier
      | `Terminator
      | `May_affect_control_flow
    ] [@@deriving bin_io, compare, enumerate, sexp]

    type having_side_effect = [
      | `May_load
      | `May_store
    ] [@@deriving bin_io, compare, enumerate, sexp]

    type t = [
      | affecting_control
      | having_side_effect
    ] [@@deriving bin_io, compare, enumerate, sexp]
  end

  (** abstract and opaque register  *)
  type reg [@@deriving bin_io, compare, sexp]

  (** opaque immediate value  *)
  type imm [@@deriving bin_io, compare, sexp]

  (** floating point value  *)
  type fmm [@@deriving bin_io, compare, sexp]

  (** kind of instruction  *)
  type kind = Kind.t [@@deriving bin_io, compare, sexp]


  (** Register.  *)
  module Reg : sig
    type t = reg

    (** unique number representing a register  *)
    val code : t -> int

    (** name of a register  *)
    val name : t -> string
    include Regular with type t := t
  end


  (** Integer immediate operand  *)
  module Imm : sig
    type t = imm
    val to_word  : t -> width:int -> word option
    val to_int64 : t -> int64
    val to_int   : t -> int option
    include Regular with type t := t
  end

  (** Floating point immediate operand  *)
  module Fmm : sig
    type t = fmm
    val to_float : t -> float
    include Regular with type t := t
  end

  (** Operand *)
  module Op : sig
    (** operand *)
    type t =
      | Reg of reg
      | Imm of imm
      | Fmm of fmm
      [@@deriving bin_io, compare, sexp]
    (** Normalized comparison.  *)
    module Normalized : sig
      val compare : t -> t -> int
      val hash : t -> int
      val compare_ops : t array -> t array -> int
    end

    val pp_adt : Format.formatter -> t -> unit
    include Regular with type t := t
  end

  type op = Op.t [@@deriving bin_io, compare, sexp_of]

  (** Expert interface to disassembler.

      This interface is rather complicated, and is built around two
      implementations of the disassembler [Basic] and [Recursive].
      [Basic] provides an efficient (and very lazy) linear sweep,
      driven in a continuation passing style. On top of the [Basic]
      the [Recursive] disassembler is built, that reconstructs the
      control flow graph, and represents the latter as a table of
      blocks. *)
  module Disasm_expert : sig
    (** Basic disassembler.

        This is a target agnostic basic low-level disassembler. *)
    module Basic : sig
      (** predicate to drive the disassembler *)
      type pred = [
        | `Valid    (** stop on first valid insn  *)
        |  Kind.t   (** stop on first insn of the specified kind *)
      ] [@@deriving sexp]

      (** {2 Basic types }  *)
      type (+'a,+'k) insn
      type (+'a,+'k) insns = (mem * ('a,'k) insn option) list
      type empty     (** set when information is not stored                *)
      type asm       (** set when assembler information is stored        *)
      type kinds     (** set when instruction kind information is stored *)

      type full_insn = (asm,kinds) insn [@@deriving compare, sexp_of]

      (** Disassembler.

          ['a] and ['k] type variables specify disassembler modes of
          operation. In a process of disassembly it can store extra
          information that might be useful. Although, since storing it
          takes extra time and space, it is disabled by default.

          The first type variable specifies whether storing assembly strings
          is enabled. It can be switched using [store_asm], [drop_asm]
          functions. When it is enabled, then this type variable will be set
          to [asm], and it will give an access to functions that returns
          this information. Otherwise, this type variable will be set to
          [empty], thus stopping you from accessing assembler information.

          The second type variable stands for [kinds], i.e. to store or not to
          store extra information about instruction kind.

          Note: at some points you can have an access to this information
          even if you don't enable it explicitly.
      *)
      type ('a,'k) t

      (** Disassembler state.

          Words of precautions: this state is valid only inside handlers
          functions of the [run] function. It shouldn't be stored
          anywhere.
          First two type variables are bound correspondingly to two
          variables of the disassmbler [('a,'k) t] type. Third type variable,
          is for user data type, that can be used to pass extra information
      *)
      type (+'a,+'k,'s,'r) state

      (** [with_disasm ?debug_level ?cpu ~backend target] creates a
          disassembler for the specified [target]. All parameters are
          backend specific, consult the concrete backend for more
          information. In general, the greater [debug_level] is, the
          more debug information will be outputed by a backend. To
          silent backend set it [0]. This is a default value. Example:

          [with_disasm ~debug_level:3 ~backend:"llvm" "x86_64" ~f:process]
      *)
      val with_disasm :
        ?debug_level:int -> ?cpu:string -> backend:string -> string ->
        f:((empty, empty) t -> 'a Or_error.t) -> 'a Or_error.t


      (** enables storing assembler information  *)
      val store_asm : (_,'k) t -> (asm,'k) t

      (** enables storing instruction kinds information *)
      val store_kinds : ('a,_) t -> ('a,kinds) t

      (** [run ?stop_on ?invalid ?stopped dis mem ~init ~return ~hit]
          performs recursive disassembly of specified memory [mem]. The
          process of disassembly can be driven using [stop], [step], [back]
          and [jump] functions, described later.

          @param backlog defines a size of history of states, that can
          be used for backtracking. Defaults to some positive natural
          number.

          @param stop_on defines a set of predicates that will be checked
          on each step to decide whether it should stop here and call a
          user-provided [hit] function, or it should continue. The descision
          is made acording to the rule: [if exists stop_on then stop], i.e.,
          it there exists such predicate in a set of predicates, that
          evaluates to true, then stop the disassembly and pass the control
          to the user function [hit].  A few notes: only valid instructions
          can match predicates, and if the set is empty, then it always
          evaluates to false.

          @param init initial value of user data, that can be passed
          through handlers (cf., [fold])

          @param return a function that lifts user data type ['s] to type
          ['r]. It is useful when you need to perform disassembly in some
          monad, like [Or_error], or [Lwt]. Otherwise, just use [ident]
          function and assume that ['s == 'r].

          In a process of disassembly user provided callbacks are invoked by
          the engine. To each callback at least two parameters are passed:
          [state] and [user_data]. [user_data] is arbitrary data of type ['s]
          with which the folding over the memory is actually
          performed. [state] incapsulates the current state of the
          disassembler, and provides continuation functions, namely [stop],
          [next] and [back], that drives the process of disassembly. This
          functions are used to pass control back to the disassembler.

          [stopped state user_data] is called when there is no more data to
          disassemble. This handler is optional and defaults to [stop].

          [invalid state user_data] is an optional handler that is called on
          each invalid instruction (i.e., a portion of data that is not a
          valid instruction), it defaults to [step], i.e., to skipping.

          [hit state mem insn data] is called when one of the predicates
          specifed by a user was hit. [insn] is actually the instruction
          that satisfies the predicate. [mem] is a memory region spanned by
          the instruction. [data] is a user data. [insn] can be queried for
          assembly string and kinds even if the corresponding modes are
          disabled.  *)
      val run :
        ?backlog:int ->
        ?stop_on:pred list ->
        ?invalid:(('a,'k,'s,'r) state -> mem -> 's -> 'r) ->
        ?stopped:(('a,'k,'s,'r) state -> 's -> 'r) ->
        ?hit:(('a,'k,'s,'r) state -> mem -> (asm,kinds) insn -> 's -> 'r) ->
        ('a,'k) t ->
        return:('s -> 'r) ->
        init:'s -> mem -> 'r

      (** [insn_of_mem dis mem] performes a disassembly of one instruction
          from the a given memory region [mem]. Returns a tuple
          [imem,insn,`left over] where [imem] stands for a piece of memory
          consumed in a process of disassembly, [insn] can be [Some ins] if
          disassembly was successful, and [None] otherwise. [`left over]
          complements [imem] to original [mem]. *)
      val insn_of_mem : (_,_) t -> mem ->
        (mem * (asm,kinds) insn option * [`left of mem | `finished]) Or_error.t

      (** current position of the disassembler  *)
      val addr : (_,_,_,_) state -> addr

      (** current set of predicates *)
      val preds : (_,_,_,_) state -> pred list

      (** updates the set of predicates, that rules the stop condition.  *)
      val with_preds : ('a,'k,'s,'r) state -> pred list -> ('a,'k,'s,'r) state

      (** a queue of instructions disassembled in this step  *)
      val insns : ('a,'k,_,_) state -> ('a,'k) insns

      (** [last s n] returns last [n] instructions disassembled in this
          step. If there are less then [n] instructions, then returns a
          smaller list *)
      val last : ('a,'k,'s,'r) state -> int -> ('a,'k) insns

      (** the memory region we're currently working on *)
      val memory : (_,_,_,_) state -> mem

      (** stop the disassembly and return the provided value.  *)
      val stop : (_,_,'s,'r) state -> 's -> 'r

      (** continue disassembling from the current point. You can change a
          a set of predicates, before stepping next. If you want to continue
          from a different address, use [jump] *)
      val step : (_,_,'s,'r) state -> 's -> 'r

      (** jump to the specified memory and continue disassembly in it.

          For example, if you want to jump to a specified address, and
          you're working in a [Or_error] monad, then you can:

          [view ~from:addr (mem state) >>= fun mem -> jump mem data]
      *)
      val jump : (_,_,'s,'r) state -> mem -> 's -> 'r

      (** restarts last step.   *)
      val back : (_,_,'s,'r) state -> 's -> 'r

      (** Basic instruction.
          This instruction is an opaque pointer into C-backend, thus
          it is protected with phantom types. *)
      module Insn : sig
        type ('a,'k) t = ('a,'k) insn
        val sexp_of_t : ('a,'k) t -> Sexp.t
        val compare : ('a,'k) t -> ('a,'k) t -> int
        val code : ('a,'k) t -> int
        val name : ('a,'k) t -> string
        val kinds : ('a,kinds) t -> Kind.t list
        val is : ('a,kinds) t -> Kind.t -> bool
        val asm : (asm,'k) t -> string
        val ops  : ('a,'k) t -> op array
      end

      (** Trie maps over instructions  *)
      module Trie : sig
        type key

        (** [key_of_first_insns state ~len:n] creates a key from first [n]
            instructions stored in the state if state contains such
            amount of instructions  *)
        val key_of_first_insns : (_,_,_,_) state -> len:int -> key option

        module Normalized : Trie with type key = key
        include Trie with type key := key
      end

      val available_backends : unit -> string list
    end

    module Linear : sig
      type t = (mem * insn option) list

      (** [Linear.sweep arch mem] will perform a linear sweep
          disassembly on the specified memory [mem] *)
      val sweep : arch -> mem -> t Or_error.t

      module With_exn : sig
        (** [Linear.With_exn.sweep] same as
            [Linear_sweep.memory], but raises an exception, instead of
            returning [Or_error] monad *)
        val sweep : arch -> mem -> t
      end
    end


    (** Recursive Descent Disassembler.
        This disassembler is built on top of [Basic] disassembler. It
        uses work list algorithm to implement recursive descent
        disassembly and lazily reconstructs the whole program CFG.

        This is an expert-level module, and it is suggested to use
        high-level [Disasm] interface, that is built ontop of this
        module.  *)
    module Recursive : sig
      type t
      type error = [
        | `Failed_to_disasm of mem
        | `Failed_to_lift of mem * Basic.full_insn * Error.t
      ] [@@deriving sexp_of]

      val run :
        ?backend:string ->
        ?brancher:brancher ->
        ?rooter:rooter -> arch -> mem -> t Or_error.t

      val cfg : t -> cfg

      val errors : t -> error list
    end
  end

  (** Assembly instruction.  *)
  module Insn : sig
    type t = insn [@@deriving bin_io, compare, sexp]

    (** {3 Creating}
        The following functions will create [insn] instances from a lower
        level representation.
    *)
    val of_basic : ?bil:bil -> Disasm_expert.Basic.full_insn -> t

    (** returns backend specific name of instruction *)
    val name : t -> string

    (** target-specific assembler string representing the instruction  *)
    val asm  : t -> string

    (** returns BIL program specifying instruction semantics  *)
    val bil  : t -> bil

    (** instruction operands  *)
    val ops  : t -> op array

    (** {3 Instruction properties} *)

    type must = Must
    type may = May
    type 'a property

    val new_property : 'a -> string -> 'a property

    val jump                : must property
    val conditional         : must property
    val indirect            : must property
    val call                : must property
    val return              : must property
    val affect_control_flow : may  property
    val load                : may  property
    val store               : may  property

    val is  : must property -> t -> bool
    val may : may  property -> t -> bool
    val must    : must property -> t -> t
    val mustn't : must property -> t -> t
    val should    : may  property -> t -> t
    val shouldn't : may  property -> t -> t


    (** [pp_adt] prints instruction in ADT format, suitable for reading
        by evaluating in many languages, e.g. Python, Js, etc *)
    val pp_adt : t printer

    (** {3 Prefix Tree}
        This module provides a trie data structure where a sequence of
        instructions is used as a key (and an individual instruction
        as a token). Two implementations are provided, a regular, where
        insns are compared as-is, and normalized, where instructions are
        compared using normalized comparison.

        In normalized comparison concerete immediate values are ignored,
        and if instructions have different number of operands, then only
        then excess operands are excluded from the comparison.
    *)
    module Trie : sig
      (** Trie requires 0(1) get operation  *)
      type key

      (** [key_of_insns insns] takes a list of instructions and transforms
          it to [key] *)
      val key_of_insns : t list -> key

      module Normalized : Trie with type key = key
      include Trie with type key := key
    end

    include Regular with type t := t
  end

  (** Access to block attributes.
      This interface provides only access to block attributes, but
      doesn't allow to navigate to other blocks.
  *)
  module Block : sig
    type t = block [@@deriving compare, sexp_of]

    (** [create mem insn] creates a block
        @pre insns is not empty  *)
    val create : mem -> (mem * insn) list -> t

    (** [addr block] address of the first instruction  *)
    val addr : t -> addr

    (** [memory blk] memory region, occupied by a block*)
    val memory : t -> mem

    (** [leader blk] the first instruction *)
    val leader : t -> insn

    (** [terminator blk] last instruction of the block  *)
    val terminator : t -> insn

    (** [insns blk] returns a list of block instructions  *)
    val insns : t -> (mem * insn) list

    include Opaque     with type t := t
    (** all the printing stuff, including [to_string] function *)
    include Printable  with type t := t
  end

  module Graphs : sig
    module Cfg : Graph with type t = cfg
                        and type node = block
                        and type Edge.label = edge

    (** A call graph representation.
        In this representations, nodes are identifiers of subroutine
        terms, and edges, representing calls, are marked with a list of
        callsites, where callsite is denoted by a jump term.  *)
    module Callgraph : Graph with type node = tid
                              and type Node.label = tid
                              and type Edge.label = jmp term list

    (** Graph view over IR.

        This module implements a graph view on an intermediate
        representation of a subroutine. To create an instance of a
        graph, using existing subroutine use {!Sub.to_cfg}. At any
        moment current sub term can be obtained using {!Sub.of_cfg}
        function. This is a just a projection operation, so it doesn't
        take any computing time.

        All [Graph] modification operations, like [insert], [remove]
        and [update] in [Node] and [Edge] modules are mapped to
        corresponding [Term] operations. Also, for performance
        reasons, graph is augmented with auxiliary data structures,
        that allows to perform most of the operations in O(log(N))
        time.

        Although this implements all operations of {!Graph} interface
        it is recommended to use {!Term} or [Builder] interfaces to
        build and modify underlying terms. The next few sections will
        clarify the behavior of a graph when it is modified using
        {!Graph} interface. If you do not want to read the following
        sections, then better do not use this module to build your
        terms.

        {2 Inserting nodes}

        When node is inserted into a graph [g] all jumps of a node,
        that lead to blocks that are already in a graph will be
        represented as edges. Also, all jumps from other nodes to the
        inserted node, will be added as edges (assuming that this
        other nodes are also in the graph g). Thus inserting node can
        create an arbitrary number of edges, from zero to N. If jump
        target is not yet in the graph, then jump is not removed from a
        sequence of jumps of the inserted node, but just ignored.


        {2 Updating nodes}

        When node is updated with the same node (but possibly with
        different set of terms, see {{!sema}description of sameness})
        then all changes that affects control flow will be
        applied. For example, if jump is absent in a new version of a
        block, and this jump corresponds to an edge in the graph, then
        this edge will be removed.

        {2 Removing nodes}

        The node will be removed from the underlying [sub term], and
        all edges incident to the removed node will be also removed.
        This will not affect jmp terms of blk terms.

        {2 Inserting edges}

        Edges in IR graph represents a transfer of a control flow
        between basic blocks. The basic block in IR is more reach,
        rather then a node in a graph. For example, in blk term the
        order of jumps matters. Jump [n] is taken, only if guard
        conditions of jumps [0] to [n-1] evaluated to [false] (like
        switch statement in C language). The order of edges in a graph
        is unspecified. So, some precaution should be taken, to handle
        edge removing and inserting correctly. Each edge is labeled
        with abstract label, that represents the jump position in a
        graph.

        When an edge is created it will look for corresponding jumps
        in source node. If there exists such jump, and it points to
        the destination, then it will be left untouched. If it points
        to a different node, then it will be fixed to point at the
        a given destination. If there is no position in a slot,
        represented by the a given label, then it will be
        inserted. Dummy jumps will be prepended before the inserted
        jump, if needed.

        When an edge is inserted into the graph, then source and
        destination nodes are inserted or updated (depending on whether
        they were already present in the graph). As a result, the
        graph must contain at least nodes, incident to the edge, and
        the edge itself.

        {2 Updating edge}

        Updating an edge is basically the same, as updating incident
        nodes, a given that the edge exists in the graph.


        {2 Removing edge}

        Removing an edge is not symmetric with edge insertion. It
        doesn't remove the incident nodes, but instead removes jumps
        from the source node to destination. The jumps are removed
        accurately, so that the order (and semantics) is preserved. If
        the removed jump was in the middle of the sequence then it is
        substituted by a dummy jump with [false] guard.
    *)
    module Ir : sig
      type t
      type edge
      type node

      (** since in IR the order of edges defines semantics, we provide
          extra functions *)
      module Edge : sig
        include Edge with type graph = t
                      and type node = node
                      and type t = edge

        (** [jmps dir e g] enumerates all jumps (including calls,
            interrupts, indirects, etc), that occurs before if
            [dir = `before] or after if [dir = `after] an edge [e] *)
        val jmps  : [`after | `before] -> t -> graph -> jmp term seq

        (** [edges dir e g] enumerates all edges occurring before of
            after an edge [e] in graph [g] *)
        val edges : [`after | `before] -> t -> graph -> t seq

        (** [jmp e] returns a jmp term associated with edge [e]  *)
        val jmp : t -> jmp term

        (** [tid e] returns a tid of a jmp term that is associated
            with an edge [e] *)
        val tid : t -> tid

        (** [cond e g] computes a condition expression that is
            asserted to be [true] if this branch is taken.

            Note: this is not the same as a condition associated with
            the jmp term itself, it takes into account all conditions
            preceding the edge.
        *)
        val cond : t -> graph -> exp

        include Printable with type t := t
      end

      module Node : sig
        include Node with type graph = t
                      and type t = node
                      and type edge = edge
                      and type label = blk term
        include Printable with type t := t
      end

      include Graph with type t := t
                     and type node := node
                     and type edge := edge
                     and type Node.label = blk term
                     and module Node := Node
                     and module Edge := Edge

    end


    module Tid : Graph with type node = tid
                        and type Node.label = tid
                        and type Edge.label = tid

  end

  (** Disassembled program.

      This module provides an interface for values of type [disasm]. *)
  module Disasm : sig
    type t = disasm

    (** [create cfg]   *)
    val create : cfg -> t

    (** [disassemble ?roots arch mem] disassemble provided memory region
        [mem] using best available algorithm and backend for the specified
        [arch]. Roots, if provided, should point to memory regions, that
        are believed to contain code. At best, this should be a list of
        function starts. If no roots are provided, then the starting
        address of the provided memory [mem] will be used as a root.

        The returned value will contain all memory reachable from the
        a given set of roots, at our best knowledge. *)
    val of_mem :
      ?backend:string ->
      ?brancher:brancher ->
      ?rooter:rooter -> arch -> mem -> t Or_error.t

    (** [disassemble_image image] disassemble a given image.
        Will take executable segments of the image and disassemble it,
        applying [disassemble] function. If no roots are specified, then
        symbol table will be used as a source of roots. If file doesn't
        contain one, then entry point will be used.
    *)
    val of_image :
      ?backend:string ->
      ?brancher:brancher -> ?rooter:rooter -> image -> t Or_error.t

    (** [disassemble_file ?roots path] takes a path to a binary and
        disassembles it  *)
    val of_file :
      ?backend:string ->
      ?brancher:brancher -> ?rooter:rooter ->
      ?loader:string -> string -> t Or_error.t

    module With_exn : sig
      val of_mem   : ?backend:string -> ?brancher:brancher -> ?rooter:rooter -> arch -> mem -> t
      val of_image : ?backend:string -> ?brancher:brancher -> ?rooter:rooter -> image -> t
      val of_file  : ?backend:string -> ?brancher:brancher ->
        ?rooter:rooter -> ?loader:string -> string -> t
    end

    (** [merge d1 d2] is a union of control flow graphs and erros of
        the two disassemblers.  *)
    val merge : t -> t -> t

    (** returns all instructions that was successfully decoded in an
        ascending order of their addresses. Each instruction is
        accompanied with its block of memory. *)
    val insns : t -> (mem * insn) seq

    (** A whole program CFG.  *)
    val cfg : t -> cfg


    (** {2 Tags}  *)

    (** start of basic block  *)
    val block : addr tag

    (** machine instruction  *)
    val insn : insn tag

    (** address of instruction  *)
    val insn_addr : addr tag
  end

  type symtab

  (** Reconstructed symbol table.  *)
  module Symtab : sig
    (** This data structure holds information about functions that
        were found in the executable.*)

    (** symbol table  *)
    type t = symtab [@@deriving sexp_of]

    type fn = string * block * cfg [@@deriving sexp_of]

    (** empty symbol table  *)
    val empty : t

    (** [add_symbol table name entry blocks] extends [table] with a
        new symbol with a given [name], [entry] block and body
        [blocks].  *)
    val add_symbol : t -> fn -> t

    (** [remove table fn] removes symbol [fn] from [table]  *)
    val remove : t -> fn -> t

    (** [find_by_name symbols name] finds a symbol with a given namem  *)
    val find_by_name  : t -> string -> fn option


    (** [find_by_start symbols addr] finds a symbol that starts from
        a given address *)
    val find_by_start : t -> addr -> fn option

    (** [owners addr] return a list of functions that owns [addr] *)
    val owners : t -> addr -> fn list

    (** [dominators syms mem] returns a list of functions that
        dominates over provided memory region [mem] *)
    val dominators : t -> mem -> fn list

    (** [intersecting_mem syms mem] returns a list of functions, that
        resides in memory region [mem]  *)
    val intersecting : t -> mem -> fn list

    (** [to_sequence symtab] returns a sequence of functions  *)
    val to_sequence : t -> fn seq

    val span : fn -> unit memmap
  end


  type lifter = mem -> Disasm_expert.Basic.full_insn -> bil Or_error.t

  (** A BIL model of CPU

      In general this is a model of a processor architecture, involving
      ALU, processing unit, registers and memory.
  *)

  (** Abstract interface to CPU  *)
  module type CPU = sig

    (** {3 Minimum set of required definitions} *)

    (** A set of general purpose registers *)
    val gpr : Var.Set.t

    (** Memory  *)
    val mem : var

    (** Stack pointer  *)
    val sp  : var

    (** {4 Flag registers}  *)
    val zf  : var
    val cf  : var
    val vf  : var
    val nf  : var

    (** {3 Predicates}  *)
    val is_reg : var -> bool
    val is_flag : var -> bool

    val is_sp : var -> bool
    val is_bp : var -> bool
    val is_pc : var -> bool

    val is_zf : var -> bool
    val is_cf : var -> bool
    val is_vf : var -> bool
    val is_nf : var -> bool
    val is_mem : var -> bool
  end

  (** Abstract interface for all targets.

      Each target supported by BAP implements this interface. To get
      access to the implementation use
      {{!target_of_arch}target_of_arch} function. Code written using
      this interface is cross-platform, i.e., target agnostic. If you
      want to write target-specific code, then use directly
      corresponding modules: {{!ARM}ARM}, {{!IA32}IA32},
      {{!AMD64}AMD64}. *)
  module type Target = sig
    module CPU : CPU
    (** [lift mem insn] lifts provided instruction to BIL.
        Usually you do not need to call this function directly, as
        [disassemble] function will do the lifting. *)
    val lift : lifter
  end

  (** [target_of_arch arch] returns a module packed into value, that
      abstracts target architecture. The returned module has type
      {!Target} and can be unpacked locally with:
      {[
        let module Target = (val target_of_arch arch) in
      ]}
  *)
  val target_of_arch : arch -> (module Target)


  (** Register new target architecture. If target for the given arch
      already exists, then it will be superseeded by the new
      target.  *)
  val register_target : arch -> (module Target) -> unit


  (** Term identifier  *)
  module Tid : sig
    type t = tid

    (** [create ()] creates a fresh newly term identifier  *)
    val create : unit -> t

    (** [set_name tid name] associates a [name] with a given
        term identifier [tid]. Any previous associations are
        overridden.*)
    val set_name : tid -> string -> unit

    (** [name tid] returns a term name: either a string name
        with at-prefix, or number identifier.   *)
    val name : tid -> string

    (** [from_string s] parses tid from string. The expected
        format is:
        {v
          tid = symbol | number.
          symbol = "@", string.
          number = "%", hex.
          string = ?sequence of characters?.
          number = ?ocaml hexadecimal representation?.
        v}
    *)
    val from_string : string -> tid Or_error.t

    (** [from_string_exn s] same as [from_string_exn] but throws
        exception on error.  *)
    val from_string_exn : string -> tid

    (** infix notation for [from_string_exn]  *)
    val (!) : string -> tid

    include Regular with type t := t
  end

  (** IR language term.  *)
  module Term : sig
    (** Term is a building block of the
        {{!sema}Intermediate Representation} of the binary program.

        This module provides functions that are overloaded for
        different term classes. Term class is denoted with an explicit
        instance of type [('a,'b)cls], where ['a] stands for the parent
        term and ['b] for the child term.

        {2 Example}

        Give a block

        {[# let b = Blk.create ();;]}
        {v val b : Blk.t =
          00000003: v}


        We can append a definition to it with an overloaded
        [Term.append]

        {[# let b = Term.append def_t b d_1;;]}
        {v val b : blk term =
          00000003:
          00000001: x := y + z
          v}

        Update a value of a definition in the block:


        {[# let b = Term.update def_t b d_2;;]}
        {v val b : blk term =
          00000003:
          00000001: x := true
          v}

    *)

    (** term type  *)
    type 'a t = 'a term

    (** [clone term] creates an object with a fresh new identifier
        that has the same contents as [term], i.e., that is
        syntactically the same. The clone operation is shallow, all
        subterms of [term] are unchanged.
    *)
    val clone : 'a t -> 'a t

    (** [same x y] returns true if [x] and [y] represents the same
        entity, i.e., [Tid.(tid x = tid y)] *)
    val same : 'a t -> 'a t -> bool

    (** [name t] returns a string representation of a term [t] identity *)
    val name : 'a t -> string

    (** [tid entity] returns a unique identifier of the [entity]  *)
    val tid : 'a t -> tid

    (** [length t p] returns an amount of terms of [t] class in a
        parent term [p] *)
    val length : ('a,'b) cls -> 'a t -> int

    (** [find t p id] is [Some c] if term [p] has a subterm of type [t]
        such that [tid c = id].  *)
    val find : ('a,'b) cls -> 'a t -> tid -> 'b t option

    (** [find_exn t p id] like {!find} but raises [Not_found] if nothing
        is found.  *)
    val find_exn : ('a,'b) cls -> 'a t -> tid -> 'b t

    (** [update t p c] if term [p] contains a term with id equal to
        [tid c] then return [p] with this term substituted with [p] *)
    val update : ('a,'b) cls -> 'a t -> 'b t -> 'a t

    (** [remove t p id] returns a term that doesn't contain element
        with the a given [id] *)
    val remove : ('a,_) cls -> 'a t -> tid -> 'a t

    (** [change t p id f] if [p] contains subterm with of a given kind
        [t] and identifier [id], then apply [f] to this
        subterm. Otherwise, apply [f] to [None]. If [f] return [None],
        then remove this subterm (a given it did exist), otherwise,
        update parent with a new subterm.  *)
    val change : ('a,'b) cls -> 'a t -> tid -> ('b t option -> 'b t option) -> 'a t


    (** [enum ?rev t p] enumerate all subterms of type [t] of the
        a given term [p] *)
    val enum : ?rev:bool -> ('a,'b) cls -> 'a t -> 'b t seq

    (** [to_sequence ?rev t p] is a synonym for [enum]. *)
    val to_sequence : ?rev:bool -> ('a,'b) cls -> 'a t -> 'b t seq

    (** [map t p ~f] returns term [p] with all subterms of type [t]
        mapped with function [f] *)
    val map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t) -> 'a t

    (** [filter_map t p ~f] returns term [p] with all subterms of type
        [t] filter_mapped with function [f], i.e., all terms for which
        function [f] returned [Some thing] are substituted by the
        [thing], otherwise they're removed from the parent term *)
    val filter_map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t option) -> 'a t

    (** [concat_map t p ~f] substitute subterm [c] of type [t] in
        parent term [p] with [f c]. If [f c] is an empty list, then
        [c] doesn't occur in a new parent term, if [f c] is a
        singleton list, then [c] is substituted with the [f c], like
        in [map]. If [f c] is a list of [n] elements, then in the
        place of [c] this [n] elements are inserted.  *)
    val concat_map : ('a,'b) cls -> 'a t -> f:('b t -> 'b t list) -> 'a t

    (** [filter t p ~f] returns a term [p] with subterms [c] for which
        [f c = false] removed. *)
    val filter : ('a,'b) cls -> 'a t -> f:('b t -> bool) -> 'a t

    (** [first t p] returns the first subterm of type [t] of a given
        parent [p] *)
    val first : ('a,'b) cls -> 'a t -> 'b t option

    (** [last t p] returns a last subterm of type [t] of a given
        parent [p] *)
    val last  : ('a,'b) cls -> 'a t -> 'b t option

    (** [next t p id] returns a term that preceeds a term with a given
        [id], if such exists.  *)
    val next : ('a,'b) cls -> 'a t -> tid -> 'b t option

    (** [next t p id] returns a term that is after a term with a given
        [id], if such exists.  *)
    val prev : ('a,'b) cls -> 'a t -> tid -> 'b t option

    (** [after t ?rev p tid] returns all subterms in term [p] that
        occur after a term with a given [tid]. if [rev] is [true] or
        omitted then terms are returned in the evaluation
        order. Otherwise they're reversed. If there is no term with
        a given [tid], then an empty sequence is returned. *)
    val after : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq

    (** [before t ?rev p tid] returns all term that occurs before
        defintion with a given [tid] in blk. If there is no such
        definition, then the sequence will be empty.  @param rev has
        the same meaning as in {!after}.  *)
    val before : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq

    (** [append t ~after:this p c] returns the [p] term with [c]
        appended after [this] term. If [after] is not specified, then
        append [def] to the end of the parent term (if it makes sense,
        otherwise it is just added).  If [this] doesn't occur in the
        [p] term then do nothing. The term tid is preserved.  *)
    val append : ('a,'b) cls -> ?after:tid -> 'a t -> 'b t -> 'a t

    (** [prepend t ~before:this p c] returns the [p] with [c] inserted
        before [this] term. If [before] is left unspecified, then
        insert the [c] at the beginning of the [p] if it is a
        sequence, otherwise just insert. If [this] is specified but
        doesn't occur in the [p] then [p] is returned as is.  In all
        cases, the returned term has the same [tid] as [p]. *)
    val prepend : ('a,'b) cls -> ?before:tid -> 'a t -> 'b t -> 'a t

    (** [nth t p n] returns [n]'th [t]-term of parent [p].  *)
    val nth : ('a,'b) cls -> 'a t -> int -> 'b t option

    (** [nth_exn t p n] same as [nth], but raises exception if [n] is
        not a valid position number.  *)
    val nth_exn : ('a,'b) cls -> 'a t -> int -> 'b t


    (** {2 Attributes}

        Terms attribute set can be extended, using {{!Value}universal
        values}. A value of type ['a tag] is used to denote an
        attribute of type ['a] with the name [Value.Tag.name tag].

        With the provided interface Term can be treated as an
        extensible record.
    *)

    (** [set_attr term attr value] attaches an [value] to attribute
        [attr] in [term] *)
    val set_attr : 'a t -> 'b tag -> 'b -> 'a t
    (** [attrs term attrs] returns the set of [attributes] associated
        with a [term]*)
    val attrs : 'a t -> Dict.t
    (** [get_attr term attr] returns a value of the a given [attr] in
        [term] *)
    val get_attr : 'a t -> 'b tag -> 'b option

    (** [has_attr term attr] is [true] if [term] has attribute [attr]  *)
    val has_attr : 'a t -> 'b tag -> bool

    (** [del_attr term attr] deletes attribute [attr] from [term]  *)
    val del_attr : 'a t -> 'b tag -> 'a t


    (** {Predefined attributes}  *)

    (** a term was artificially produced from a term with a given tid.   *)
    val origin : tid tag

    (** a term was introduced artificially by an analysis.  *)
    val synthetic : unit tag

    (** a term is identified as always non dead  *)
    val live : unit tag

    (** a term is identified as dead  *)
    val dead : unit tag

    (** to mark a term as visited by some algorithm  *)
    val visited : unit tag

    (** precondition must on the entrance to the subroutine *)
    val precondition : exp tag

    (** invariant must be always true while the term is evaluated  *)
    val invariant : exp tag

    (** must hold just after the term is left  *)
    val postcondition : exp tag


    (** {2 Higher order mapping}  *)

    (** Mapper perfoms deep identity term mapping. If you override any
        method make sure that you didn't forget to invoke parent's
        method, as OCaml will not call it for you.  *)
    class mapper : object
      inherit Exp.mapper

      (** [map_term cls t] dispatches [t] to corresponding method *)
      method map_term : 't 'p. ('p,'t) cls -> 't term -> 't term

      (** [run p] maps each sub in program [p]  *)
      method run : program term -> program term

      (** [map_sub sub] maps each arg and blk in [sub]  *)
      method map_sub : sub term -> sub term

      (** [map_arg arg] is [arg]   *)
      method map_arg : arg term -> arg term

      (** [map_blk blk] is [blk]   *)
      method map_blk : blk term -> blk term

      (** [map_phi phi] is [phi]   *)
      method map_phi : phi term -> phi term

      (** [map_def def] is [def]   *)
      method map_def : def term -> def term

      (** [map_jmp jmp] is [jmp]   *)
      method map_jmp : jmp term -> jmp term
    end

    (** Visitor performs deep visiting. As always, don't forget to
        overrid parent methods. The visitor comes with useful [enter_T]
        [leave_T] that are no-ops in this visitor, so if you inherit
        directly from it, then you may not call to the parent method.  *)
    class ['a] visitor : object
      inherit ['a] Exp.visitor

      method enter_term : 't 'p . ('p,'t) cls -> 't term -> 'a -> 'a
      (** [visit_term cls t] dispatch term [t] to corresponding method  *)
      method visit_term : 't 'p . ('p,'t) cls -> 't term -> 'a -> 'a
      method leave_term : 't 'p . ('p,'t) cls -> 't term -> 'a -> 'a


      method enter_program : program term -> 'a -> 'a
      method run           : program term -> 'a -> 'a
      method leave_program : program term -> 'a -> 'a

      method enter_sub : sub term -> 'a -> 'a
      method visit_sub : sub term -> 'a -> 'a
      method leave_sub : sub term -> 'a -> 'a

      method enter_blk : blk term -> 'a -> 'a
      method visit_blk : blk term -> 'a -> 'a
      method leave_blk : blk term -> 'a -> 'a

      method enter_arg : arg term -> 'a -> 'a
      method visit_arg : arg term -> 'a -> 'a
      method leave_arg : arg term -> 'a -> 'a

      method enter_phi : phi term -> 'a -> 'a
      method visit_phi : phi term -> 'a -> 'a
      method leave_phi : phi term -> 'a -> 'a

      method enter_def : def term -> 'a -> 'a
      method visit_def : def term -> 'a -> 'a
      method leave_def : def term -> 'a -> 'a

      method enter_jmp : jmp term -> 'a -> 'a
      method visit_jmp : jmp term -> 'a -> 'a
      method leave_jmp : jmp term -> 'a -> 'a
    end


    (** [switch cls t ~program ~sub .. ~jmp] performs a pattern
        matching over a term [t] based on its type class [cls].
        It is guaranteed that only one function will be called for a
        term.*)
    val switch : ('p,'t) cls ->
      program:(program term -> 'a) ->
      sub:(sub term -> 'a) ->
      arg:(arg term -> 'a) ->
      blk:(blk term -> 'a) ->
      phi:(phi term -> 'a) ->
      def:(def term -> 'a) ->
      jmp:(jmp term -> 'a) -> 't term -> 'a


    (** [proj cls t ?case] a special case of pattern matching,
        where all cases by default returns [None] *)
    val proj : ('p,'t) cls ->
      ?program:(program term -> 'a option) ->
      ?sub:(sub term -> 'a option) ->
      ?arg:(arg term -> 'a option) ->
      ?blk:(blk term -> 'a option) ->
      ?phi:(phi term -> 'a option) ->
      ?def:(def term -> 'a option) ->
      ?jmp:(jmp term -> 'a option) ->
      't term -> 'a option

    (** [cata cls ~init t ?case] performs a pattern matching. All
        methods by default returns [init].
        Note: [cata] stands for [catamorphism] *)
    val cata : ('p,'t) cls -> init:'a ->
      ?program:(program term -> 'a) ->
      ?sub:(sub term -> 'a) ->
      ?arg:(arg term -> 'a) ->
      ?blk:(blk term -> 'a) ->
      ?phi:(phi term -> 'a) ->
      ?def:(def term -> 'a) ->
      ?jmp:(jmp term -> 'a) ->
      't term -> 'a

  end

  (** Program in Intermediate representation.  *)
  module Program : sig
    (** Program is a collection of function terms. *)

    type t = program term

    (** [create ?tid ()] creates an empty program. If [tid]  *)
    val create : ?tid:tid -> unit -> t

    (** [lift symbols] takes a table of functions and return a whole
        program lifted into IR *)
    val lift : symtab -> program term

    (** [to_graph program] creates a callgraph of a [program]  *)
    val to_graph : t -> Graphs.Callgraph.t

    (** [lookup t program id] is like {{!find}find} but performs deep
        lookup in the whole [program] for a term with a given [id].
        This function is memoized, so it has amortized O(1)
        complexity, with a wostcase complexity of $O(N)$, where $N$ is
        the total amount of terms in program.  *)
    val lookup : (_,'b) cls -> t -> tid -> 'b term option

    (** [parent t program id] is [Some p] iff [find t p id <> None]  *)
    val parent : ('a,'b) cls -> t -> tid -> 'a term option

    (** Program builder.  *)
    module Builder : sig
      type t
      (** Initializes an empty builder.  *)
      val create : ?tid:tid  -> ?subs:int -> unit -> t

      (** [add_sub builder sub] appends a subroutine term to the
          program.  *)
      val add_sub : t -> sub term -> unit

      (** fixes the result  *)
      val result : t -> program term
    end

    include Regular with type t := t
  end

  (** Subroutine.  *)
  module Sub : sig
    (** Subroutine is a set of blocks.  The first block of a function is
        considered an entry block.  *)
    type t = sub term

    (** [create ?name ()] creates an empty subroutine with an optional
        name. *)
    val create : ?tid:tid -> ?name:string -> unit -> t

    (** [lift entry] takes an basic block of assembler instructions,
        as an entry and lifts it to the subroutine term.  *)
    val lift : block -> cfg -> sub term

    (** [name sub] returns a subroutine name  *)
    val name : t -> string

    (** updates subroutine name *)
    val with_name : t -> string -> t

    (** [ssa sub] returns [sub] in SSA form. If program is already in
        SSA, then do nothing (see also {!is_ssa}). The underlying
        algorithm produces a semi-pruned SSA form. To represent
        different versions of the same variable we use {{!Var}variable
        versions}. Any definition of a variable increases its version
        number. So, the zero version is reserved for variables that
        weren't defined before the first use.  *)
    val ssa : t -> t

    (** [is_ssa sub] is [true] if [sub] was transformed into an SSA
        form. This is O(1) predicate that doesn't really check, that
        a subroutine is in an SSA form, so it is a responsibility of
        a user to preserve the SSA form on any transformation.    *)
    val is_ssa : t -> bool

    (** [free_vars sub] computes a set of variables that are free in
        a given subroutine [sub]. The variable is considered free if it
        is used before defined or is not locally bound.  If [sub] is in
        an SSA form, then the set is computed trivially, thanks to a
        naming scheme. If program is not in an SSA form, then a BFS on a
        dominators tree is used.  *)
    val free_vars : t -> Var.Set.t

    (** [to_graph sub] builds a graph of subroutine [sub]. Graph nodes
        are block term identifiers, and edges are labeled with term
        identifiers of the jmp terms, that corresponds to the edge.
        This representation is useful, if you need to compute some
        graph relation on a subroutine, that will be later used to
        perform its incremental transformation. *)
    val to_graph : t -> Graphs.Tid.t

    (** [to_cfg sub] builds a graph representation of a subroutine
        [sub]. All graph operations are mapped to corresponding
        [Term] operations. See {!Graphlib.Ir} for more information.*)
    val to_cfg : t -> Graphs.Ir.t

    (** [of_cfg cfg] extracts a [sub term] from a given graph [cfg].
        Since {!Graphlib.Ir} module builds term incrementally this
        operation is just a projection, i.e., it has O(0) complexity.  *)
    val of_cfg : Graphs.Ir.t -> t


    (** other names for the given subroutine.*)
    val aliases : string list tag



    (** A subroutine doesn't examine any values except its arguments,
        and have no effects except the return value. Basically this is
        just slightly more strict class than the pure attribute below,
        since function is not allowed to read global memory.  Note that a
        function that has pointer arguments and examines the data
        pointed to is not const. Likewise, a function that
        calls a non-const function usually is not be const. It does not
        make sense for a const function to return void *)
    val const : unit tag

    (** A subroutine have no effects except the return value and their
        return value depends only on the parameters and/or global
        variables.  *)
    val pure : unit tag

    (** A subroutine is a stub  *)
    val stub : unit tag

    (** A subroutine is visible outside of the compilation unit  *)
    val extern : unit tag

    (** a subroutine doesn't contain any calls in any disguise, i.e.,
        no longjmps, indirect calls, exceptions, etc.    *)
    val leaf : unit tag

    (** A subroutine is malloc-like, i.e., the pointer P returned
        by the subroutine cannot alias any other pointer valid when the
        function returns, and moreover no pointers to valid objects occur
        in any storage addressed by P. *)
    val malloc : unit tag

    (** A subroutine will not return (either loop infinitely or abort
        a program)  *)
    val noreturn : unit tag

    (** A subroutine may return more than one time. Examples of such
        functions are setjmp and vfork *)
    val returns_twice : unit tag

    (** A subroutine doesn't throw exceptions  *)
    val nothrow : unit tag

    (** Subroutine builder *)
    module Builder : sig
      type t
      (** initializes empty subroutine builder.  *)
      val create : ?tid:tid -> ?args:int -> ?blks:int -> ?name:string ->
        unit -> t
      (** appends a block to a subroutine  *)
      val add_blk : t -> blk term -> unit
      (** appends an argument  *)
      val add_arg : t -> arg term -> unit

      (** returns current result  *)
      val result : t -> sub term
    end

    include Regular with type t := t
  end

  (** Block.  *)
  module Blk : sig
    (** Logically block consists of a set of {{!Phi}phi nodes}, a
        sequence of {{!Def}definitions} and a sequence of out-coming
        edges, aka {{!Jmp}jumps}. A colloquial term for this three
        entities is a {e block element}.

        The order of Phi-nodes can be specified in any order, as
        they execute simultaneously . Definitions are stored in the
        order of execution. Jumps are specified in the order in which
        they should be taken, i.e., jmp_n is taken only after
        jmp_n-1 and if and only if the latter was not taken. For
        example, if block ends with N jumps, where each n-th jump
        have destination named t_n and condition c_n then it
        would have the semantics as per the following OCaml program:

        {v
            if c_1 then jump t_1 else
            if c_2 then jump t_2 else
            if c_N then jump t_N else
            stop
          v}
    *)

    type t = blk term

    (** Union type for all element types  *)
    type elt = [
      | `Def of def term
      | `Phi of phi term
      | `Jmp of jmp term
    ]

    (** [create ()] creates a new empty block.  *)
    val create : ?tid:tid -> unit -> t

    (** [lift block] takes a basic block of assembly instructions and
        lifts it to a list of blk terms. The first term in the list
        is the entry. *)
    val lift : cfg -> block -> blk term list


    (** [from_insn insn] creates an IR representation of a single
        machine instruction [insn].  *)
    val from_insn : insn -> blk term list

    (** [split_while blk ~f] splits [blk] into two block: the first
        block holds all definitions for which [f p] is true and has
        the same tid as [blk]. The second block is freshly created and
        holds the rest definitions (if any). All successors of the
        [blk] become successors of the second block, which becomes the
        successor of the first block.

        Note: if [f def] is [true] for all blocks, then the second
        block will not contain any definitions, i.e., the result would
        be the same as of {{!split_bot}split_bot} function. *)
    val split_while : t -> f:(def term -> bool) -> t * t

    (** [split_after blk def] creates two new blocks, where the first
        block contains all defintions up to [def] inclusive, the
        second contains the rest.

        Note: if def is not in a [blk] then the first block will contain
        all the defintions, and the second block will be empty.  *)
    val split_after : t -> def term -> t * t

    (** [split_before blk def] is like {{!split_after}split_after} but
        [def] will fall into the second [blk] *)
    val split_before : t -> def term -> t * t

    (** [split_top blk] returns two blocks, where first block shares
        the same tid as [blk] and has all $\Phi$-nodes of [blk], but
        has only one destination, namely the second block. Second
        block has new tidentity, but inherits all definitions and
        jumps from the [blk]. *)
    val split_top : t -> t * t

    (** [split_top blk] returns two blocks, where first block shares
        the same tid as [blk], has all $\Phi$-nodes and definitions
        from [blk], but has only one destination, namely the second
        block. Second block has new tidentity, all jumps from the
        [blk]. *)
    val split_bot : t -> t * t

    (** [elts ~rev blk] return all elements of the [blk].  if [rev] is
        false or left unspecified, then elements are returned in the
        following order: $\Phi$-nodes, defs (in normal order), jmps in
        the order in which they will be taken.  If [rev] is true, the
        order will be the following: all jumps in the opposite order,
        then definitions in the opposite order, and finally
        $\Phi$-nodes. *)
    val elts : ?rev:bool -> t -> elt seq

    (** [map_exp b ~f] applies function [f] for each expression in
        block [b]. By default function [f] will be applied to all
        values of type [exp], including right hand sides of
        phi-nodes, definitions, jump conditions and targets. [skip]
        parameter allows to skip expressions from a given term kind.*)
    val map_exp :
      ?skip:[`phi | `def | `jmp] list -> (** defaults to [[]]  *)
      t -> f:(exp -> exp) -> t


    (** [substitute ?skip blk x y] substitutes each occurrence of
        expression [x] with expression [y] in block [blk]. See
        {!map_exp} for [skip] parameter. The substitution is performed
        deeply.  *)
    val substitute :
      ?skip:[`phi | `def | `jmp] list -> (** defaults to [[]]  *)
      t -> exp -> exp -> t

    (** [map_lhs blk ~f] applies [f] to every left hand side variable
        in def and phi subterms of [blk]. Parameter [skip] allows to
        ignore particular terms.  E.g.,
        [map_lhs ~skip:[`phi] ~f:(substitute vars)].*)
    val map_lhs :
      ?skip:[`phi | `def ] list -> (** defaults to [[]]  *)
      t -> f:(var -> var) -> t

    (** [find_var blk var] finds a last definition of a variable [var]
        in a block [blk].  *)
    val find_var : t -> var -> [
        | `Phi of phi term
        | `Def of def term
      ] option

    (** [defines_var blk x] true if there exists such phi term or def
        term with left hand side equal to [x]  *)
    val defines_var : t -> var -> bool

    (** [free_vars blk] returns a set of variables that occurs free
        in block [blk]. A variable is free, if it occurs unbound in the
        expression and there is no preceding definition of this variable
        in a block [blk].  *)
    val free_vars : t -> Var.Set.t

    (** [uses_var blk x] true if variable [x] is in [free_vars blk].
        If you need to call this function on several variables it is
        better to compute [free_vars] explicitly and use [Set.mem]
        function.  *)
    val uses_var : t -> var -> bool

    (** [occurs blk after:x def] if [def] is occurs after definition
        [def] in [blk].  *)
    val occurs : t -> after:tid -> tid -> bool

    (** Builder interface.  *)
    module Builder : sig
      (** This interface provides an efficient way to build new
          blocks. It is also useful, when rebuilding existing block,
          as it allows to specify the [tid] of the block. It is a user
          responsibility to preserve the uniqueness of tidentifiers
          throughout the program instance.  *)
      type t

      (** [create ~tid ~phis ~defs ~jmp ()] creates a block builder.
          If [tid] parameter is specified, then the new block will
          have this tid. If any of [phis], [defs] or [jmps] parameters
          are specified, the provtided number would be used as a hint
          of the expected amount of the corresponding entries. Since
          it is the hint, it can mismatch with the actual size. The
          hint must be a positive number.  *)
      val create : ?tid:tid -> ?phis:int -> ?defs:int -> ?jmps:int -> unit -> t

      (** [init blk] creates a builder based on an existing
          block. If [copy_phis], [copy_defs] or [copy_jmps] is [true]
          (defaults to [false]), then prepopulate builder with
          corresponding terms from block [blk]. If [same_tid] is true
          (default), then a resulting block will have the same [tid]
          as block [blk]. Otherwise, a fresh new [tid] will be created. *)
      val init :
        ?same_tid :bool ->       (** defaults to [true]  *)
        ?copy_phis:bool ->       (** defaults to [false] *)
        ?copy_defs:bool ->       (** defaults to [false] *)
        ?copy_jmps:bool ->       (** defaults to [false] *)
        blk term -> t

      (** appends a definition  *)
      val add_def : t -> def term -> unit
      (** appends a jump  *)
      val add_jmp : t -> jmp term -> unit
      (** appends a phi node  *)
      val add_phi : t -> phi term -> unit
      (** appends generic element *)
      val add_elt : t -> elt -> unit
      (** returns current result  *)
      val result  : t -> blk term
    end

    include Regular with type t := t
  end

  (** Definition.  *)
  module Def : sig
    (** The definition is an assignment. The left hand side of an
        assignment is a variable, and the right side is an expression.

        The definition is the only way for a block to perform some
        side effects.
    *)

    type t = def term

    (** [create ?tid x exp] creates definition [x := exp]  *)
    val create : ?tid:tid -> var -> exp -> t

    (** returns the left hand side of a definition  *)
    val lhs : t -> var
    (** returns the right hand side of a definition  *)
    val rhs : t -> exp

    (** updates the lhs of definition  *)
    val with_lhs : t -> var -> t
    (** updates the right hand side of a definition  *)
    val with_rhs : t -> exp -> t

    (** [map_exp def f] applies [f] to a [rhs] of [def] and returns
        an updated definition. *)
    val map_exp : t -> f:(exp -> exp) -> t

    (** [substitute def x y] substitutes [x] by [y] in the right hand
        side of a definition [def] *)
    val substitute : t -> exp -> exp -> t

    (** [free_vars def] returns a set of free variables, that occurs
        on the right hand side of definition [def]. See {!Exp.free_vars}
        for more information.  *)
    val free_vars : t -> Var.Set.t

    include Regular with type t := t
  end

  (** A control transfer operation.  *)
  module Jmp : sig
    (** Jmp is the only way to transfer control from block to block.
        Jumps are guarded with conditions. The jump should be taken
        only if its condition is evaluated to true.
        When control flow reaches the end of block it should take the
        first jump with true condition. If there is no such jump, then
        program stops.

        Jumps are further subdivided into categories:
        - goto - is a local control transfer instruction. The label
          can be only local to subroutine;
        - call - transfer a control to another subroutine. A call
          contains a continuation, i.e., a label to which we're hoping
          to return after subroutine returns the control to us. Of
          course, called subroutine can in general return to another
          position, or not to return at all.
        - ret - performs a return from subroutine
        - int - calls to interrupt subroutine. If interrupt returns,
          then continue with the provided label.

    *)
    type t = jmp term

    (** [create ?cond kind] creates a jump of a given kind  *)
    val create : ?tid:tid -> ?cond:exp -> jmp_kind -> t

    (** [create_call ?cond target] transfer control to subroutine
        [target] *)
    val create_call : ?tid:tid -> ?cond:exp -> call  -> t

    (** [create_goto ?cond label] local jump  *)
    val create_goto : ?tid:tid -> ?cond:exp -> label -> t

    (** [create_ret ?cond label] return from a procedure  *)
    val create_ret  : ?tid:tid -> ?cond:exp -> label -> t

    (** [create_int ?cond int_number return] call interrupt subroutine  *)
    val create_int  : ?tid:tid -> ?cond:exp -> int -> tid -> t

    (** [kind jmp] evaluates to a kind of jump  *)
    val kind : t -> jmp_kind

    (** [cond jmp] returns the jump guard condition  *)
    val cond : t -> exp

    (** [exps jmp] returns a sequence of expressions occurring in
        different positions of a jump [jmp], e.g., in [cond],
        [target], etc.  *)
    val exps : t -> exp seq

    (** [free_vars jmp] returns a set of all variables that are free
        in some expression in the jump [jmp].  *)
    val free_vars : t -> Var.Set.t

    (** [map_exp jmp ~f] applies [f] to each expression in a [jmp],
        e.g., conditions and indirect labels.  *)
    val map_exp : t -> f:(exp -> exp) -> t

    (** [substitute jmp x y] substitutes [x] by [y] in all expressions
        that occur in jump [jmp] expressions.*)
    val substitute : t -> exp -> exp -> t

    (** updated jump's guard condition  *)
    val with_cond : t -> exp -> t
    (** updated jump's kind  *)
    val with_kind : t -> jmp_kind -> t

    include Regular with type t := t
  end

  (** PHI-node  *)
  module Phi : sig
    (** Phi nodes are used to represent a set of values that can be
        assigned to a given variable depending on a control flow path
        taken.  Phi nodes should occur only in blocks that has more
        than one incoming edge, i.e., in blocks to which there is a
        transfer of control flow from more than one block.

        Each element of a phi-node corresponds to a particular
        incoming edge. *)
    type t = phi term

    (** [create var label exp] creates a phi-node that associates a
        variable [var] with an expression [exp]. This expression
        should be selected if a control flow enters a block, that owns
        this phi-node from a block labeled with [label]. Example,
        [create x loop_header y].*)
    val create : ?tid:tid -> var -> tid -> exp -> t

    (** [of_list var bindings] creates a phi-node, that for each pair
        of [label,exp] in the [bindings] list associates variable [var]
        with expression [exp] if control flow reaches this point via block
        labeled with [label].  *)
    val of_list : ?tid:tid -> var -> (tid * exp) list -> t

    (** [values phi] enumerate all possible values.  *)
    val values : t -> (tid * exp) seq

    (** [free_vars t] returns a set of variables that occur free on
        the right hand side of the phi-node. See {Exp.free_vars} for
        clarification on what variables are considered free.  *)
    val free_vars : t -> Var.Set.t

    (** [lhs phi] returns a variable associated with a phi node  *)
    val lhs : t -> var

    (** [with_lhs phi var] updates a left hand side of [phi] with
        [var] *)
    val with_lhs : t -> var -> t

    (** [map_exp t ~f] applies [f] to all expressions on the right
        hand side of a phi-node [t] *)
    val map_exp : t -> f:(exp -> exp) -> t

    (** [substitute phi x y] substitutes [x] by [y] in all right
        hand-side expressions of the [phi] node. *)
    val substitute : t -> exp -> exp -> t

    (** [update phi label exp] associates expression [exp] with a
        control flow path labeled with [label].  *)
    val update : t -> tid -> exp -> t

    (** [select phi label] takes the value corresponding to a control
        flow path marked with [label].   *)
    val select : t -> tid -> exp option

    (** [select_or_unknown phi label] is [exp] if
        [select phi label = Some exp], otherwise returns a
        [Bil.unknown] expression.     *)
    val select_or_unknown : t -> tid -> exp

    (** [remove def id] removes definition with a given [id]  *)
    val remove : t -> tid -> t

    include Regular with type t := t
  end

  (** Subroutine argument.  *)
  module Arg : sig
    (** In the IR model subroutines are not functions, that has a return
        value, but a more general subroutine that has a set of
        arguments, that can be used for  input, output or both
        purposes. *)

    type t = arg term

    (** [create ?intent var exp] creates an argument. If intent is
        not specified it is left unknown.   *)
    val create : ?tid:tid -> ?intent:intent -> var -> exp -> t

    (** [lhs arg] returns a variable associated with the argument.  *)
    val lhs : t -> var

    (** [rhs arg] returns an expression to which argument is
        bound.  *)
    val rhs : t -> exp

    (** [intent arg] returns the argument intent. The [None] value
        denontes unknown intent.  *)
    val intent : t -> intent option

    (** [with_intent intent arg] updates argument intent  *)
    val with_intent : t -> intent -> t

    (** removes the intent from an argument  *)
    val with_unknown_intent : t -> t


    (** {2 Attributes}  *)

    (** a caller of the subroutine must use an argument tagged with
        this attribute. This is useful for subroutines where not
        checking the result is either a security problem or always a
        bug, such as [realloc] *)
    val warn_unused : unit tag


    (** the size of allocated memory is the product of arguments
        marked with [alloc_size] attribute  *)
    val alloc_size : unit tag


    (** format(DSL) the specified argument of a subroutine is
        actually a format string written in a corresponding DSL. *)
    val format : string tag

    (** a contract requirement that this argument is not NULL.  *)
    val nonnull : unit tag

    include Regular with type t := t
  end

  (** A control transfer to another subroutine.  *)
  module Call : sig
    (** calls have two-fold representation. From the intra-procedural
        point of view call is a transfer of control to the next
        address with a side effect of calling to other
        subroutine. From the iter-procedural point of view, call is
        transfer of control from caller to the callee, that may or may
        not result in a return to the caller side.  Thus each call is
        represented by two labels. The [target] label points to the
        procedure that is called, the [return] label denotes a block
        to which the control flow should (but may not) continue when
        called subroutine returns.  *)

    type t = call


    (** [create ?return ~target ()] creates a call to the [target]
        subroutine. If [return] is not provided, that it is assumed that
        subroutine doesn't return. *)
    val create : ?return:label -> target:label -> unit -> t

    (** returns the target of the call  *)
    val target : t -> label

    (** returns call continuation  *)
    val return : t -> label option

    (** updates target  *)
    val with_target : t -> label -> t

    (** updates return continuation *)
    val with_return : t -> label -> t

    (** marks call as a "noreturn"  *)
    val with_noreturn : t -> t

    include Regular with type t := t
  end

  (** Target of a control flow transfer.  *)
  module Label : sig
    (** Labels can be direct, that are known to us. Or indirect, that
        are arbitrary expressions.  *)

    type t = label

    (** [create ()] creates a new label with a freshly generated
        identifier.  *)
    val create : unit -> t

    (** [direct label] creates a direct label with a given identifier.  *)
    val direct : tid -> t

    (** [indirect exp] creates a label that is resolved to an
        expression [exp] *)
    val indirect : exp -> t

    (** updates the label  *)
    val change : ?direct:(tid -> tid) -> ?indirect:(exp -> exp) -> t -> t

    include Regular with type t := t
  end

  (** Source of information.*)
  module Source : sig
    type 'a t = 'a Or_error.t stream
    type 'a source = 'a t

    (** Factory of data processors.
        Allows to store a registry of processors. *)
    module type Factory = sig
      type t

      (** [list source] is a list of names of source providers *)
      val list : unit -> string list

      (** [create name args] finds a source provider with the
          given name and creates it *)
      val find : string -> t source option

      (** [register name cons] registers a method that creates a given
          source of information. If a method with the given name already
          exists, then it will be superceeded by a new one.  *)
      val register : string -> t source -> unit
    end

    module Factory(T : T) : Factory with type t = T.t
  end

  (** Abstract taint.

      We represent a taint with a term identifier, to designated,
      that a taint was produced by a term with the given id. A taint
      set is usually associated with each variable of a given
      term. This set defines a set of taints with which a variable is
      tainted.*)
  module Taint : sig
    type t = tid

    type set = Tid.Set.t [@@deriving bin_io, compare, sexp]
    type map = set Var.Map.t [@@deriving bin_io, compare, sexp]


    (** value stored in register is source of taint  *)
    val reg : t tag

    (** value stored at memory location, that is stored
        in the register is tainted.*)
    val ptr : t tag

    val regs : map tag

    val ptrs : map tag

    val merge : map -> map -> map

    class context :  object('s)
      (** taint result with the given set of taints  *)
      method taint_reg : Bil.result -> set -> 's

      (** taint memory region [addr, addr+size] with the given set of taints  *)
      method taint_ptr : addr -> size -> set -> 's

      (** returns a set of taints associated with a given result of computation  *)
      method reg_taints : Bil.result -> set

      (** returns a set of taints associated with a given address   *)
      method ptr_taints : addr -> set

      (** returns all known taints.  *)
      method all_taints : set
    end

    (** Propagate taint through expressions.


        {2 Semantics}

        {3 Grammar}

        The following syntactic forms are used in propagation rules:

        [*a] - load from address [a], where [a] is immediate value;
        [*a <- v] - store value [v] at address [a];
        [exp ~> v] - expression reduces to value [v];
        [v -> t] - value [v] is tainted by a taint [t];
        [<bop>] - BIL binary operation or BIL concat expression;
        [<uop>] - BIL unary, extract or cast expression.


        {3 Rules}

        Value [v] is tainted by taint [t], denoted as [v -> t], if there
        exists a deriviation of the following rules, proving this fact.

        {v

    *a ~> v
    a -> t
    ---------------- :: p_load
    v -> t

    *a <- v
    v -> t
    ---------------- :: p_store
    a -> t

    v1 <bop> v2 ~> v3
    v1 -> t
    ----------------- :: p_bop_lhs
    v3 -> t

    v1 <bop> v2 ~> v3
    v2 -> t
    ----------------- :: p_bop_rhs
    v3 -> t

    <uop> v1 ~> v2
    v1 -> t
    ----------------- :: p_uop
    v2 -> t

    v}

        Note 1: this class overrides only methods, that computes non-leaf
        expressions, leaving a space for extension for derived classes.

        Note 2: we do not propagate taint from condition to branches in the
        if/then/else expression, since we're propagating only data
        dependency, not control dependency.

        Although, one can argue, that in expression [if c then x else y]
        the result depends on [c], since if we change [c] we will get
        different results, there is a good reason for not propagating this
        dependency - the consistency with BIR and BIL. Consider, BIL's
        [if] statement or BIR's conditional jump. If we will start to
        propagate taint from condition in [ite] expression, then we should
        also propagate it in BIL's and BIR's conditionals. Unfortunatelly
        the latter is not possible.

    *)
    class ['a] propagator : object('s)
      constraint 'a = #context
      inherit ['a] expi
    end


    val pp_set : set printer

    val pp_map : map printer

    module Map : Regular with type t = map
  end


  type 'a source = 'a Source.t

  (** Symbolizer maps addresses to function names  *)
  module Symbolizer : sig

    (** symbolizer data type  *)
    type t = symbolizer

    (** [create fn] creates a symbolizer for a given function  *)
    val create : (addr -> string option) -> t

    (** [of_blocks] produces a symbolizer from a serialized
        sequence of blocks. Each element of the sequence is deconstructed
        as [(name,ba,ea)], where [name] is a subroutine name, [ba] is a
        virtual address of a block start, and [ea] is an address of the
        block end.  *)
    val of_blocks : (string * addr * addr) seq -> t


    (** [resolve symbolizer addr] returns a name of function,
        to which a given address belongs. If the address is not know to
        the symbolizer, then the name is constructed from an address *)
    val resolve : t -> addr -> string

    (** [chain ss] creates a symbolizer, that will try to resolve
        an address using each symbolizer in order. *)
    val chain : t list -> t

    (** [empty] is a symbolizer that knows nothing.  *)
    val empty : t

    (** A factory of symbolizers. Use it register and create
        symbolizers.  *)
    module Factory : Source.Factory with type t = t
  end

  (** Rooter find start of functions in the binary. *)
  module Rooter : sig
    type t = rooter

    (** [create seq] creates a rooter from a given sequence of addresses  *)
    val create : addr seq -> t

    (** [of_image img] create a rooter that will use existing symbol
        information inside the image, to find roots. *)
    val of_image : image -> t

    (** [of_blocks] produces a rooter from a serialized
        sequence of blocks. Each element of the sequence is deconstructed
        as [(name,ba,ea)], where [name] is a subroutine name, [ba] is a
        virtual address of a block start, and [ea] is an address of the
        block end.  *)
    val of_blocks : (string * addr * addr) seq -> t


    (** [roots r] enumerates roots found by rooter [r]  *)
    val roots : t -> addr seq

    (** [union r1 r2] joins roots from rooters [r1] and [r2]  *)
    val union : t -> t -> t

    (** A factory of rooters. Useful to register custom rooters  *)
    module Factory : Source.Factory with type t = t
  end

  (** Brancher is responsible for resolving destinations of branch
      instructions.   *)
  module Brancher : sig
    open Disasm_expert.Basic
    type t = brancher

    type dest = addr option * edge [@@deriving sexp]
    type dests = dest list [@@deriving sexp]

    val create : (mem -> full_insn -> dests) -> t

    val of_bil : arch -> t

    val resolve : t -> mem -> full_insn -> dests

    module Factory : Source.Factory with type t = t

  end

  (** Reconstructor is responsible for reconstructing symbol table
      from a CFG. It should partition a CFG into a set of possibly
      intersecting functions. See {!Symtab} module for more
      information about symbol table and functions. *)
  module Reconstructor : sig
    type t = reconstructor

    (** [create f] creates a reconstructor from a given function [f]  *)
    val create : (cfg -> symtab) -> t

    (** [default name roots] builds a reconstructor from given a
        function, that maps addresses to function names (see
        {!Symbolizer}) and a list of known function starts. The
        reconstructor will extend the list of function start with
        destinations of call instructions found in the CFG, and then
        for each function start build a function using the following
        definition of a function:

           Function is built from the entry block and every block that
           is reachable from it without using calls, if the block
           address is greater than the entry block address and less
           than the address of entry block of the next symbol.

        Note: this is an approximation, that works fine for most cases.  *)
    val default : (word -> string) -> word list -> t

    (** [of_blocks] produces a reconstructor from a serialized
        sequence of blocks. Each element of the sequence is deconstructed
        as [(name,ba,ea)], where [name] is a subroutine name, [ba] is a
        virtual address of a block start, and [ea] is an address of the
        block end.  *)
    val of_blocks : (string * addr * addr) seq -> t

    (** [run reconstructor cfg] reconstructs a symbol table from a
        given cfg  *)
    val run : t -> cfg -> symtab

    (** a factory of reconstructors  *)
    module Factory : Source.Factory with type t = t
  end

  module Event : sig
    type t = ..
    type event = t = ..


    val stream : t stream

    val send : t -> unit


    (** [register_printer f] when event [e] is printed, [f e] must be
        [None] if [f] is not a subset of events, that is intended to be
        printed by an [f]. If it is [Some str], then [str] is printed
        out.

        If more than one printer returns [Some thing] for the same event,
        then the last registered has the precedence.
    *)
    val register_printer : (t -> string option) -> unit

    module Log : sig
      type level =
        | Debug
        | Info
        | Warning
        | Error

      type info = {
        level : level;
        section : string;
        message : string;
      }

      type event += Message of info

      val message :  level -> section:string -> ('a,Format.formatter,unit) format -> 'a
    end

    include Printable with type t := t
  end

  type event = Event.t = ..

  type project

  (** Target of analysis.  *)
  module Project : sig
    (** A project groups together all the information recovered from
        the underlying binary. It is also used for exchanging
        information between {{!section:project}passes}.  *)

    type t = project
    type input

    (** IO interface to a project data structure.  *)
    include Data with type t := t

    (** [from_file filename] creates a project from a provided input
        source. The reconstruction is a multi-pass process driven by
        the following input variables, provided by a user:

        - [brancher] decides instruction successors;
        - [rooter] decides function starts;
        - [symbolizer] decides function names;
        - [reconstructor] provides algorithm for symtab reconstruction;

        The project is built incrementally and iteratively until a
        fixpoint is reached. The fixpoint is reached when an
        information stops to flow from the input variables.

        The overall algorithm of can depicted with the following
        diargram, where boxes denote data and ovals denote processes:

        {v
               +---------+   +---------+   +---------+
               | brancher|   |code/data|   |  rooter |
               +----+----+   +----+----+   +----+----+
                    |             |             |
                    |             v             |
                    |        -----------        |
                    +------>(   disasm  )<------+
                             -----+-----
                                  |
                                  v
              +----------+   +---------+   +----------+
              |symbolizer|   |   CFG   |   | reconstr +
              +-----+----+   +----+----+   +----+-----+
                    |             |             |
                    |             v             |
                    |        -----------        |
                    +------>(  reconstr )<------+
                             -----+-----
                                  |
                                  v
                             +---------+
                             |  symtab |
                             +----+----+
                                  |
                                  v
                             -----------
                            (  lift IR  )
                             -----+-----
                                  |
                                  v
                             +---------+
                             | program |
                             +---------+

       v}


        The input variables, are represented with stream of
        values. Basically, they can be viewed as cells, that depends
        on some input. When input changes, the value is recomputed and
        passed to the stream. Circular dependencies are allowed, so a
        rooter may actually depend on the [program] term. In case of
        circular dependencies, the above algorithm will be run
        iteratively, until a fixpoint is reached. A criterium for the
        fixpoint, is when no data need to be recomputed. And the data
        must be recomputed when its input is changed or needs to be
        recomputed.

        User provided input can depend on any information, but a good
        start is the information provided by the {!Info} module. It
        contains several variables, that are guaranteed to be defined
        in the process of reconstruction.

        For example, let's assume, that a [create_source] function
        actually requires a filename as its input, to create a source
        [t], then it can be created as easily as:

        [Stream.map Input.file ~f:create_source]

        As a more complex, example let's assume, that a source now
        requires that both [arch] and [file] are known. We can combine
        two different streams of information with a [merge] function:

        [Stream.merge Input.file Input.arch ~f:create_source], where
        [create_source] is a function of type: [string -> arch -> t].

        If the source requires more than two arguments, then a
        [Stream.Variadic], that is a generalization of a merge
        function can be used. Suppose, that a source of information
        requires three inputs: filename, architecture and compiler
        name. Then we first define a list of arguments,

        [let args = Stream.Variadic.(args Input.arch $Input.file $Compiler.name)]

        and apply them to our function [create_source]:

        [Stream.Variadic.(apply ~f:create_source args].


        Sources, specified in the examples above, will call a [create_source]
        when all arguments changes. This is an expected behavior for
        the [arch] and [file] variables, since the do not change during
        the program computation. Mixing constant and non-constant
        (with respect to a computation) variables is not that easy, but
        still can be achieved using [either] and [parse] combinators.
        For example, let's assume, that a [source] requires [arch] and
        [cfg] as its input:

        {[
          Stream.either Input.arch Input.cfg |>
          Stream.parse inputs ~init:nil ~f:(fun create -> function
              | First arch -> None, create_source arch
              | Second cfg -> Some (create cfg), create)
        ]}

        In the example, we parse the stream that contains either
        architectures or control flow graphs with a state of type,
        [cfg -> t Or_error.t]. Every time an architecture is changed,
        (i.e., a new project is started), we recreate a our state,
        by calling the [create_source] function. Since, we can't
        proof, that architecture will be decided before the [cfg], or
        decided at all we need to provide an initial [nil] function.
        It can return either a bottom value, e.g.,
        [let nil _ = Or_error.of_string "expected arch"]

        or it can just provide an empty information.
    *)
    val create :
      ?disassembler:string ->
      ?brancher:brancher source ->
      ?symbolizer:symbolizer source ->
      ?rooter:rooter source ->
      ?reconstructor:reconstructor source ->
      input -> t Or_error.t


    (** [arch project] reveals the architecture of a loaded file  *)
    val arch : t -> arch

    (** [disasm project] returns results of disassembling  *)
    val disasm : t -> disasm

    (** [program project] returns a program lifted into {{!sema}IR}  *)
    val program : t -> program term

    (** [with_program project program] updates a project program *)
    val with_program : t -> program term -> t

    (** [symbols t] returns reconstructed symbol table  *)
    val symbols : t -> symtab

    (** [with_symbols project symbols] updates [project] symbols  *)
    val with_symbols : t -> symtab -> t

    (** [memory t] returns the memory as an interval tree marked with
        arbitrary values.   *)
    val memory : t -> value memmap

    (** [tag_memory project region tag value] tags a given [region] of
        memory in [project] with a given [tag] and [value]. Example:
        [Project.tag_memory project tained color red]
    *)
    val tag_memory : t -> mem -> 'a tag -> 'a -> t

    (** [substitute p region tag value] is like
        {{!tag_memory}tag_memory}, but it will also apply
        substitutions in the provided string value, as per OCaml
        standard library's [Buffer.add_substitute] function.

        Example: {[
          Project.substitute project comment "$symbol starts at $symbol_addr"
        ]}

        The following substitutions are supported:

        - [$section{_name,_addr,_min_addr,_max_addr}] - name of region of file
        to which it belongs. For example, in ELF this name will
        correspond to the section name

        - [$symbol{_name,_addr,_min_addr,_max_addr}] - name or address
        of the symbol to which this memory belongs

        - [$asm] - assembler listing of the memory region

        - [$bil] - BIL code of the tagged memory region

        - [$block{_name,_addr,_min_addr,_max_addr}] - name or address of a basic
        block to which this region belongs

        - [$min_addr, $addr] - starting address of a memory region

        - [$max_addr] - address of the last byte of a memory region. *)
    val substitute : t -> mem -> string tag -> string -> t

    (** [with_memory project] updates project memory. It is
            recommended to use {!tag_memory} and {!substitute} instead of this
        function, if possible.  *)
    val with_memory : t -> value memmap -> t

    (** {3 Extensible record}

        Project can also be viewed as an extensible record, where one
        can store arbitrary values. Example,
        {[
          let p = Project.set project color `green
        ]}
        This will set field [color] to a value [`green].*)

    (** [set project field value] sets a [field] to a give value. If
        [field] was already set, then new value overrides the old
        one. Otherwise the field is added.  *)
    val set : t -> 'a tag -> 'a -> t

    (** [get project field] returns the value of the [field] if it
        exists *)
    val get : t -> 'a tag -> 'a option

    (** [has project field] checks whether field exists or not. Useful
        for fields of type unit, that actually isomorphic to bool fields,
        e.g., [if Project.has project mark]
    *)
    val has : t -> 'a tag -> bool

    (** Information obtained during project reconstruction.

        These pieces of information are guaranteed to be discovered
        during the project reconstruction. See {!Project.create}
        function for more information on the reconstruction process. *)
    module Info : sig
      (** occurs everytime a new file is opened. The value is a filename  *)
      val file : string stream

      (** occurs once input architecture is known  *)
      val arch : arch stream

      (** occurs once input memory is loaded  *)
      val data : value memmap stream

      (** occurs once code segment is discovered  *)
      val code : value memmap stream

      (** occurs everytime a whole program control flow graph is changed  *)
      val cfg : cfg stream

      (** occurs everytime a symbol table is changed  *)
      val symtab : symtab stream

      (** occurs every time a program term is changed during the
          project reconstruction process.   *)
      val program : program term stream
    end

    module Input : sig
      type t = input
      val file : ?loader:string -> filename:string -> t
      val binary : ?base:addr -> arch -> filename:string -> t
      val create : arch -> string -> code:value memmap -> data: value memmap -> t
      val register_loader : string -> (string -> t) -> unit
      val available_loaders : unit -> string list
    end


    (** {3 Registering passes}

        To add new pass one of the following [register_*] functions
        should be called.*)

    type pass

    (** [register_pass ?autorun ?runonce ?deps ?name pass] registers a
        [pass] over a project.

        If [autorun] is [true], then the host program will run this
        pass automatically. If [runonce] is true, then for a given
        project the pass will be run only once. Each repeating
        attempts to run the pass will be ignored. The [runonce]
        parameter defaults to [false] when [autorun] is [false], and
        to [true] otherwise.

        Parameter [deps] is list of dependencies. Each dependency is a
        name of a pass, that should be run before the [pass]. The
        dependencies will be run in a specified order every time the
        [pass] is run.

        To get access to command line arguments use [Plugin.argv] *)
    val register_pass :
      ?autorun:bool ->           (** defaults to [false] *)
      ?runonce:bool ->           (** defaults to [autorun]  *)
      ?deps:string list -> ?name:string -> (t -> t) -> unit

    (** [register_pass' pass] registers [pass] that doesn't modify
        the project effect and is run only for side effect.
        (See {!register_pass})  *)
    val register_pass':
      ?autorun:bool ->           (** defaults to [false] *)
      ?runonce:bool ->           (** defaults to [autorun]  *)
      ?deps:string list -> ?name:string -> (t -> unit) -> unit

    (** [passes ()] returns all currently registered passes.  *)
    val passes : unit -> pass list

    val find_pass : string -> pass option

    (** time duration in seconds  *)
    type second = float

    module Pass : sig

      type t = pass
      (** An error that can occur when loading or running pass.
          - [Not_loaded name] pass with a given [name] wasn't loaded for
          some reason. This is a very unlikely error, indicating
          either a logic error in the plugin system implementation or
          something very weird, that we didn't expect.


          - [Not_loaded name] when we tried to load plugin with a given
          [name] we failed to find it in our search paths.

          - [Runtime_error (name,exn)] when plugin with a given [name]
          was run it raised an [exn].

      *)
      type error =
        | Unsat_dep of pass * string
        | Runtime_error of pass * exn
        [@@deriving sexp_of]

      (** raised when a pass failed to load or to run. Note: this
          exception is raised only from two functions in this module, that
          state this in their documentation and has [_exn] suffix in their
          name. *)
      exception Failed of error [@@deriving sexp]


      (** [run_pass project pass] applies [pass] to a [project].

          If a pass has dependencies, then they will be run before the
          pass in some topological order. *)
      val run : t -> project -> (project,error) Result.t

      (** [run_pass_exn proj] is the same as {!run_pass}, but raises an
          exception on error. Useful to provide custom error
          handling/printing.

          @raise Pass_failed if failed to load, or if plugin failed at
          runtime.  *)
      val run_exn : t -> project -> project


      (** [name plugin] is a plugin name  *)
      val name : t -> string


      (** [autorun plugin] is [true] if a [plugin] was created with
          autorun option *)
      val autorun : t -> bool

    end

    (**/**)
    val restore_state : t -> unit
    (**/**)
  end
end
