open Core_kernel.Std
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

      Vector is an implementation of STL like vectors with logarithmic
      push back.

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


      {3 Graph library}

      {!Graphlib} is a generic library that extends a well known
      OCamlGraph library. {!Graphlib} uses its own, more reach,
      {!Graph} interface that is isomorphic to OCamlGraph's [Sigs.P]
      signature for persistant graphs. Two functors witness the
      isomorphism of the interfaces:
      {!Graphlib.To_ocamlgraph} and {!Graphlib.Of_ocamlgraph}. Thanks
      to this functors any algorithm written for OCamlGraph can be
      used on [Graphlibs] graph and vice verse.

      The {!Graph} interface provides a richer interface in a Core
      style. Nodes and Edges implements {!Opaque} data structure,
      i.e., they come with Maps, Sets, Hashtbls, etc, preloaded (e.g.,
      [G.Node.Set] is a set of node for graph implementation, provided
      by a module named [G]). Graphs also implement {!Printable}
      interface, that makes them much easier to debug.

      Along with graphs, auxiliary data structures are provided, like
      {{!Path}path} to represent paths in graph, {{!Tree}tree} for
      representing different graph spannings, {{!Partition}partition}
      for graph partitioning, and more.

      {!Graphlib} is a library that provides a set of generic
      algorithms, as well as implementations of a {!Graph} interface,
      and a suite of preinstantiated graphs.

      Contrary to OCamlGraph, each {!Graphlib} interface is provided
      as a function, not a functor. Thus making there use syntactically
      easier. Also, {!Graphlib} heavily uses optional and keyword
      parameters. For die-hards, may algorithms are still have functor
      interface.

      All {!Graphlib} algorithms accept a first-class module with
      graph implementation as a first argument. You can think of this
      parameter as an explicit type class. Later, when modular
      implicits will be accepted in OCaml, this parameter can be
      omitted. But for now, we need to pass it.

      A recommended way to work with {!Graphlib} is to bind the
      chosen implementation with some short name, usually [G] would be
      a good choice:

      {[module G = Graphlib.String.Bool]}

      This will bind name [G] with a graph implementation that has
      [string] nodes, with edges marked by values of type [bool].

      To create a graph of type [G.t] one can use a generic
      {!Graphlib.create} function:

      {[let g = Graphlib.create (module G) ~edges:[
          "entry", "loop", true;
          "loop", "exit", true;
          "loop", "loop", false;
        ] ()]}

      This will create an instance of type [G.t]. Of course, it is
      still possible to use non-generic [G.empty], [G.Node.insert],
      [G.Edge.insert].
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
      independently from the target platform. Modules of type
      {{!ABI}ABI} provide even more information, e.g., it maps
      registers to formals. The module type {{!Target}Target} brings
      [CPU] and [ABI] together. To get an instance of this module,
      you can use the {{!target_of_arch}target_of_arch} function. For
      accessing all information about target platform, use the
      following modules that expose low-level and platform-specific
      details:

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


  (** {2:aux Auxiliary libraries}

      {3:dwarf DWARF library}

      The Dwarf library provides an access to DWARF debugging
      information. It implements parsing of some subset of DWARF
      features, and a high-level interface {{!Dwarf.Fbi}Fbi} that
      extracts function symbols from the given DWARF data.

      {3:sigs Byteweight}

      This {{!Byteweight}library} implements a byteweight algorithm
      that identifies functions in stripped binaries, as well as
      being able to train itself on a provided training corpus.
  *)

  (** {1:api BAP API}  *)

  (** Access to BAP configuration variables  *)
  module Config : sig
    val pkg_version : string
  end

  (** ['a printer] defines a type for pretty-printers for a value of
      type ['a]. This is the type, that is required by [%a] specifier,
      for [Format.printf]-family of functions. Also, this is the type,
      that can be installed into OCaml toplevel or debugger.

      Note: `bap.top` library automatically installs all printers. *)
  type 'a printer = Format.formatter -> 'a -> unit


  (** Printable interface is implemented by a significant amount of
      BAP types.  *)
  module type Printable = sig
    type t

    (** [to_string x] returns a human-readable representation of [x]  *)
    val to_string : t -> string

    (** [str () t] is formatted output function that matches "%a"
        conversion format specifier in functions, that prints to string,
        e.g., [sprintf], [failwithf], [errorf] and, suprisingly all
        [Lwt] printing function, including [Lwt_io.printf] and logging
        (or any other function with type ('a,unit,string,...)
        formatN`. Example:

        [Or_error.errorf "type %a is not valid for %a"
          Type.str ty Exp.str exp] *)
    val str : unit -> t -> string

    (** synonym for [str]  *)
    val pps : unit -> t -> string

    (** will print to a standard [output_channel], useful for using in
        [printf], [fprintf], etc. *)
    val ppo : out_channel -> t -> unit

    (** prints a sequence of values of type [t] *)
    val pp_seq : Format.formatter -> t Sequence.t -> unit


    (** this will include [pp] function from [Core] that has type
        {{!printer}[t printer]}, and can be used in [Format.printf]
        family of functions *)
    include Pretty_printer.S     with type t := t
  end

  (** Regular types models a general concept of value, i.e., something
      that can be used in way similar to regular [int], [string],
      [char] and other built in types. So that it can be compared, used
      in maps, sets, hashtables, printer, etc.

      Note: this signature is pretty similar to core's [Identifiable],
      but doesn't require [of_string] function, that is usually much
      harder to implement in comparison with [to_string] function. Also,
      instead of [to_string] it requires [pp] function that can be
      implemented much more efficiently and elegantly. From the [pp]
      function the whole plethora of printing functions are derived:
      [str], [pps], [ppo]. *)
  module type Regular = sig
    type t with bin_io, sexp, compare
    include Printable            with type t := t
    include Comparable.S_binable with type t := t
    include Hashable.S_binable   with type t := t
  end

  (** Opaque type is like regular type, except that we can print or
      examine it in any way. So it can't be serialized or
      pretty-printed. An {!Opaque.Make} can create an instances of
      such type.  *)
  module type Opaque = sig
    type t
    include Comparable with type t := t
    include Hashable   with type t := t
  end

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

  (** In order to implement [Regular] interface you need to provide a
      minimum implementation [M]  *)
  module Regular : sig
    module Make( M : sig
        (** type t should be binable, sexpable and provide compare function  *)
        type t with bin_io, sexp, compare
        include Pretty_printer.S with type t := t
        val hash : t -> int
        val module_name : string option
      end) : Regular with type t := M.t
  end

  (** creates a module implementing [Opaque] interface.   *)
  module Opaque : sig
    module Make(S : sig
        type t with compare
        val hash : t -> int
      end) : Opaque with type t := S.t
  end


  (** Lazy sequence  *)
  module Seq : sig
    type 'a t = 'a Sequence.t
    include module type of Sequence with type 'a t := 'a t

    (** for compatibility with Core <= 111.28  *)
    val filter : 'a t -> f:('a -> bool) -> 'a t
    val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int

    val of_array : 'a array -> 'a t

    val cons : 'a -> 'a t -> 'a t

    val is_empty : 'a t -> bool

    val pp : 'a printer -> 'a t printer
    val pp_bools : bool t printer
    val pp_chars : char t printer
    val pp_floats : float t printer
    val pp_ints : int t printer
    val pp_strings : string t printer

  end

  (** type abbreviation for ['a Sequence.t]  *)
  type 'a seq = 'a Seq.t

  (** [x ^:: xs] is a consing operator for sequences  *)
  val (^::) : 'a -> 'a seq -> 'a seq

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
    type 'a t with bin_io, sexp

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
      type token with bin_io, compare, sexp

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
      type t  with bin_io, compare, sexp
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
  type word with bin_io, compare, sexp

  (** A synonym for [word], that should be used for words
      that are addresses  *)
  type addr = word with bin_io, compare, sexp

  (** Type safe operand and register sizes.  *)
  module Size : sig
    (** Defines possible sizes for operations operands  *)
    type all = [
      | `r8
      | `r16
      | `r32
      | `r64
    ] with variants

    type 'a p = 'a constraint 'a = [< all]
    with bin_io, compare, sexp

    type t = all p
    with bin_io, compare, sexp

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

    (** [to_bits size] returns the number of bits. *)
    val to_bits  : 'a p -> int
    val to_bytes : 'a p -> int

    include Regular with type t := t
  end

  (** size of operand  *)
  type size = Size.t
  with bin_io, compare, sexp

  (** size of address  *)
  type addr_size = [ `r32 | `r64 ] Size.p
  with bin_io, compare, sexp

  (** just a fancy type abbreviation  *)
  type nat1 = int
  with bin_io, compare, sexp


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
    include Comparable.With_zero with type t := t
    include Integer with type t := t

    (** A comparable interface with size-monomorphic comparison. *)
    module Mono : Comparable with type t := t

    (** [Width] exception is raised when size-monomorphic operation is
        applied to operands with different sizes. *)
    exception Width with sexp

    (** Specifies the order of bytes in a word. *)
    type endian =
      | LittleEndian (** least significant byte comes first  *)
      | BigEndian    (** most  significant byte comes first  *)
    with bin_io, compare, sexp

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

    (** [to_bytes x order] returns bytes of [x] in a specified [order].
        Each byte is represented as a [bitvector] itself. *)
    val to_bytes : t -> endian ->    t seq
    (** [to_bytes x order] returns bytes of [x] in a specified [order],
        with bytes represented by [char] type *)
    val to_chars : t -> endian -> char seq

    (** [to_bits x order] returns bits of [x] in a specified [order].
        [order] defines only the ordering of words in a bitvector, bits
        will always be in MSB first order. *)
    val to_bits  : t -> endian -> bool seq

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
  with sexp,bin_io,compare

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
    with variants

    (** BIL type is regular  *)
    include Regular with type t := t
  end

  (** short abbreviation for a type  *)
  type typ = Type.t
  with bin_io, compare, sexp

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
  with bin_io, compare, sexp

  (** BIL variable.
      BIL variables are regular values. Variables can versioned. I.e.,
      a version number can be added to a variable, to represent the
      same variable but at different time or space (control flow path).
      This is particulary useful for representing variables in SSA
      form.

      By default, comparison functions takes version number into
      account. In order to compare two variables regardless their
      version number use [same] function, or compare with [base x].

      Temporary variables (those, that are created with flag
      [tmp:true] argument, are treated differently. Every time a
      temporary variable is created a fresh new identifier is created
      and appended to a provided name (interleaved with '_'
      character). So that it is impossible to create two equal
      temporary variables.

  *)
  module Var : sig

    type t = var

    (** implements [Regular] interface  *)
    include Regular with type t := t

    (** [create ?tmp name typ] creates a variable with associated
        [name] and type [typ]. A newly created variable has version
        equal to 0.

        If [tmp] is [true] then a fresh new identifier is created by
        incrementing a hidden counter, and prepending it to a variable
        name.*)
    val create : ?tmp:bool -> string -> typ -> t

    (** [name var] returns a name assosiated with variable  *)
    val name : t -> string

    (** [typ var] returns a type assosiated with variable  *)
    val typ : t -> typ

    (** [is_tmp] true if variable is temporary  *)
    val is_tmp : t -> bool

    (** [renumber var ver] returns a variable, that is identical to
        [var], but with version equal to [ver] *)
    val renumber : t -> int -> t

    (** [version v] returns a variable version  *)
    val version : t -> int

    (** [base var] returns an original variable. Essentially,
        identical to [renumber var 0] *)
    val base : t -> t

    (** [same x y] compares variables ignoring versions, i.e.,
        [same x y] iff [equal (base x) (base y)] *)
    val same : t -> t -> bool

    (** Serialization format  *)
    module V1 : sig
      type r = string * int * typ * bool
      val serialize   : t -> r
      val deserialize : r -> t
    end
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
      with bin_io, compare, sexp

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
      with bin_io, compare, sexp

      (** Unary operations implemented in the IR *)
      type unop =
        | NEG (** Negate. (2's complement) *)
        | NOT (** Bitwise not. *)
      with bin_io, compare, sexp

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
      with bin_io, compare, sexp

      type stmt =
        | Move    of var * exp  (** assign value of expression to variable *)
        | Jmp     of exp        (** jump to absolute address *)
        | Special of string     (** Statement with semantics not expressible in BIL *)
        | While   of exp * stmt list (** while loops  *)
        | If      of exp * stmt list * stmt list (** if/then/else statement  *)
        | CpuExn  of int                         (** CPU exception *)
      with bin_io, compare, sexp
    end

    (** include all constructors into Bil namespace *)
    open Types
    include module type of Types with type cast = cast
                                  and type binop = binop
                                  and type unop = unop
                                  and type exp = exp
                                  and type stmt = stmt
    type t = stmt list
    with bin_io, compare, sexp


    include Printable with type t := t

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

    (** {2:bil_visitor AST Visitors}.

        [visitor] folds arbitrary value over the AST, [finder] is a
        visitor, that can prematurely finish the traversal, [mapper]
        that maps AST, allowing limited transformation of its
        structure.  *)

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

      (** a stack of expr, that are parents for the currenly visiting
          expression *)
      val exps_stack  : exp  list

      (** is [true] if we're visiting expression that is a jump target *)
      val in_jmp : bool

      (** is [true] if we're visiting expression that is on the left or
          right side of the assignment. *)
      val in_move : bool

      (** is [true] if currently visiting expression or statement is
          executed under condition.  *)
      val under_condition : bool
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
      inherit state
      (** Default entry point *)
      method run : stmt list -> 'a -> 'a

      (** {2 Statements}  *)
      method enter_stmt : stmt -> 'a -> 'a
      method visit_stmt : stmt -> 'a -> 'a
      method leave_stmt : stmt -> 'a -> 'a

      (** [Move(var,exp)]  *)
      method enter_move : var -> exp -> 'a -> 'a
      method visit_move : var -> exp -> 'a -> 'a
      method leave_move : var -> exp -> 'a -> 'a

      (** [Jmp exp]  *)
      method enter_jmp : exp -> 'a -> 'a
      method visit_jmp : exp -> 'a -> 'a
      method leave_jmp : exp -> 'a -> 'a

      (** [While (cond,bil)]  *)
      method enter_while : cond:exp -> stmt list -> 'a -> 'a
      method visit_while : cond:exp -> stmt list -> 'a -> 'a
      method leave_while : cond:exp -> stmt list -> 'a -> 'a

      (** [If (cond,yes,no)]  *)
      method enter_if : cond:exp -> yes:stmt list -> no:stmt list -> 'a -> 'a
      method visit_if : cond:exp -> yes:stmt list -> no:stmt list -> 'a -> 'a
      method leave_if : cond:exp -> yes:stmt list -> no:stmt list -> 'a -> 'a

      (** [CpuExn n]  *)
      method enter_cpuexn : int -> 'a -> 'a
      method visit_cpuexn : int -> 'a -> 'a
      method leave_cpuexn : int -> 'a -> 'a

      (** [Special string]  *)
      method enter_special : string -> 'a -> 'a
      method visit_special : string -> 'a -> 'a
      method leave_special : string -> 'a -> 'a

      (** {2 Expressions}  *)
      method enter_exp : exp -> 'a -> 'a
      method visit_exp : exp -> 'a -> 'a
      method leave_exp : exp -> 'a -> 'a

      (** [Load (src,addr,endian,size)]  *)
      method enter_load : mem:exp -> addr:exp -> endian -> size -> 'a -> 'a
      method visit_load : mem:exp -> addr:exp -> endian -> size -> 'a -> 'a
      method leave_load : mem:exp -> addr:exp -> endian -> size -> 'a -> 'a

      (** [Store (dst,addr,src,endian,size)]  *)
      method enter_store : mem:exp -> addr:exp -> exp:exp -> endian -> size -> 'a -> 'a
      method visit_store : mem:exp -> addr:exp -> exp:exp -> endian -> size -> 'a -> 'a
      method leave_store : mem:exp -> addr:exp -> exp:exp -> endian -> size -> 'a -> 'a

      (** [BinOp (op,e1,e2)]  *)
      method enter_binop : binop -> exp -> exp -> 'a -> 'a
      method visit_binop : binop -> exp -> exp -> 'a -> 'a
      method leave_binop : binop -> exp -> exp -> 'a -> 'a

      (** [Unop (op,e)]  *)
      method enter_unop : unop -> exp -> 'a -> 'a
      method visit_unop : unop -> exp -> 'a -> 'a
      method leave_unop : unop -> exp -> 'a -> 'a

      (** [Cast(kind,size,e)]  *)
      method enter_cast : cast -> nat1 -> exp -> 'a -> 'a
      method visit_cast : cast -> nat1 -> exp -> 'a -> 'a
      method leave_cast : cast -> nat1 -> exp -> 'a -> 'a

      (** [Let (v,exp,body)]  *)
      method enter_let : var -> exp:exp -> body:exp -> 'a -> 'a
      method visit_let : var -> exp:exp -> body:exp -> 'a -> 'a
      method leave_let : var -> exp:exp -> body:exp -> 'a -> 'a

      (** [Ite (cond,yes,no)]  *)
      method enter_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a
      method visit_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a
      method leave_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a

      (** [Extract (hi,lo,e)]  *)
      method enter_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a
      method visit_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a
      method leave_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a

      (** [Concat(e1,e2)]  *)
      method enter_concat : exp -> exp -> 'a -> 'a
      method visit_concat : exp -> exp -> 'a -> 'a
      method leave_concat : exp -> exp -> 'a -> 'a

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
      method find_in_bil : stmt list -> 'a option
      method find_in_exp : exp -> 'a option
    end

    (** AST transformation.
        mapper allows one to map AST, performing some limited
        amount of transformations on it. Mapper provides extra
        flexibility by mapping [stmt] to [stmt list], thus allowing
        to remove statements from the output (by mapping to empty list) or
        to map one statement to several. This is particularly useful when
        you map [if] or [while] statements. *)
    class mapper : object
      inherit state

      (** Default entry point.
          But again, you can use any method as an entry  *)
      method run : stmt list -> stmt list

      (** {3 Statements}  *)
      method map_stmt : stmt -> stmt list
      method map_move : var -> exp -> stmt list
      method map_jmp : exp -> stmt list
      method map_while : cond:exp -> stmt list -> stmt list
      method map_if : cond:exp -> yes:stmt list -> no:stmt list -> stmt list
      method map_cpuexn : int -> stmt list
      method map_special : string -> stmt list

      (** {3 Expressions}  *)
      method map_exp : exp -> exp
      method map_load : mem:exp -> addr:exp -> endian -> size -> exp
      method map_store : mem:exp -> addr:exp -> exp:exp -> endian -> size -> exp
      method map_binop : binop -> exp -> exp -> exp
      method map_unop : unop -> exp -> exp
      method map_cast : cast -> nat1 -> exp -> exp
      method map_let : var -> exp:exp -> body:exp -> exp
      method map_ite : cond:exp -> yes:exp -> no:exp -> exp
      method map_extract : hi:nat1 -> lo:nat1 -> exp -> exp
      method map_concat : exp -> exp -> exp
      method map_int : word -> exp
      method map_var : var -> exp
      method map_sym : var -> var
      method map_unknown : string -> typ -> exp
    end


    (** {2:bil_helpers BIL Helper functions}  *)


    (** {3 General purpose iterators}  *)

    (** [fold visitor ~init bil] folds visitor over BIL, passing init
        value through the tree nodes. See also {!Exp.fold} and
        {!Stmt.fold}.  *)
    val fold : 'a #visitor -> init:'a -> stmt list -> 'a

    (** [iter visitor bil] apply a visitor for each node of a BIL
        forest. See also {!Exp.iter} and {!Stmt.iter}. *)
    val iter : unit #visitor -> stmt list -> unit

    (** [map mapper bil] map or transform BIL program. See also
        {!Exp.map}. *)
    val map : #mapper -> stmt list -> stmt list

    (** [find finder bil] search in [bil] using provided [finder]. See
        also {!Exp.find} and {!Stmt.find}. *)
    val find : 'a #finder -> stmt list -> 'a option

    (** [exists finder bil] returns true if [finder] finds
        something. See also {!Exp.exists} and {!Stmt.exists}.*)
    val exists : unit #finder -> stmt list -> bool


    (** [is_referenced x p] is [true] if [x] is referenced in some
        expression or statement in program [p], before it is
        assigned. *)
    val is_referenced : var -> stmt list -> bool

    (** [is_assigned x p] is [true] if there exists such [Move]
        statement, that [x] occures on the left side of it. If
        [strict] is true, then only unconditional assignments are
        accounted. By default, [strict] is [false] *)
    val is_assigned : ?strict:bool -> var -> stmt list -> bool

    (** [prune_unreferenced p] remove all assignments to variables that
        are not used in the program [p]. This is a local optimization.
        The variable is unreferenced if it is not referenced in its lexical
        scope, or if it is referenced after the assignment. Only
        temporary variables are pruned, as their scope is local.  *)
    val prune_unreferenced : stmt list -> stmt list

    (** [normalize_negatives p] transform [x + y] to [x - abs(y)] if [y < 0] *)
    val normalize_negatives : stmt list -> stmt list

    (** [substitute x y p] substitutes each occurrence of expression [x] by
        expression [y] in program [p]. The mnemonic to remember the
        order is to recall the sed's [s/in/out] syntax. *)
    val substitute : exp -> exp -> stmt list -> stmt list

    (** [substitute_var x y p] substitutes all occurences of variable [x]
        by expression [y] *)
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

    (** [constant_folder] is a class that implements the [fold_consts]  *)
    class constant_folder : mapper

    (** [fixpoint f] applies transformation [f] until fixpoint is
        reached. If the transformation orbit contains non-trivial cycles,
        then the transformation will stop at an arbitrary point of a
        cycle. *)
    val fixpoint : (stmt list -> stmt list) -> (stmt list -> stmt list)

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

  (** [Regular] interface for BIL expressions *)
  module Exp : sig
    type t = Bil.exp

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
    val fold : 'a #Bil.visitor -> init:'a -> t -> 'a

    (** [iter visitor exp] iterates over all terms of the [exp] using
        provided visitor. See also {!Bil.iter} and {!Stmt.iter}  *)
    val iter : unit #Bil.visitor -> t -> unit

    (** [find finder exp] returns [Some thing] if finder finds some
        [thing]. See also {!Bil.find} and {!Stmt.find} *)
    val find : 'a #Bil.finder -> t -> 'a option

    (** [map mapper exp] maps [exp] tree using provided [mapper].
        See also {!Bil.map} *)
    val map  : #Bil.mapper -> t -> t

    (** [exists finder exp] is [true] if [finder] finds
        something. See also {!Bil.exists} and {Stmt.exists}  *)
    val exists : unit #Bil.finder -> t -> bool

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

    include Regular with type t := t
    val pp_adt : t printer
  end

  (** [Regular] interface for BIL statements  *)
  module Stmt : sig
    type t = Bil.stmt
    (** [fold ~init visitor stmt] folds a [stmt] with a visitor. See
        {!Bil.fold} and {!Exp.fold} for more details.  *)
    val fold : 'a #Bil.visitor -> init:'a -> t -> 'a

    (** [iter visitor stmt] iters over a [stmt] with a visitor. See
        {!Bil.iter} and {!Exp.iter} for more details.  *)
    val iter : unit #Bil.visitor -> t -> unit

    (** [find finder stmt] performs a lookup into the Bil statement. See
        {!Bil.find} and {!Exp.find} for more details.  *)
    val find : 'a #Bil.finder -> t -> 'a option


    (** [exists finder stmt] is [true] iff [find finder stmt <> None].
        See {!Bil.exists} and {!Exp.exists} for more details.  *)
    val exists : unit #Bil.finder -> t -> bool

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

    include Regular with type t := t
    val pp_adt : t printer
  end

  (** Architecture  *)
  module Arch : sig
    type x86 = [
      | `x86
      | `x86_64
    ] with bin_io, compare, enumerate, sexp

    type arm = [
      | `arm
      | `armeb
      | `armv4
      | `armv4t
      | `armv5
      | `armv6
      | `armv7
      | `thumb
      | `thumbeb
    ] with bin_io, compare, enumerate, sexp

    type aarch64 = [
      | `aarch64
      | `aarch64_be
    ]
    with bin_io, compare, enumerate, sexp

    type ppc = [
      | `ppc
      | `ppc64
      | `ppc64le
    ]
    with bin_io, compare, enumerate, sexp

    type mips = [
      | `mips
      | `mipsel
      | `mips64
      | `mips64el
    ]
    with bin_io, compare, enumerate, sexp

    type sparc = [
      | `sparc
      | `sparcv9
    ]
    with bin_io, compare, enumerate, sexp

    type nvptx = [
      | `nvptx
      | `nvptx64
    ]
    with bin_io, compare, enumerate, sexp

    type hexagon = [`hexagon]
    with bin_io, compare, enumerate, sexp

    type r600 = [`r600]
    with bin_io, compare, enumerate, sexp

    type systemz = [`systemz]
    with bin_io, compare, enumerate, sexp

    type xcore = [`xcore]
    with bin_io, compare, enumerate, sexp

    type t = [
      | aarch64
      | arm
      | hexagon
      | mips
      | nvptx
      | ppc
      | r600
      | sparc
      | systemz
      | x86
      | xcore
    ] with bin_io, compare, enumerate, sexp, variants

    (** [of_string s] will try to be clever and to capture all
        commonly known synonyms, e.g., [of_string "i686"] will
        work    *)
    val of_string : string -> t option

    (** [addr_size arch] returns an address size for a given [arch]  *)
    val addr_size : t -> addr_size

    (** [endian arch] returns a word endianness of the [arch]  *)
    val endian : t -> endian

    (** [arch] type implements [Regular]  interface  *)
    include Regular with type t := t
  end

  (** architecture  *)
  type arch = Arch.t
  with bin_io, compare, sexp


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
    type t with bin_io, compare, sexp

    (** Tag constructor of type ['a]  *)
    type 'a tag

    (** A required interface for the type to be lifted to value. *)
    module type S = sig
      (** In order to construct a value with the given type you must
          provide an implementation for marshaling functions,
          comparison function and pretty-printing.  *)
      type t with bin_io, compare, sexp
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
    type typeid with bin_io, compare, sexp

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

    end

    (** Persistent type identifiers.  *)
    module Typeid : Regular with type t = typeid

    (** Although values of type [value] implements regular interface
        it is recommended to used [dict] data structure instead of
        those, that are provided by [Regular] interface.x *)
    include Regular with type t := t
  end

  type 'a tag = 'a Value.tag

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
  ] with bin_io, compare, sexp

  (** Color something with a color  *)
  val color : color tag

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

  (** Universal Heterogeneous Map.  *)
  module Dict : sig
    (** The dictionary can store values of arbitrary type. Only one
        value of a given tag can be stored in the map. For example, if
        you have tag [cconv] (calling convention) then it is
        guaranteed that in map there is zero or one value with this
        tag. *)

    (** type of map *)
    type t with bin_io, compare, sexp

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

    (** [data dict] is a sequence of all dict elements  *)
    val data : t -> Value.t seq
  end



  (** {{!Vector}Resizable array}  *)
  type 'a vector

  (** Resizable Array.

      Resizable arrays with a logarithmic push_back in the style of
      C++. A user need to provide a default value (c.f.,
      DefaultConstructible requirement in C++ version). *)
  module Vector : sig
    (** a type of vector holding elements of type ['a]  *)
    type 'a t = 'a vector with bin_io, compare, sexp

    (** [create ?capacity default] creates an empty vector with a given
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

    (** implements common accessors for the array, like [find], [fold],
        [iter], etc  *)
    include Container.S1 with type 'a t := 'a t
  end

  type bil   = Bil.t       with bin_io, compare, sexp
  type binop = Bil.binop   with bin_io, compare, sexp
  type cast  = Bil.cast    with bin_io, compare, sexp
  type exp   = Exp.t       with bin_io, compare, sexp
  type stmt  = Stmt.t      with bin_io, compare, sexp
  type unop  = Bil.unop    with bin_io, compare, sexp
  type value = Value.t     with bin_io, compare, sexp
  type dict  = Dict.t      with bin_io, compare, sexp

  (** BAP IR.

      Program is a tree of terms.

  *)
  type 'a term with bin_io, compare, sexp

  type program with bin_io, compare, sexp
  type sub with bin_io, compare, sexp
  type arg with bin_io, compare, sexp
  type blk with bin_io, compare, sexp
  type phi with bin_io, compare, sexp
  type def with bin_io, compare, sexp
  type jmp with bin_io, compare, sexp

  type tid with bin_io, compare, sexp
  type call with bin_io, compare, sexp

  (** target of control transfer  *)
  type label =
    | Direct of tid             (** direct jump  *)
    | Indirect of exp           (** indirect jump  *)
  with bin_io, compare, sexp

  (** control transfer variants  *)
  type jmp_kind =
    | Call of call              (** call to subroutine          *)
    | Goto of label             (** jump inside subroutine      *)
    | Ret  of label             (** return from call to label   *)
    | Int  of int * tid         (** interrupt and return to tid *)
  with bin_io, compare, sexp

  (** argument intention  *)
  type intent =
    | In                        (** input argument  *)
    | Out                       (** output argument *)
    | Both                      (** input/output    *)
  with bin_io, compare, sexp

  type ('a,'b) cls

  (** {4 Term type classes}  *)

  val sub_t : (program, sub) cls (** sub  *)
  val arg_t : (sub, arg) cls     (** arg  *)
  val blk_t : (sub, blk) cls     (** blk  *)
  val phi_t : (blk, phi) cls     (** phi  *)
  val def_t : (blk, def) cls     (** def  *)
  val jmp_t : (blk, jmp) cls     (** jmp  *)

  (** {!Graph} nodes.  *)
  module type Node = sig
    (** Semantics of operations is denoted using mathematical model,
        described in {!Graph} interface.  *)

    type t                      (** node type is opaque  *)
    type graph
    type label
    type edge

    (** [create label] creates a new node, and associates it with a
        given [label].  *)
    val create : label -> t

    (** [label n] returns a value associated with a node [n].  *)
    val label : t -> label

    (** [mem n g] is [true] if [n] is a member of nodes [N] of graph
        [g].  *)
    val mem : t -> graph -> bool

    (** [succs node graph] returns a sequence of successors of a
        [node] in a given [graph] *)
    val succs : t -> graph -> t seq

    (** [preds node graph] returns a sequence of predecessors of a
        [node] in a given [graph] *)
    val preds : t -> graph -> t seq

    (** [inputs node graph] is incomming edges of a [node] in [graph]  *)
    val inputs : t -> graph -> edge seq

    (** [outputs node graph] is outcomming edges of a [node] in [graph]  *)
    val outputs : t -> graph -> edge seq

    (** [degree ?dir n] when [in_or_out] is [`In] then returns
        the amount of incomming edges, otherwise returns the amount of
        outcomming edges. If parameter [dir] is left absent, then
        return the amount of adjacent nodes (i.e., a sum of incomming
        and outcomming edges).  *)
    val degree : ?dir:[`In | `Out] -> t -> graph -> int

    (** [insert n g] returns new graph [g'] that has a set of nodes
        [N] extended with node [n]. If [N] was contained [n], then
        the [g == g']. Use {!update} to change existing nodes.

        Postconditions: {v
          - N(g') = N(g) ∪ {n}.
          v}
    *)
    val insert : t -> graph -> graph

    (** [update n l g] if node [n] is in [N] then return a graph [g]
        in which node [n] is associated with label [l]. If wasn't in
        the set [N] then [g] is returned unchanged.

        Postconditions: {v
          - n ∉ N(g) -> n ∉ N(g').
          - n ∈ N(g) → ν(g')n = l.
          v}
    *)
    val update : t -> label -> graph -> graph

    (** [remove n g] returns graph [g'], with a node [n] removed from
        a set of nodes [N].

        Postconditions: {v
          - E(g) ⊆ E(g')
          - N(g) ⊆ N(g')
          - N(g') = N(g) \ {n}.
          v}
    *)
    val remove : t -> graph -> graph

    (** [has_edge x y g] is true iff (x,y) ∈ E. *)
    val has_edge : t -> t -> graph -> bool

    (** [edge x y g] if graph [g] has an edge between nodes [x] and
        [y] then it is returned.  *)
    val edge : t -> t -> graph -> edge option

    (** node provides common data structures, like Table, Map, Set,
        Hash_set, etc.  *)
    include Opaque with type t := t
  end

  (** Interface that every Graph edge should provide  *)
  module type Edge = sig
    (** Semantics of operations is denoted using mathematical model,
        described in {!Graph} interface.  *)

    type t
    type node
    type graph
    type label

    (** [create x y l] creates an edge connecting nodes [x] and [y]
        labeled with a given label [l] *)
    val create : node -> node -> label -> t

    (** [label e] returns a label of an edge [e]  *)
    val label : t -> label

    (** [src e] returns a source of an edge [e]  *)
    val src : t -> node

    (** [dst e] returns a destination of an edge [e] *)
    val dst : t -> node

    (** [mem e g] is true if [e] ∈ E.  *)
    val mem : t -> graph -> bool

    (** [insert e g] returns a graph [g'] with a set of edges extended
        with edge [e]. If [src e] or [dst e] wasn't in the set of nodes
        [N], then it is extended as well, so that axioms of graph are
        preserved.

        Postconditions: {v
          - E(g') = E(g) ∪ {e}.
          v}
    *)
    val insert : t -> graph -> graph

    (** [update e l g] if edge [e] exists in graph [g] then return a
        new graph [g'] in which edge [e] is associated with label [l].
        Otherwise return [g] unchanged.

        Postcondition: {v
          - E(g) ⊆ E(g')
          - N(g) ⊆ N(g')
          - e ∉ E(g) → e ∉ E(g').
          - e ∈ E(g) → ε(g')e = l.
          v}
    *)
    val update : t -> label -> graph -> graph

    (** [remove e g] returns a graph [g'] that doesn't contain edge
        [e].

        Postconditions: {v
          - E(g') = E(g) \ {e}.
          v}
    *)
    val remove : t -> graph -> graph
    include Opaque with type t := t
  end

  (** Graph signature.  *)
  module type Graph = sig
    (** Graph is mathematical data structure that is used to represent
        relations between elements of a set. Usually, graph is defined
        as an ordered pair of two sets - a set of vertices and a set
        of edges that is a 2-subset of the set of nodes,

        {v G = (V,E). v}

        In Graphlib vertices (called nodes in our parlance) and edges
        are labeled. That means that we can associate data with edges
        and nodes. Thus graph should be considered as an associative
        data structure. And from mathematics perspective graph is
        represented as an ordered 6-tuple, consisting of a set of nodes,
        edges, node labels, edge labels, and two functions that maps
        nodes and edges to their corresponding labels:

        {v G = (N, E, N', E', ν : N -> N', ε : E -> E'), v}

        where set [E] is a subset of [ N × N ].

        With this general framework an unlabeled graph can be
        represented as:

        {v G = (N, E, N, E, ν = λx.x, ε = λx.x) v}

        Another possible representation of an unlabeled graph would be:

        {v G = (N, E, {u}, {v}, ν = λx.u, ε = λx.v). v}

        Implementations are free to choose any suitable representation
        of graph data structure, if it conforms to the graph signature
        and all operations follows the described semantics and
        properties of a graph structure are preserved.

        The axiomatic semantics of operations on a graph is described by
        a set of postconditions. To describe the semantics of an
        operation in terms of input and output arguments, we project
        graphs to its fields with the following notation
        [<field>(<graph>)], e.g., [N(g)] is a set of nodes of graph [g].

        Only the strongest postcondition is specified, e.g., if it
        specified that [νn = l], than it also means that

        [n ∈ N ∧ ∃u((u,v) ∈ E ∨ (v,u) ∈ E) ∧ l ∈ N' ∧ ...]

        In other words the structure [G] of the graph G is an invariant
        that is always preserved.
    *)

    (** type of graph  *)
    type t

    (** type of nodes *)
    type node

    (** type of edges  *)
    type edge


    (** Graph nodes.  *)
    module Node : Node with type graph = t
                        and type t = node
                        and type edge = edge

    (** Graph edges  *)
    module Edge : Edge with type graph = t
                        and type t = edge
                        and type node = node

    (** [empty] is an empty graph  *)
    val empty : t

    (** [nodes g] returns all nodes of graph [g] in an unspecified order  *)
    val nodes : t -> node seq

    (** [edges g] returns all edges of graph [g] in an unspecified order  *)
    val edges : t -> edge seq

    (** [is_directed] is true if graph is a directed graph.  *)
    val is_directed : bool

    (** [number_of_edges g] returns the size of a graph [g].  *)
    val number_of_edges : t -> int

    (** [number_of_nodes g] returns the order of a graph [g]  *)
    val number_of_nodes : t -> int

    (** All graphs provides a common interface for any opaque data structure  *)
    include Opaque with type t := t

    (** All graphs are printable.   *)
    include Printable with type t := t
  end

  (** a type abbreviation for a packed module, implementing graph
      interface.
      Note: this type prenexes only 3 out of 8 type variables, so,
      sometimes it is not enough. *)
  type ('c,'n,'e) graph =
    (module Graph with type t = 'c
                   and type node = 'n
                   and type edge = 'e)

  (** Graph edges classification.
      For explanations see {{!Graphlib.depth_first_search}DFS}.*)
  type edge_kind = [
    | `Tree                     (** edge is a part of a tree  *)
    | `Back                     (** back edge   *)
    | `Cross                    (** cross edge  *)
    | `Forward                  (** forward edge  *)
  ]

  (** a {!Tree} representation.  *)
  type 'a tree

  (** a type representing {!Frontier}s  *)
  type 'a frontier

  (** a {{!Partition}result} of partitioning algorithms  *)
  type 'a partition

  (** a partition {{!Group}Cell} *)
  type 'a group

  (** walk without a repetition of edges and inner nodes *)
  type 'a path

  (** runtime witness of the {{!Equiv}equivalence class} *)
  type equiv

  (** Tree is a particular subtype of a graph for which
      each node has only one predecessor, and there is only
      one path between tree root and any other node.
      Here is an example of a tree:
      {v
                                 (A)
                                  |
                          +-------+-------+
                          |       |       |
                         (B)     (C)     (D)
                                  |
                          +-------+-------+
                          |       |       |
                         (E)     (F)     (G)
                                  |
                                 (H)
        v}
  *)
  module Tree : sig
    type 'a t = 'a tree

    (** [children tree x] returns all immediate successors of node
        [x]. For example, children of node [A] is a sequence of
        [B,C,D]. But node [B] doesn't have any children at all.*)
    val children : 'a t -> 'a -> 'a seq

    (** [parent tree n] returns an immediate parent of a given node.
        Returns [None] only if [n] is a tree root. For example, parent
        of [F] is [C]. And [A] doesn't have a parent.*)
    val parent : 'a t -> 'a -> 'a option

    (** [ancestors tree n] returns a sequence of all ancestors of node
        [n]. An ancestor of a node is either a parent or an ancestor of
        the parent. For example, ancestors of [G] are [C] and [D]. The
        root node is the only one, that has an empty set of
        ancestors. *)
    val ancestors : 'a t -> 'a -> 'a seq

    (** [descendants tree n] returns a set of all descendants of a
        given node [n]. Descendant is either a child or a descendant
        of a child. For example, all nodes in the example [tree]
        (except the roots itself) are descendants of the root [A].
        The descendants of [C] are [E,F,G,H].    *)
    val descendants : 'a t -> 'a -> 'a seq

    (** [is_child_of tree parent child] returns [true] if child is one
        of [children tree root] *)
    val is_child_of : 'a t -> parent:'a -> 'a -> bool

    (** [is_ancestor_of tree child x] returns true, if [x] is one of
        the ancestors of a [child] node.  *)
    val is_ancestor_of : 'a t -> child:'a -> 'a -> bool

    (** [is_descendant_of ~parent tree x] is [true] for all [x] that
        are descendants of a [parent] node.  *)
    val is_descendant_of : 'a t -> parent:'a -> 'a -> bool

    (** [to_sequence tree] enumerates nodes of a [tree] in an
        unspecified order.  *)
    val to_sequence : 'a t -> 'a seq

    (** [pp pp_elt] creates a pretty-printer for a node, based on
        element's pretty-printer [pp_elt]. The tree is printed in a dot
        format, for the ease of visualization. Example:
        {[let pp_int_tree = Tree.pp Int.pp]}
        Note: For all instatiations of [Graph] interface in the
        [Graphlib] library printable versions of [tree], [partition],
        [group] etc are provided. For example, for [Graphlib.Int.Bool]
        graph the printable version of a [tree] is available under
        [Graphlib.Int.Tree]. All instantiated pretty-printers are
        automatically installed once the library is loaded into the
        toplevel. *)
    val pp : 'a printer -> 'a t printer
  end

  (** Frontier maps each node into a possibly empty set of nodes.
      This is used for representing dominance and post-dominance
      frontiers.  *)
  module Frontier : sig

    type 'a t = 'a frontier

    (** [enum f x] enumerates frontier of [x]  *)
    val enum : 'a t -> 'a -> 'a seq

    (** [mem f x y] is true if [y] is in a frontier of [x]  *)
    val mem : 'a t -> 'a -> 'a -> bool

    (** [to_sequence frontier] enumerates all elements of a [frontier] *)
    val to_sequence : 'a t -> 'a seq

    (** [pp pp_elt] instantiates a pretty-printer for a given
        element. See {!Tree.pp} for more information.  *)
    val pp : 'a printer -> 'a t printer
  end

  (** Path between two nodes.  *)
  module Path : sig
    (** path is a walk without repetitions  *)

    (** representation type  *)
    type 'e t = 'e path

    (** [start p] the starting edge of a path [p] *)
    val start : 'e t -> 'e

    (** [finish p] the last edge of a path [p] *)
    val finish : 'e t -> 'e

    (** [edges p] a sequence of edges from start to finish  *)
    val edges : 'e t -> 'e seq

    (** [edges_rev p] a reversed sequence from finish to start  *)
    val edges_rev : 'e t -> 'e seq

    (** [weight p] total weight of a path *)
    val weight : 'e t -> int

    (** amount of edges in a path *)
    val length : 'e t -> int

    (** [pp pp_elt] constructs a pretty based on element printer
        [pp_elt] *)
    val pp : 'a printer -> 'a t printer
  end

  (** Result of a set partitioning.

      A partition of a set [S] is a set of non-empty subset of set [S],
      such that each element in a set of [S] is included in one and only
      one of the subsets and a union of the subsets forms a set [S].

      All nodes belonging to the same subset (called [group] in our
      parlance) represents the equivalence class. The equivalence side
      can be represented by a particular ordinal number or
      representative, that can be thought about as an ordinary
      number. See {!Equiv} for the representation of this ordinal
      numbers. A particular element of an equivalence class plays a
      role of «representative element». Depending on the nature of
      partitioning, this role can have different semantics.

      This data structure is used to represent results of partioning of
      a graph into groups of nodes, for example, to strongly connected
      components.*)
  module Partition : sig

    type 'a t = 'a partition

    (** [groups p] returns all partition cells of a partitioning [p] *)
    val groups : 'a t -> 'a group seq

    (** [group p x] returns a [group] of an element [x]. Note, this
        function is not total since the set of all values of type ['a] is
        usually larger than the underlying set that was partitioned.  *)
    val group : 'a t -> 'a -> 'a group option

    (** [equiv p x y] is true of [x] and [y] belongs to the same
        equivalence class (i.e., to the same group).  *)
    val equiv : 'a t -> 'a -> 'a -> bool

    (** [number_of_groups p] returns the amount of groups in a given
        partitioning [p]. *)
    val number_of_groups : 'a t -> int

    (** [of_equiv p n] rebuilds a group from an equivalence class
        ordinal number. *)
    val of_equiv : 'a t -> equiv -> 'a group option
  end

  (** Group is a non-empty set that is a result of partitioning of an
      underlying set [S] into a set of non-intersecting and non-empty
      subsets that cover set [S]. See {!Partition} for more
      information.  *)
  module Group : sig

    type 'a t = 'a group

    (** [enum group] enumerates all elements of a group, including the
        designated one.  *)
    val enum : 'a group -> 'a seq

    (** [mem group x] checks membership of [x] in a given group.  *)
    val mem  : 'a group -> 'a -> bool

    (** [top group] returns the top element of a group also known as a
        representative element. The function is total since groups is
        guaranteed to be non-empty.    *)
    val top  : 'a group -> 'a

    (** [to_equiv g] returns the ordinal number representing the
        particular group [g] *)
    val to_equiv : 'a group -> equiv
  end

  (** Ordinal for representing equivalence. Useful, for indexing
      elements based on their equivalence. *)
  module Equiv : sig
    type t
    val to_int : t -> int
    include Regular with type t := t
  end

  (** {5 Auxiliary graph data structures}  *)

  (** A type of modules for filtering graphs.
      See {!Graphlib.filtered} or {!Graphlib.Filtered}  *)
  module type Predicate = sig
    type edge
    type node
    val edge : edge -> bool
    val node : node -> bool
  end

  (** [Isomorphism] is a bijection between type [s] and [t].
      Usefull for creating graph views and mapping graphs.
      See {!Graphlib.view} and {!Graphlib.Mapper}.
  *)
  module type Isomorphism = sig
    type s
    type t
    val forward  : s -> t
    val backward : t -> s
  end

  class type ['n,'e,'s] dfs_visitor = object
    method start_tree :       'n -> 's -> 's
    method enter_node : int -> 'n -> 's -> 's
    method leave_node : int -> 'n -> 's -> 's
    method enter_edge : edge_kind -> 'e -> 's -> 's
    method leave_edge : edge_kind -> 'e -> 's -> 's
  end


  (** {4 Visual attributes for graphvizualization.}
      Consult OCamlGraph library for more information.
  *)

  type node_attr  = Graph.Graphviz.DotAttributes.vertex
  type edge_attr  = Graph.Graphviz.DotAttributes.edge
  type graph_attr = Graph.Graphviz.DotAttributes.graph

  type ('n,'a) labeled = {
    node : 'n;
    node_label : 'a;
  }


  (** Generic Graph Library  *)
  module Graphlib : sig

    (* we need this restatement, since first class modules in
       4.01 were nominally typed.  *)
    module type Graph = Graph

    (** [create (module G) ~nodes ~edges ()] creates a graph using
        implementation provided by [module G].
        Example:
        {[
          module G = Graphlib.String.Bool;;
          let g = Graphlib.create (module G) ~edges:[
              "entry", "loop", true;
              "loop", "exit", false;
              "loop", "loop", true] ()
        ]} *)
    val create :
      (module Graph with type t = 'c
                     and type Node.label = 'a
                     and type Edge.label = 'b) ->
      ?nodes:'a list ->
      ?edges:('a * 'a * 'b) list -> unit -> 'c

    (** [to_dot (module G) ~filename:"graph.dot" g] dumps graph [g]
        using [dot] format. This is a customizable version of printing
        function. For most cases it will be enough to use [G.pp] or
        [G.to_string] function. Use this function, if you really need
        to customize your output.

        @param graph_attrs a list of global graph attributes;
        @param node_attrs  a list of node specific attributes;
        @param edge_attrs  a list of edge specific attributes;
        @param string_of_node used to print nodes;
        @param string_of_edge used to print edges;
        @param channel where to output the graph;
        @param formatter where to output the graph;
        @param filename where to output the graph;

        Note: if no output parameter is provided, the graph will not be
        outputted. More than one output targets is OK. For example,
        [to_dot (module G) ~filename:"graph.dot" ~channel:stdout g] will
        output graph [g] into both file named ["graph.dot"] and
        standard output.

        Note: if [string_of_node] function is not provided, then graph
        nodes will be labeled with the reverse post order number.  *)
    val to_dot :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?graph_attrs:('c -> graph_attr list) ->
      ?node_attrs:('n -> node_attr list) ->
      ?edge_attrs:('e -> edge_attr list) ->
      ?string_of_node:('n -> string) ->
      ?string_of_edge:('e -> string) ->
      ?channel:out_channel ->
      ?formatter:Format.formatter ->
      ?filename:string -> 'c -> unit

    (** [depth_first_search (module G) ~init g].  It is the most
        important algorithm of the Graphlib. It builds a forest of
        spanning trees of a graph, classifies graph edges and numbers
        nodes. It is a Swiss-army knife, that is very useful in
        implementing many other algorithms. You can think of this
        function as [fold] on steroids. But unlike [fold], that
        accepts only one function, the [depth_first_search] accepts 5
        different functions, that will be called on different
        situations, allowing you to «fill in the blanks» of your
        algorithm.

        Although [depth_first_search] doesn't allow you to drive the
        walk itself, there're still ways to do this, using {!filtered}
        function. That allows you to hide nodes or edges from the
        walker, thus effectively erasing them from a graph, without
        even touching it.

        @param rev if true, then the graph [g] is traversed in a
        reverse direction. This is essentially the same, as reversing
        the graph, but make sure, that you've adjusted the start
        node.

        @param start if specified, then the traverse will be started
        from the node that is equal to node [start]. Otherwise the
        traverse is started from the first node of a graph as returned
        by [G.nodes], i.e., usually it is an arbitrary node.

        @param start_tree [node] [state] is called on each new spanning
        tree started by the algorithm. If all nodes are reachable from
        the start node, then this function will be called only
        once. If all nodes of a graph are connected, then this
        function, will be called only once.

        @param enter_node [pre] [node] [state] is called when a node
        is first discovered by the traversal. The number is a preorder
        number, also known as depth-first number or [dfnum]. All nodes
        are entered in a pre-order.

        @param leave_node [rpost] [node] [state] is called when all
        successors of a [node] are left (finished). The provided
        number is a reverse post order number, that also defines a
        topological sorting order of a graph. All nodes, are left in
        a post order.

        @param enter_edge [kind] [edge] [state] is called when and
        [edge] is first discovered. Edge kinds are described below.
        The destination of the edge may not be discovered (i.e.,
        entered) yet. But the source is already entered (but not
        finished).

        @param leave_edge [kind] [edge] [state] is called when the
        edge destination is at least started.

        {2 Edges classification}

        An edge in a spanning tree, produced by a depth first walk,
        can belong to one of the following category (kind):
        - Tree edges constitutes a spanning tree [T] of a graph;
        - Forward edges go from an ancestor to a descendants in
          a tree [T];
        - Back edges go from descendants to ancestors in [T],
          including node itself (they are also known as cycle
          edges).
        - Cross edges - all other edges, i.e., such edges for
          which doesn't go from ancestors to descendants or vice
          verse. They are possible since, tree defines only partial
          ordering.

        With respect to a pre-order and reverse post-ordering
        numbering the source [x] and a destination [y] of an edge with
        a given [kind] satisfy to the following inequalities:

        {v
            +---------+-----------------+---------------------+
            | Tree    | pre[x] < pre[y] | rpost[x] < rpost[y] |
            | Forward | pre[x] < pre[y] | rpost[x] < rpost[y] |
            | Back    | pre[x] ≥ pre[y] | rpost[x] ≥ rpost[y] |
            | Cross   | pre[x] > pre[y] | rpost[x] < rpost[y] |
            +---------+-----------------+---------------------+
          v}

        Note: since there can be more than one valid order of
        traversal of the same graph, (and thus more than one valid
        spanning tree), depending on a traversal the same edges can be
        classified differently. With the only exception, that a back
        edge will be always a back edge, disregarding the particular
        order.

        {3 Complexity}
        The algorithm is linear in time and space (including the stack
        space). In fact, for small graphs it uses stack, but for large
        graphs dynamically switches to a heap storage. *)
    val depth_first_search :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool ->
      ?start:'n ->
      ?start_tree:('n -> 's -> 's) ->
      ?enter_node:(int -> 'n -> 's -> 's) ->
      ?leave_node:(int -> 'n -> 's -> 's) ->
      ?enter_edge:(edge_kind -> 'e -> 's -> 's) ->
      ?leave_edge:(edge_kind -> 'e -> 's -> 's) ->
      'c -> init:'s -> 's

    (** [depth_first_visit (module G) ~init visitor g] allows to
        specify visiting functions using object. That opens space for
        re-usability and using open recursion.  *)
    val depth_first_visit :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> ?start:'n -> 'c -> init:'s -> ('n,'e,'s) dfs_visitor -> 's

    (** base class with all methods defaults to nothing.  *)
    class ['n,'e,'s] dfs_identity_visitor : ['n,'e,'s] dfs_visitor

    (** returns a sequence of nodes in reverse post order.  *)
    val reverse_postorder_traverse :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> ?start:'n -> 'c -> 'n seq

    (** returns a sequence of nodes in post order  *)
    val postorder_traverse :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> ?start:'n -> 'c -> 'n seq

    (** [dominators (module G) g entry] builds a dominators tree for a
        given graph.

        Definition: a {b walk} is a sequence of alternating nodes and
        edges, where each edge's endpoints are the preceding and
        following nodes in the sequence.

        Definition: a node [v] is {b reachable} if there exists a walk
        starting from [entry] and ending with [v].

        Definition: node [u] {b dominates} [v] if [u = v] or if all walks
        from [entry] to [v] contains [u].

        Definition: node [u] {b strictly dominates} [v] if it dominates
        [v] and [u <> v].

        Definition: node [u] {b immediately dominates} [v] if it
        strictly dominates [v] and there is no other node that
        strictly dominates [v] and is dominated by [u].

        Algorithm computes a dominator tree [t] that has the following
        properties:
        + Sets of graph nodes and tree nodes are equal;
        + if node [u] is a parent of node [v], then node [u]
           immediately dominates node [v];
        + if node [u] is an ancestors of node [v], then node [u]
           strictly dominates node [v];
        + if node [v] is a child of node [u], then node [u]
           immediately dominates node [v];
        + if node [v] is a descendant of node [u], then node [u]
           strictly dominates node [v].

        If every node of graph [g] is reachable from a provided
        [entry] node, then properties (2) - (5) are reversible, i.e.,
        an [if] statement can be read as [iff], and the tree is
        unique.


        {b Lemma}: Everything dominates unreachable block.

        {b Proof}: (by contradiction) suppose there exists a node [u] that
        doesn't dominate unreachable block [v]. That means, that there
        exists a path from [entry] to [v] that doesn't contain
        [u]. But that means, at least, that [v] is reachable. This  is
        a contradiction with the original statement that [v] is
        unreachable. {b Qed.}

        If some nodes of graph [g] are unreachable from the provided
        [entry] node, then they are dominated by all other nodes of a
        graph. It means that the provided system is under constrained
        and has more then one solution (i.e., there exists more than
        one tree, that satisfies properties (1) - (5). In a current
        implementation each unreachable node is immediately dominated
        by the [entry], if the [entry] is in graph.

        To get a post-dominator tree, reverse the graph by passing
        [true] to [rev] and pass exit node as a starting node.

        Note: although it is not imposed by the algotihm, but it is a
        good idea to have an entry node, that doesn't have any
        predecessors. Usually, this is what is silently assumed in
        many program analysis textbooks, but is not true in general
        for control-flow graphs that are reconstructed from binaries *)
    val dominators :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> 'c -> 'n -> 'n tree

    (** [dom_frontier (module G) g dom_tree] calculates dominance
        frontiers for all nodes in a graph [g].     *)
    val dom_frontier :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> 'c -> 'n tree -> 'n frontier

    (** [strong_components (module G) g] partition graph into strongly
        connected components. The top of each component is a root
        node, i.e., a node that has the least pre-order number.*)
    val strong_components :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      'c -> 'n partition

    (** [shortest_path (module G) ?weight ?rev g u v]
        Find a shortest path from node [u] to node [v].

        @param weight defines a weight of each edge. It defaults to 1.
        @param rev allows to reverse graph.    *)
    val shortest_path :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?weight:('e -> int) -> ?rev:bool -> 'c -> 'n -> 'n -> 'e path option

    (** [is_reachable (module G) ?rev g u v] is true if node [v] is
        reachable from node [u] in graph [g]. If rev is true, then it
        will solve the same problem but on a reversed graph.  *)
    val is_reachable :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> 'c -> 'n -> 'n -> bool

    (** [fold_reachable (module G) ?rev ~init ~f g n] applies function
        [f] to all nodes reachable from node [g] in graph [g]. If
        [rev] is true, then the graph is reversed.

        For example, the following will build a set of reachable nodes:
        [fold_reachable (module G) ~init:G.Node.Set.empty ~f:Set.add]
    *)
    val fold_reachable :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?rev:bool -> init:'a -> f:('a -> 'n -> 'a) -> 'c -> 'n -> 'a

    (** [compare (module G1) (module G2) g1 g2] compares two graphs,
        with different implementation but the same node type.  *)
    val compare :
      (module Graph with type t = 'a
                     and type node = 'n) ->
      (module Graph with type t = 'b
                     and type node = 'n) ->
      'a -> 'b -> int

    (** [let module G' = filtered (module G) ?skip_node ?skip_edge ()]
        creates a new module [G'] that can be used at any place
        instead of [G], but that will hide nodes and edges, for which
        functions [skip_node] and [skip_edge] return true.

        Example:
        {[
          let killed_edges = G.Edge.Hash_set.create () in
          let module G = Graphlib.filtered (module G)
              ~skip_edge:(Hash_set.mem killed_edges) () in
          let rec loop g () =
            (* use (module G) as normal *)
            Hash_set.add killed_edges some_edge;
            (* all edges added to [killed_edges] will no be visible *)
        ]} *)
    val filtered :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e) ->
      ?skip_node:('n -> bool) ->
      ?skip_edge:('e -> bool) -> unit ->
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e)

    (** [view (module G) ~node ~edge ~node_label ~edge_label]
        creates a proxy module, that will transform back and
        forward elements of graph, using corresponding functions.  *)
    val view :
      (module Graph with type t = 'c
                     and type node = 'n
                     and type edge = 'e
                     and type Node.label = 'a
                     and type Edge.label = 'b) ->
      node:(('n -> 'f) * ('f -> 'n)) ->
      edge:(('e -> 'd) * ('d -> 'e)) ->
      node_label:(('a -> 'p) * ('p -> 'a)) ->
      edge_label:(('b -> 'r) * ('r -> 'b)) ->
      (module Graph with type t = 'c
                     and type node = 'f
                     and type edge = 'd
                     and type Node.label = 'p
                     and type Edge.label = 'r)


    (** [To_ocamlgraph(G)] returns a module that implements
        OCamlGraph interface for a persistent graph.  *)
    module To_ocamlgraph(G : Graph) :
      Graph.Sig.P with type t = G.t
                   and type V.t = G.node
                   and type E.t = G.edge
                   and type V.label = G.Node.label
                   and type E.label = G.Edge.label

    (** [Of_ocamlgraph(O)] creates an adapter module, that implements
        [Graphlib] interface on top of the module implementing
        [OCamlGraph] interface.*)
    module Of_ocamlgraph(G : Graph.Sig.P) :
      Graph with type t = G.t
             and type node = G.V.t
             and type edge = G.E.t
             and type Node.label = G.V.label
             and type Edge.label = G.E.label

    (** functorized version of a {!filter} function.  *)
    module Filtered
        (G : Graph)
        (P : Predicate with type node = G.node
                        and type edge = G.edge) :
      Graph with type t = G.t
             and type node = G.node
             and type edge = G.edge
             and module Node = G.Node
             and module Edge = G.Edge

    (** functorized version of {!Graphlib.view} function.  *)
    module Mapper
        (G  : Graph)
        (N  : Isomorphism with type s = G.node)
        (E  : Isomorphism with type s = G.edge)
        (NL : Isomorphism with type s = G.Node.label)
        (EL : Isomorphism with type s = G.Edge.label) :
      Graph with type t = G.t
             and type node = N.t
             and type edge = E.t
             and type Node.label = NL.t
             and type Edge.label = EL.t

    (** [Make(Node)(Edge)] creates a module that implements [Graph]
        interface and has unlabeled nodes of type [Node.t] and edges
        labeled with [Edge.t] *)
    module Make(Node : Opaque)(Edge : Opaque) : Graph
      with type node = Node.t
       and type Node.label = Node.t
       and type Edge.label = Edge.t


    module Labeled(Node : Opaque)(NL : T)(EL : T) : Graph
      with type node = (Node.t, NL.t) labeled
       and type Node.label = (Node.t, NL.t) labeled
       and type Edge.label = EL.t


    (** a common interface for a regular graph.
        Graphlib comes with a big set of predefined (i.e.,
        instantiated graphs. Each regular graph is actually a
        family of graphs indexed by a type of an edge label.

        For example, [Graphlib.Int.Bool] is a graph with [int]s as
        nodes, and with edges labeled with values of type [bool].

        Each regular graph also provides a printable interface for
        each auxiliary data structure.  *)
    module type Graphs = sig
      type node

      module Bool : Graph with type node = node
                           and type Node.label = node
                           and type Edge.label = bool
      module Unit : Graph with type node = node
                           and type Node.label = node
                           and type Edge.label = unit
      module Value : Graph with type node = node
                            and type Node.label = node
                            and type Edge.label = value
      module Word : Graph with type node = node
                           and type Node.label = node
                           and type Edge.label = word
      module Int : Graph with type node = node
                          and type Node.label = node
                          and type Edge.label = int
      module String : Graph with type node = node
                             and type Node.label = node
                             and type Edge.label = string
      module Exp : Graph with type node = node
                          and type Node.label = node
                          and type Edge.label = exp

      module Stmt : Graph with type node = node
                           and type Node.label = node
                           and type Edge.label = stmt

      module Var : Graph with type node = node
                          and type Node.label = node
                          and type Edge.label = var

      module Tid : Graph with type node = node
                          and type Node.label = node
                          and type Edge.label = tid

      module Type : Graph with type node = node
                           and type Node.label = node
                           and type Edge.label = typ

      module Tree : Printable with type t = node tree
      module Frontier : Printable with type t = node frontier
      module Path : Printable with type t = node path
      module Partition : Printable with type t = node partition
      module Group : Printable with type t = node group
    end

    (** {3 Pre-instantiated graphs} *)

    module Int    : Graphs with type node = int
    module Word   : Graphs with type node = word
    module Value  : Graphs with type node = value
    module String : Graphs with type node = string
    module Var    : Graphs with type node = var
    module Exp    : Graphs with type node = exp
    module Stmt   : Graphs with type node = stmt
    module Tid    : Graphs with type node = tid

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
        it is recommended to use {!Term} or [Builder} interfaces to
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
        given destination. If there is no position in a slot,
        represented by the given label, then it will be
        inserted. Dummy jumps will be prepended before the inserted
        jump, if needed.

        When an edge is inserted into the graph, then source and
        destination nodes are inserted or updated (depending on whether
        they were already present in the graph). As a result, the
        graph must contain at least nodes, incident to the edge, and
        the edge itself.

        {2 Updating edge}

        Updating an edge is basically the same, as updating incident
        nodes, given that the edge exists in the graph.


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

      (** {4 Printable interface for auxiliary data structures}  *)
      module Tree : Printable with type t = node tree
      module Frontier : Printable with type t = node frontier
      module Path : Printable with type t = node path
      module Partition : Printable with type t = node partition
      module Group : Printable with type t = node group
    end

  end

  (** an image loaded into memory  *)
  type image

  (** opaque memory  *)
  type mem with sexp_of

  (** a table from memory to ['a]  *)
  type 'a table with sexp_of

  (** interval trees from memory regions to ['a] *)
  type 'a memmap with sexp_of

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
    type t = mem with sexp_of

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
    type 'a t = 'a table with sexp_of
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
  end

  (** A locations of a chunk of memory  *)
  module Location : sig
    type t = {
      addr : addr;
      len  : int;
    } with bin_io, compare, fields, sexp
  end

  (** memory location  *)
  type location = Location.t with bin_io, compare, sexp

  (** A backend interface.

      This interface must be implemented by a backend plugin, and
      registered with [Image.register] function in order to be
      accessible for loading images.
  *)
  module Backend : sig

    (** memory access permissions  *)
    type perm = R | W | X | Or of perm * perm
    with bin_io, compare, sexp

    (** A named contiguous part of file with permissions.
        Also, known as segment in ELF.    *)
    module Segment : sig
      type t = {
        name: string;
        perm: perm;         (** segment's permissions  *)
        off: int;
        location : location;
      } with bin_io, compare, fields, sexp
    end

    (** Symbol definition, that can span several non-contiguous parts of
        memory *)
    module Symbol : sig
      type t = {
        name : string;
        is_function : bool;
        is_debug : bool;
        locations : location * location list;
      } with bin_io, compare, fields, sexp
    end

    (** Just a named region of memory.  *)
    module Section : sig
      type t = {
        name : string;
        location : location;
      } with bin_io, compare, fields, sexp
    end

    (** A Img from a backend perspective.  *)
    module Img : sig
      type t = {
        arch     : arch;
        entry    : addr;
        segments : Segment.t * Segment.t list;
        symbols  : Symbol.t list;
        sections  : Section.t list;
      } with bin_io, compare, fields, sexp
    end

    (** the actual interface to be implemented  *)
    type t = Bigstring.t -> Img.t option
  end


  (** Binary Image.  *)
  module Image : sig
    (** {2 Type definitions}  *)

    type t = image with sexp_of            (** image   *)

    (** segment *)
    type segment with bin_io, compare, sexp
    (** symbol  *)
    type symbol with bin_io, compare, sexp

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
    type 'a t = 'a memmap with sexp_of

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
  end

  (** value of type [disasm] is a result of the disassembling of a
      memory region. To create values of this type use [disassemble]
      function *)
  type disasm

  (** values of type [insn] represents machine instructions decoded
      from the given piece of memory *)
  type insn with bin_io, compare, sexp_of

  (** [block] is a region of memory that is believed to be a basic block
      of control flow graph to the best of our knowledge. *)
  type block with compare, sexp_of

  (** [disassemble ?roots arch mem] disassemble provided memory region
      [mem] using best available algorithm and backend for the specified
      [arch]. Roots, if provided, should point to memory regions, that
      are believed to contain code. At best, this should be a list of
      function starts. If no roots are provided, then the starting
      address of the provided memory [mem] will be used as a root.

      The returned value will contain all memory reachable from the
      given set of roots, at our best knowledge. *)
  val disassemble : ?roots:addr list -> arch -> mem -> disasm

  (** [disassemble_image image] disassemble given image.
      Will take executable segments of the image and disassemble it,
      applying [disassemble] function. If no roots are specified, then
      symbol table will be used as a source of roots. If file doesn't
      contain one, then entry point will be used.
  *)
  val disassemble_image : ?roots:addr list -> image -> disasm

  (** [disassemble_file ?roots path] takes a path to a binary and
      disassembles it  *)
  val disassemble_file : ?roots:addr list -> string -> disasm Or_error.t

  (** [disassemble_file ?roots path] takes a path to a binary and
      disassembles it  *)
  val disassemble_file_exn : ?roots:addr list -> string -> disasm

  (** [linear_sweep arch mem] will perform a linear sweep disassembly on
      the specified memory [mem] *)
  val linear_sweep : arch -> mem -> (mem * insn option) list Or_error.t

  (** [linear_sweep_exn] same as [linear_sweep], but raises an
      exception, instead of returning [Or_error] monad  *)
  val linear_sweep_exn : arch -> mem -> (mem * insn option) list


  (** Disassembled program.

      This module provides an interface for values of type [disasm]. *)
  module Disasm : sig
    type t = disasm

    (** returns all instructions that was successfully decoded in an
        ascending order of their addresses. Each instruction is
        accompanied with its block of memory. *)
    val insns : t -> (mem * insn) seq

    (** returns a mapping from memory regions to basic blocks. One may
        think this as a whole program CFG that is constructed on the
        fly.  *)
    val blocks : t -> block table

    (** performs lookup for an instruction that occupies exactly
        the given piece of memory. If you need to find all instructions
        that lies in a given region of memory, use [insns_of_mem] or
        [insns_of_block] functions.
    *)
    val insn_at_mem : t -> mem -> insn option

    (** [insns_of_mem] returns all instructions that occupies memory
        regions that have intersections with [mem].  *)
    val insns_at_mem : t -> mem -> (mem * insn) seq

    (** returns a sequence of memory regions occupied by the given
        instruction.  *)
    val mems_of_insn : t -> insn -> mem seq

    (** [insn_at_addr t addr] finds instruction to which the [addr]
        belongs. In other words if instruction at given address is
        found it doesn't mean, that it starts at this address.
        Consider comparison with [min_addr] if you need to match
        starting address only.  *)
    val insn_at_addr : t -> addr -> (mem * insn) option

    (** returns a blocks of memory that was visited during the
        disassembly. The regions are merged with [Memory.merge] if
        possible. So it returns the least possible amount of contiguous
        memory regions *)
    val span : t -> mem seq

    type error = [
      | `Failed of Error.t
      | `Failed_to_disasm of mem
      | `Failed_to_lift of mem * insn * Error.t
    ] with sexp_of

    module Error : Printable with type t := error

    (** returns a list of all errors and warnings that occurred during
        the disassembling *)
    val errors : t -> (mem * error) seq

    (** {2 Tags}  *)

    (** start of basic block  *)
    val block : addr tag

    (** machine instruction  *)
    val insn : insn tag

    (** address of instruction  *)
    val insn_addr : addr tag


  end

  (** Kinds of instructions  *)
  module Kind : sig
    type branch = [
      | `Conditional_branch
      | `Unconditional_branch
      | `Indirect_branch
    ] with bin_io, compare, enumerate, sexp

    type affecting_control = [
      | branch
      | `Return
      | `Call
      | `Barrier
      | `Terminator
      | `May_affect_control_flow
    ] with bin_io, compare, enumerate, sexp

    type having_side_effect = [
      | `May_load
      | `May_store
    ] with bin_io, compare, enumerate, sexp

    type t = [
      | affecting_control
      | having_side_effect
    ] with bin_io, compare, enumerate, sexp
  end

  (** abstract and opaque register  *)
  type reg with bin_io, compare, sexp

  (** opaque immediate value  *)
  type imm with bin_io, compare, sexp

  (** floating point value  *)
  type fmm with bin_io, compare, sexp

  (** kind of instruction  *)
  type kind = Kind.t with bin_io, compare, sexp


  (** Register.  *)
  module Reg : sig
    type t = reg

    (** unique number representig a register  *)
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
    with bin_io, compare, sexp
    (** Normalized comparison.  *)
    module Normalized : sig
      val compare : t -> t -> int
      val hash : t -> int
      val compare_ops : t array -> t array -> int
    end

    val pp_adt : Format.formatter -> t -> unit
    include Regular with type t := t
  end

  type op = Op.t with bin_io, compare, sexp_of

  type symtab

  (** Reconstructed symbol table.  *)
  module Symtab : sig
    (** This data structure holds information about functions that
        were found in the executable.*)


    (** symbol table  *)
    type t = symtab

    (** symbol table entry  *)
    type fn

    (** [reconstruct ?name ?roots cfg] reconstructs symbols using a
        whole program [cfg]. [name] is a naming function, that should
        return [Some name] for the symbols starting at provided
        address. If [name] returns [None], then the default naming
        scheme will be used for that symbol. [roots] is the initial
        set of function starts. This set will be extended with all
        targets of calls, found in the [cfg]. Note: it is not required
        for [roots] to be actually a set - duplicates is ok.  *)
    val reconstruct :
      ?name:(addr -> string option) ->
      ?roots:addr list -> block table -> t

    (** [add_symbol table name entry blocks] extends [table] with a
        new symbol with a given [name], [entry] block and body
        [blocks].  *)
    val add_symbol : t -> string -> block -> block seq -> t

    (** [remove table fn] removes symbol [fn] from [table]  *)
    val remove : t -> fn -> t

    (** [find_by_name symbols name] finds a symbols with a given name  *)
    val find_by_name  : t -> string -> fn option

    (** [find_by_start symbols addr] finds a symbol that starts from
        a given address *)
    val find_by_start : t -> addr -> fn option

    (** [fns_of_addr symbols addr] return a list of functions that
        owns [addr]   *)
    val fns_of_addr : t -> addr -> fn list

    (** [fns_of_mem syms mem] returns a list of functions that
        dominates over provided memory region [mem] *)
    val fns_of_mem : t -> mem -> fn list

    (** [create_bound syms fn] creates a predicate that returns [true]
        for all addresses that belongs to function [f]. The returned
        predicated is in a staged form, to prevent abuse. To unstage,
        use function [unstage], e.g.,
        [let bound = unstage (Symtab.create_bound syms fn)]
    *)
    val create_bound : t -> fn -> (addr -> bool) Staged.t

    (** [to_sequence symtab] returns a sequence of functions  *)
    val to_sequence : t -> fn seq

    (** [name_of_fn fn] returns symbol name  *)
    val name_of_fn : fn -> string

    (** [entry_of_fn fn] returns an entry block of a given function [fn]  *)
    val entry_of_fn : fn -> block

    (** [memory_of_fn syms fn] returns memory covered by function [fn]   *)
    val memory_of_fn : t -> fn -> unit memmap
  end

  (** ABI interface.
      Each ABI object must implement this interface. *)
  class type abi = object
    (** unique identifier of the ABI.
        Used to communicate between to ABI's.

        The order of id parts should be from more specific, to less
        specific, i.e. in reverse order (so that deriving classes can
        easily append their own parts). The architecture shouldn't be
        specified in the id, as two ABIs from different architectures
        should never met.

        A good start whould be to use:
        [specific; compiler; os; vendor]

        Example: ["*exit"; "gnueabi"; "linux"; "unknown"]

        Will encode an ABI of [exit] family of functions for ARM linux
        gnueabi. The recommended printing format for the ABI is to
        append the arch name and print all constituents of the name from
        right to left, using "-" symbol as a separator.

        In any case, the meaning of the identifier is specific to a
        particular family of ABIs, that are, usually inherit the same
        parent or set of parents. *)
    method id : string list

    (** [self#specific] is [true] if this ABI is specific
        for the provided function. The [specific] ABI is always more
        preferrable to non-specific one. If more than one specific
        ABIs is applicable for the provided symbol, than the normal
        resolution process will be used (see method [choose])
    *)
    method specific : bool

    (** [self#choose other] used to sort a set of applicable ABI.

        Must return:
        - [0] if [other] abi is not known or is considered equaly
        applicable for the given context.
        - [1] if [other] abi is known, and [self] is preferrable
        to [other]
        - [-1] if [other] abi is more preferrable. This value can
        be even returned when the other abi is not known to [self].

        In case of inconsistency the solving mechanism will consider
        inconsistent abi's as equal. The examples of inconsistent
        comparison results are: both abis preferred each other, or
        both abis claimed that they are preferrable. *)
    method choose : abi -> int

    (** [return_value] returns an expression, that can be used to return
        a value from a function. Use [Bil.concat] to represent return
        value that doesn't fit into one register  *)
    method return_value : (var * exp) option

    (** [args] returns a list of expressions that represents
        arguments of the given function. Each expression can be
        annotated with suggested name  *)
    method args : (var * exp) list

    (** [vars] returns a list of expressions, that represents
        local variables of the function  *)
    method vars : (var * exp) list

    (** [records] returns a list of records, found in the symbol.  *)
    method records : (var * exp) list list
  end

  (** symbol name may be provided if known. Also an access
      to the whole binary image is provided if there is one. *)
  type abi_constructor = sub term -> abi


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

    (** Program counter.
        @deprecated this maybe removed, if we decide, that PC register
        breaks abstraction.*)
    val pc  : var

    (** Stack pointer  *)
    val sp  : var

    (** {4 Flag registers}  *)
    val zf  : var
    val cf  : var
    val vf  : var
    val nf  : var

    (** [addr_of_pc mem] given a memory region, representing some
        instruction, return a value to which a PC register is set when
        this instruction is executed by CPU.
        This function is useful to resolve PC-relative calculations.
        @deprecated this maybe removed, if we decide, that PC register
        breaks abstraction.
    *)
    val addr_of_pc : mem -> addr

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

  (** Application Binary Interface

      Under this name, we're gathering several different concepts, like:

      - calling convention
      - stack frame organization
      - data representation
      - special functions

      Later we may extend the ABI class to handle system calls, type
      inference and other stuff.

      Each ABI object is constructed specifically to a particular
      symbol using the following functional constructor, of the
      following type:

      [?image:image -> ?sym:string -> mem -> block -> abi option]

      ABI constructors are registered in the target specific lifter,
      and constructed for each symbol. Afterwards a set of most
      (and equally) applicable ABIs is provided to a calling part,
      to which it is left the final decision on how to disambiguate
      them. *)
  module type ABI = sig


    (** creates a set of ABI for the provided symbol.
        Until [all] parameter is set to true the ABI will be
        disambiguated, using [choose] method. Only equally
        valid ABI are returned. *)
    val create : ?merge:(abi list -> abi) -> sub term -> abi

    (** [merge abis] create an abi that tries to take best from all
        provided abi. If the input list is empty, then the stub abi
        will be returned. *)
    val merge : abi list -> abi

    val merge_id : string list -> string list -> string list

    (** ABI that understands nothing. All methods are dump stubs. *)
    class stub : abi

    val to_string : arch -> string list -> string

    (** registers given ABI under the given target   *)
    val register : abi_constructor -> unit
  end

  (** a jump kind.
      A jump to another block can be conditional or unconditional.
  *)
  type jump = [
    | `Jump     (** unconditional jump                  *)
    | `Cond     (** conditional jump                    *)
  ] with compare, sexp
  (** This type defines a relation between two basic blocks.  *)
  type edge = [jump | `Fall] with compare,sexp

  (** Access to block attributes.
      This interface provides only access to block attributes, but
      doesn't allow to navigate to other blocks.
  *)
  module type Block_accessors = sig
    type t with compare, sexp_of
    type insn
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

    include Comparable with type t := t
    include Hashable   with type t := t
    (** all the printing stuff, including [to_string] function *)
    include Printable  with type t := t
  end

  (** Navigate to neighborhood blocks.

      The following functions allows you to navigate through blocks
      without explicitly using graphs. Each neighborhood function
      returns closest neighbors as a lazy sequence. Please, be cautious,
      since this can contain loops, i.e. block can contain itself as a
      predecessor.

      You can use [Block.compare] or [compare_block] functions, to safely
      compare blocks with each other, without a risk of non-termination.
  *)
  module type Block_traverse = sig

    (** block type  *)
    type t

    (** [dest] type also handles indirect jumps.  *)
    type dest = [
      | `Block of t * edge
      | `Unresolved of    jump
    ] with compare, sexp_of

    (** [dests blk] block immediate destinations including unresolved
        one. Successors are returned in the order of execution, e.g.,
        taken branch comes before the implicit one. *)
    val dests : t -> dest seq

    (** [succs blk] block immediate successors in the order of
        execution, .  *)
    val succs : t -> t seq

    (** [preds blk] block immediate predecessors in unspecified order. *)
    val preds : t -> t seq
  end

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
        | `Valid  (** stop on first valid insn  *)
        |  Kind.t   (** stop on first insn of the specified kind *)
      ] with sexp

      (** {2 Basic types }  *)
      type reg  with bin_io, compare, sexp
      type imm  with bin_io, compare, sexp
      type fmm  with bin_io, compare, sexp
      type (+'a,+'k) insn
      type (+'a,+'k) insns = (mem * ('a,'k) insn option) list
      type empty     (** set when information is not stored                *)
      type asm       (** set when assembler information is stored        *)
      type kinds     (** set when instruction kind information is stored *)

      type full_insn = (asm,kinds) insn with compare, sexp_of

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

      (** [create ?debug_level ?cpu ~backend target] creates a disassembler
          for the specified [target]. All parameters are backend specific,
          consult the concrete backend for more information. In general,
          the greater [debug_level] is, the more debug information will be
          outputed by a backend. To silent backend set it [0]. This is a
          default value. Example:

          [create ~debug_level:3 ~backend:"llvm" "x86_64"]
      *)
      val create :
        ?debug_level:int ->
        ?cpu:string ->
        backend:string ->
        string ->
        (empty, empty) t Or_error.t

      (** enables storing assembler information  *)
      val store_asm : (_,'k) t -> (asm,'k) t

      (** enables storing instruction kinds information *)
      val store_kinds : ('a,_) t -> ('a,kinds) t

      (** [run ?stop_on ?invalid ?stopped dis mem ~init ~return ~hit]
          performs recursive disassembly of specified memory [mem]. The
          process of disassembly can be driven using [stop], [step], [back]
          and [jump] functions, described later.

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
        ?stop_on:pred list ->
        ?invalid:(('a,'k,'s,'r) state -> mem -> 's -> 'r) ->
        ?stopped:(('a,'k,'s,'r) state -> 's -> 'r) ->
        ?hit:(('a,'k,'s,'r) state -> mem -> (asm,kinds) insn -> 's -> 'r) ->
        ('a,'k) t ->
        return:('s -> 'r) ->
        init:'s -> mem -> 'r

      (** [insn_of_mem dis mem] performes a disassembly of one instruction
          from the given memory region [mem]. Returns a tuple
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
      type block with compare, sexp_of
      type lifter = mem -> Basic.full_insn -> bil Or_error.t
      type maybe_insn = Basic.full_insn option * bil option with sexp_of
      type decoded = mem * maybe_insn with sexp_of

      type error = [
        | `Failed_to_disasm of mem
        | `Failed_to_lift of mem * Basic.full_insn * Error.t
      ] with sexp_of

      val run :
        ?backend:string ->
        ?lifter:lifter -> ?roots:addr list -> arch -> mem -> t Or_error.t

      val blocks : t -> block Table.t

      val errors : t -> error list

      (** Low-level opaque representation of basic block. *)
      module Block : sig
        include Block_accessors
          with type t = block
           and type insn := maybe_insn
        include Block_traverse  with type t := t
      end
    end
  end

  (** Assembly instruction.  *)
  module Insn : sig
    type t = insn with bin_io, compare, sexp

    type op = Op.t with bin_io, compare, sexp

    include Regular with type t := t

    (** returns backend specific name of instruction *)
    val name : t -> string

    (** target-specific assembler string representing the instruction  *)
    val asm  : t -> string

    (** returns BIL program specifying instruction semantics  *)
    val bil  : t -> bil

    (** instruction operands  *)
    val ops  : t -> op array

    (** {3 Instruction predicates} *)

    (** [is_jump] [true] for all jumps  *)
    val is_jump : t -> bool

    (** [is_conditional] [true] for conditional jumps  *)
    val is_conditional_jump : t -> bool

    (** [is_unconditional] iff [is_jump && not is_conditional_jump]  *)
    val is_unconditional_jump : t -> bool

    (** [is_indirect_jump] [true] if it is indirect *)
    val is_indirect_jump : t -> bool

    (** [is_call] is [true] for all call instructions  *)
    val is_call : t -> bool

    (** [is_return] [true] for returns  *)
    val is_return : t -> bool

    (** [may_affect_control_flow] is true if it may affect control flow.
        «may» stays for the fact, that it «may not» affect.
    *)
    val may_affect_control_flow : t -> bool

    (** [has_side_effect] is [true] if instruction may load or store  *)
    val has_side_effect : t -> bool

    (** [may_load] is true if instruction may load data from memory  *)
    val may_load : t -> bool

    (** [may_store] is true if instruction may store data to memory  *)
    val may_store : t -> bool


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


    (** {3 Creating}
        The following functions will create [insn] instances from a lower
        level representation.
    *)
    val of_basic : ?bil:bil -> Disasm_expert.Basic.full_insn -> t

  end


  (** Basic block of machine instructions.  *)
  module Block : sig
    type t = block with compare, sexp_of
    include Block_accessors with type t := t and type insn := insn
    include Block_traverse  with type t := t

    (** Graph of blocks.  *)
    module Graph : Graph
      with type node = t
       and type Node.label = t
       and type Edge.label = edge

    (** [to_graph ?bound entry] builds a graph starting with [entry] and
        spanning all reachable blocks.
        @param bound if specified, then the resulting graph will
        contain only blocks [b] for which [bound (Block.addr b)]
        evaluated to [true]. {!Symtab.create_bound} is an example of
        such function. *)
    val to_graph : ?bound:(addr -> bool) -> t -> Graph.t
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
    module ABI : ABI

    (** [lift mem insn] lifts provided instruction to BIL.
        Usually you do not need to call this function directly, as
        [disassemble] function will do the lifting. *)
    val lift : mem -> ('a,'k) Disasm_expert.Basic.insn -> bil Or_error.t
  end

  (** [target_of_arch arch] returns a module packed into value, that
      abstracts target architecture. The returned module has type
      {!Target} and can be unpacked locally with:
      {[
        let module Target = (val target_of_arch arch) in
      ]}
  *)
  val target_of_arch : arch -> (module Target)


  (** ARM architecture. *)
  module ARM  : sig

    val lift : mem -> ('a,'k) Disasm_expert.Basic.insn -> bil Or_error.t

    module ABI : ABI

    (** ARM CPU.
        Other than common CPU interface, this module also exposes ARM
        specific registers and flags.    *)
    module CPU : sig
      include CPU
      val spsr : var
      val cpsr : var
      val nf : var
      val zf : var
      val cf : var
      val vf : var
      val qf : var
      val ge : var array
      val itstate : var
      val lr : var
      val pc : var
      val sp : var
      val r0 : var
      val r1 : var
      val r2 : var
      val r3 : var
      val r4 : var
      val r5 : var
      val r6 : var
      val r7 : var
      val r8 : var
      val r9 : var
      val r10 : var
      val r11 : var
      val r12 : var
    end


    (** Arm Instruction Set.  *)
    module Insn : sig
      type move = [
        | `ADCri
        | `ADCrr
        | `ADCrsi
        | `ADCrsr
        | `ADDri
        | `ADDrr
        | `ADDrsi
        | `ADDrsr
        | `ANDri
        | `ANDrr
        | `ANDrsi
        | `ANDrsr
        | `BICri
        | `BICrr
        | `BICrsi
        | `BICrsr
        | `CMNri
        | `CMNzrr
        | `CMNzrsi
        | `CMNzrsr
        | `CMPri
        | `CMPrr
        | `CMPrsi
        | `CMPrsr
        | `EORri
        | `EORrr
        | `EORrsi
        | `EORrsr
        | `MOVTi16
        | `MOVi
        | `MOVi16
        | `MOVr
        | `MOVsi
        | `MOVsr
        | `MVNi
        | `MVNr
        | `MVNsi
        | `MVNsr
        | `ORRri
        | `ORRrr
        | `ORRrsi
        | `ORRrsr
        | `RSBri
        | `RSBrr
        | `RSBrsi
        | `RSBrsr
        | `RSCri
        | `RSCrr
        | `RSCrsi
        | `RSCrsr
        | `SBCri
        | `SBCrr
        | `SBCrsi
        | `SBCrsr
        | `SUBri
        | `SUBrr
        | `SUBrsi
        | `SUBrsr
        | `TEQri
        | `TEQrr
        | `TEQrsi
        | `TEQrsr
        | `TSTri
        | `TSTrr
        | `TSTrsi
        | `TSTrsr
      ] with bin_io, compare, sexp, enumerate

      type bits = [
        | `BFC
        | `BFI
        | `PKHTB
        | `RBIT
        | `SBFX
        | `SWPB
        | `SXTAB
        | `SXTAH
        | `SXTB
        | `SXTH
        | `UBFX
        | `UXTAB
        | `UXTAH
        | `UXTB
        | `UXTH
        | `REV
        | `REV16
        | `CLZ
      ] with bin_io, compare, sexp, enumerate

      type mult = [
        | `MLA
        | `MLS
        | `MUL
        | `SMLABB
        | `SMLAD
        | `SMLAL
        | `SMLALBT
        | `SMLAWB
        | `SMUAD
        | `SMULBB
        | `SMULL
        | `SMULTB
        | `UMLAL
        | `UMULL
      ] with bin_io, compare, sexp, enumerate


      type mem_multi = [
        | `LDMDA
        | `LDMDA_UPD
        | `LDMDB
        | `LDMDB_UPD
        | `LDMIA
        | `LDMIA_UPD
        | `LDMIB
        | `LDMIB_UPD
        | `STMDA
        | `STMDA_UPD
        | `STMDB
        | `STMDB_UPD
        | `STMIA
        | `STMIA_UPD
        | `STMIB
        | `STMIB_UPD
      ] with bin_io, compare, sexp, enumerate


      type mem = [
        | mem_multi
        | `LDRBT_POST_IMM
        | `LDRBT_POST_REG
        | `LDRB_POST_IMM
        | `LDRB_POST_REG
        | `LDRB_PRE_IMM
        | `LDRB_PRE_REG
        | `LDRBi12
        | `LDRBrs
        | `LDRD
        | `LDRD_POST
        | `LDRD_PRE
        | `LDREX
        | `LDREXB
        | `LDREXD
        | `LDREXH
        | `LDRH
        | `LDRHTr
        | `LDRH_POST
        | `LDRH_PRE
        | `LDRSB
        | `LDRSBTr
        | `LDRSB_POST
        | `LDRSB_PRE
        | `LDRSH
        | `LDRSHTi
        | `LDRSHTr
        | `LDRSH_POST
        | `LDRSH_PRE
        | `LDRT_POST_REG
        | `LDR_POST_IMM
        | `LDR_POST_REG
        | `LDR_PRE_IMM
        | `LDR_PRE_REG
        | `LDRi12
        | `LDRrs
        | `STRBT_POST_IMM
        | `STRBT_POST_REG
        | `STRB_POST_IMM
        | `STRB_POST_REG
        | `STRB_PRE_IMM
        | `STRB_PRE_REG
        | `STRBi12
        | `STRBrs
        | `STRD
        | `STRD_POST
        | `STRD_PRE
        | `STREX
        | `STREXB
        | `STREXD
        | `STREXH
        | `STRH
        | `STRHTr
        | `STRH_POST
        | `STRH_PRE
        | `STRT_POST_REG
        | `STR_POST_IMM
        | `STR_POST_REG
        | `STR_PRE_IMM
        | `STR_PRE_REG
        | `STRi12
        | `STRrs
      ] with bin_io, compare, sexp, enumerate

      type branch = [
        | `BL
        | `BLX
        | `BLX_pred
        | `BLXi
        | `BL_pred
        | `BX
        | `BX_RET
        | `BX_pred
        | `Bcc
      ] with bin_io, compare, sexp, enumerate

      type special = [
        | `CPS2p
        | `DMB
        | `DSB
        | `HINT
        | `MRS
        | `MSR
        | `PLDi12
        | `SVC
      ] with bin_io, compare, sexp, enumerate

      type t = [
        | move
        | bits
        | mult
        | mem
        | branch
        | special
      ] with bin_io, compare, sexp, enumerate

      (** [create basic_insn] lifts ARM instruction from a basic instruction  *)
      val create : ('a,'b) Disasm_expert.Basic.insn -> t option
      include Regular with type t := t
    end

    (** ARM Registers.  *)
    module Reg : sig
      type nil = [ `Nil ]
      with bin_io, compare, sexp, enumerate

      (** General purpose registers  *)
      type gpr = [
        | `R0
        | `R1
        | `R2
        | `R3
        | `R4
        | `R5
        | `R6
        | `R7
        | `R8
        | `R9
        | `R10
        | `R11
        | `R12
        | `LR
        | `PC
        | `SP
      ] with bin_io, compare, sexp, enumerate

      type gpr_or_nil = [nil | gpr]
      with bin_io, compare, sexp, enumerate

      (** conditition code registers  *)
      type ccr = [
        | `CPSR
        | `SPSR
        | `ITSTATE
      ] with bin_io, compare, sexp, enumerate

      type ccr_or_nil = [nil | ccr ]
      with bin_io, compare, sexp, enumerate

      type non_nil = [gpr | ccr]
      with bin_io, compare, sexp, enumerate

      type t = [nil | non_nil]
      with bin_io, compare, sexp, enumerate

      (** lifts basic register to a ARM one  *)
      val create : Disasm_expert.Basic.reg -> t option

      include Regular with type t := t
    end


    (** ARM instruction operands  *)
    module Op : sig
      type t =
        | Reg of Reg.t
        | Imm of word
      with bin_io, compare, sexp

      (** lifts operand from a basic one  *)
      val create : op -> t option
      include Regular with type t := t
    end

    (** Condition prefixes.  *)
    module Cond : sig
      type t = [
        | `EQ
        | `NE
        | `CS
        | `CC
        | `MI
        | `PL
        | `VS
        | `VC
        | `HI
        | `LS
        | `GE
        | `LT
        | `GT
        | `LE
        | `AL
      ] with bin_io, compare, sexp, enumerate

      (** decodes condition value from a word  *)
      val create : word -> t Or_error.t
      include Regular with type t := t
    end
  end

  (** [x86] architecture  *)
  module IA32 : sig
    val lift : mem -> ('a,'k) Disasm_expert.Basic.insn -> stmt list Or_error.t
    module ABI : ABI
    module CPU : sig
      include CPU
      val rbp : var
      val rsp : var
      val rsi : var
      val rdi : var
      val rip : var
      val rax : var
      val rbx : var
      val rcx : var
      val rdx : var
      val r : var array
    end
  end

  (** [x86-64] architecture  *)
  module AMD64 : sig

    val lift : mem -> ('a,'k) Disasm_expert.Basic.insn -> stmt list Or_error.t

    module CPU : sig
      include CPU
      include module type of IA32.CPU
      val r : var array
    end
    module ABI : ABI
  end


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
        with @prefix, or number identifier.   *)
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
        with the given [id] *)
    val remove : ('a,_) cls -> 'a t -> tid -> 'a t

    (** [change t p id f] if [p] contains subterm with of a given kind
        [t] and identifier [id], then apply [f] to this
        subterm. Otherwise, apply [f] to [None]. If [f] return [None],
        then remove this subterm (given it did exist), otherwise,
        update parent with a new subterm.  *)
    val change : ('a,'b) cls -> 'a t -> tid -> ('b t option -> 'b t option) -> 'a t


    (** [enum ?rev t p] enumerate all subterms of type [t] of the
        given term [p] *)
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
        order. Otherwise they're reversed. If there is no term with a
        given [tid], then an empty sequence is returned. *)
    val after : ('a,'b) cls -> ?rev:bool -> 'a t -> tid -> 'b t seq

    (** [before t ?rev p tid] returns all term that occurs before
        defintion with given [tid] in blk. If there is no such
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

    (** [get_attr term attr] returns a value of the given [attr] in
        [term] *)
    val get_attr : 'a t -> 'b tag -> 'b option

    (** [has_attr term attr] is [true] if [term] has attribute [attr]  *)
    val has_attr : 'a t -> 'b tag -> bool

    (** [del_attr term attr] deletes attribute [attr] from [term]  *)
    val del_attr : 'a t -> 'b tag -> 'a t
  end

  (* TBD

     module Callgraph : Graph
     with type node = sub term
     and type Node.label = sub term
     and type Edge.label = Blk.Set.t
  *)
  (** Program.  *)
  module Program : sig
    (** Program is a collection of function terms. *)

    type t = program term

    (** [create ?tid ()] creates an empty program. If [tid]  *)
    val create : ?tid:tid -> unit -> t

    (** [lift symbols] takes a table of functions and return a whole
        program lifted into IR *)
    val lift : symtab -> program term

    (* TBD: val to_callgraph : t -> Callgraph.t *)

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
    val lift : ?bound:(addr -> bool) -> block -> sub term

    (** [name sub] returns a subroutine name  *)
    val name : t -> string

    (** updates subroutine name *)
    val with_name : t -> string -> t

    (** [ssa sub] returns [sub] in SSA form. The underlying algorithm
        produces a semi-pruned SSA form. To represent different
        versions of the same variable we use {{!Var}variable
        versions}. Any definition of a variable increases its version
        number. So, the zero version is reserved for variables that
        weren't defined before the first use.  *)
    val ssa : t -> t

    (** [is_ssa sub] is [true] if [sub] was transformed into an SSA
        form. This is O(1) predicate that doesn't really check, that
        a subroutine is in an SSA form, so it is a responsibility of
        a user to preserve the SSA form on any transformation.    *)
    val is_ssa : t -> bool

    (** [free_vars sub] computes a set of variables that are free in a
        given subroutine [sub]. The variable is considered free if it
        is used before defined or is not locally bound.  If [sub] is in
        an SSA form, then the set is computed trivially, thanks to a
        naming scheme. If program is not in an SSA form, then a BFS on a
        dominators tree is used.  *)
    val free_vars : t -> Var.Set.t

    (** [infer_args sub arch] uses {!abi} to infer and populate
        arguments of subroutine [sub].  *)
    val infer_args : t -> arch -> t

    (** [to_graph sub] builds a graph of subroutine [sub]. Graph nodes
        are block term identifiers, and edges are labeled with term
        identifiers of the jmp terms, that corresponds to the edge.
        This representation is useful, if you need to compute some
        graph relation on a subroutine, that will be later used to
        perform its incremental transformation. *)
    val to_graph : t -> Graphlib.Tid.Tid.t

    (** [to_cfg sub] builds a graph representation of a subroutine
        [sub]. All graph operations are mapped to corresponding
        [Term] operations. See {!Graphlib.Ir} for more information.*)
    val to_cfg : t -> Graphlib.Ir.t

    (** [of_cfg cfg] extracts a [sub term] from a given graph [cfg].
        Since {!Graphlib.Ir} module builds term incrementally this
        operation is just a projection, i.e., it has O(0) complexity.  *)
    val of_cfg : Graphlib.Ir.t -> t

    (** Subroutine builder *)
    module Builder : sig
      type t
      (** initializes empty subroutine builder.  *)
      val create : ?tid:tid -> ?args:int -> ?blks:int -> ?name:string -> unit -> t
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
        the executes simultaneously . Definitions are stored in the
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
    val lift : block -> blk term list


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

    (** [create ?cond kind] creates a jump of given kind  *)
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
    (** Phi nodes are used to represent a set of values, that can be
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
        to which the control flow should (but mustn't) continue when
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

  (** Target of analysis.  *)
  module Project : sig
    (** A project groups together all the information recovered from
        the underlying binary. It is also used for exchanging
        information between {{!section:project}passes}.  *)

    type t

    (** [from_file filename] creates a project from a binary file. The
        file must be in format, supportable by some of our loader plugins,
        e.g., ELF, COFF, MACH-O, etc. A provided [filename] is stored
        in a {!filename} field of a project.

        @param on_warning is a function that will be called if some
        non-critical problem has occurred during loading file;

        @param name is a naming function, that allows to specify a
        name for a function starting at give address. If [None] is
        provided, then the default naming scheme will be used, i.e.,
        [sub_ADDR].

        @param backend allows to choose loader plugin

        @param roots allows to provide starting approximation of the
        roots for recursive disassembling procedure. Each root should
        be a start of a function.


    *)
    val from_file :
      ?on_warning:(Error.t -> unit Or_error.t) ->
      ?backend:string ->
      ?name:(addr -> string option) ->
      ?roots:addr list ->
      string -> t Or_error.t

    (** [from_image image] is like {!from_file} but accepts already
        loaded image of a binary file. If [image] was loaded from a
        file, then {!filename} field is set to the name of the file.  *)
    val from_image :
      ?name:(addr -> string option) ->
      ?roots:addr list ->
      image -> t Or_error.t

    (** [from_mem arch mem] creates a project directly from a memory
        object. Parameters [name] and [roots] has the same meaning as
        in {!from_file}  *)
    val from_mem :
      ?name:(addr -> string option) ->
      ?roots:addr list ->
      arch -> mem -> t Or_error.t

    (** [from_string arch string] creates a memory object from a
        provided string and uses {!from_mem} to create a project.  *)
    val from_string :
      ?base:addr ->
      ?name:(addr -> string option) ->
      ?roots:addr list ->
      arch -> string -> t Or_error.t

    (** [from_bigstring arch bigstring] is the same as {!from_string}
        but accepts a value of type [bigstring] *)
    val from_bigstring :
      ?base:addr ->
      ?name:(addr -> string option) ->
      ?roots:addr list ->
      arch -> Bigstring.t -> t Or_error.t

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

    (** [tag_memory project region tag value] tags given [region] of
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

    (** {3 Registering and running passes}

        To add new pass one of the following [register_*] functions
        should be called.*)

    type 'a register = ?deps:string list -> string -> 'a -> unit

    (** An error that can occur when loading or running pass.
        - [Not_loaded name] pass with a given [name] wasn't loaded for
          some reason. This is a very unlikely error, indicating
          either a logic error in the plugin system implementation or
          something very weird, that we didn't expect.

        - [Is_duplicate name] more than one plugin were registered
          under this [name], either it is the same plugin or a name clash
          between to different we don't know.

        - [Not_found name] when we tried to load plugin with a given
          [name] we failed to find it in our search paths.

        - [Doesn't_register name] the plugin with a given [name]
          doesn't register a pass with the same name.

        - [Load_failed (name,problem)] a [problem] has occured, when
          we tried to dynamically link a plugin with a given [name]

        - [Runtime_error (name,exn)] when plugin with a given [name]
          was ran it raised an [exn].

    *)
    type error =
      | Not_loaded of string
      | Is_duplicate of string
      | Not_found of string
      | Doesn't_register of string
      | Load_failed of string * Error.t
      | Runtime_error of string * exn
    with sexp_of

    (** raised when a pass failed to load or to run. Note: this
        exception is raised only from two functions in this module, that
        state this in their documentation and has [_exn] suffix in their
        name. *)
    exception Pass_failed of error with sexp

    (** [register_pass_with_args name pass] registers [pass] that
        requires command line arguments. The arguments will be passed
        in the first parameter of the [pass] function.

        Parameter [deps] is list of dependencies. Each dependency is a
        name of a pass, that should be run before the [pass]. If
        dependency pass is not registered it will be auto-loaded (See
        {!run_passes}) *)
    val register_pass_with_args : (string array -> t -> t) register

    (** [register_pass ?deps name pass] registers project transformation,
        that doesn't require command line arguments.
        (See {!register_pass_with_args})*)
    val register_pass : (t -> t) register

    (** [register_pass ?deps name pass] registers [pass] that doesn't modify
        the project effect and is run only for side effect.
        (See {!register_pass_with_args})  *)
    val register_pass': (t -> unit) register

    (** [register_pass_with_args' name pass] register a [pass] that
        requires arguments for a side effect.
        (See {!register_pass_with_args}) *)
    val register_pass_with_args' : (string array -> t -> unit) register

    (** [run_passes ?library ?argv project] folds [project] over all
        registered passes. Passes are invoked in the order of
        registration.

        If pass has dependencies, then they will be run before the
        pass. The dependencies will be auto-loaded if needed. The
        auto-loading procedure will look for the file with the
        specified name (modulo hyphens) and [".plugin"] extension in
        current folder and all folders specified with [library]
        argument. If nothing found, then it will search for plugins of
        system ["bap.pass"] using findlib. If the dependency cannot be
        satisfied [run_pass] will terminate with error.

        If pass requires command line arguments then they will be
        provided to a pass as a first parameter. The arguments will be
        extracted from [argv] array (which defaults to [Sys.argv]) by
        removing all arguments that doesn't start with
        [--name-]. Then, from all command arguments that are left, the
        [--name-] prefix is substituted with [--]. For example, if
        [argv] contained [ [| "bap"; "-lcallgraph";
        "--callgraph-help"|]] then pass that registered itself under
        [callgraph] name will receive the following array of arguments
        [ [| "callgraph"; --help |] ]. That means, that plugins can't
        accept arguments that are anonymous or short options.

        Note: currently only the following syntax is supported:
         [--plugin-name-agrument-name=value], the following IS NOT
         supported [--plugin-name-argument-name value].
    *)
    val run_passes : ?library:string list -> ?argv:string array -> t -> t Or_error.t


    (** [passes ?library ()] returns a transitive closure of all
        passes registered in the system so far.   *)
    val passes : ?library:string list -> unit -> string list Or_error.t


    (** [run_passes_exn proj] is the same as {!run_passes}, but raises
        an exception on error. Useful to provide custom error
        handling/printing.
        @raise Pass_failed if failed to load, or if plugin failed at runtime.
    *)
    val run_passes_exn : ?library:string list -> ?argv:string array -> t -> t


    (** [passes_exn proj] is the same as {!passes}, but raises
        an exception on error. Useful to provide custom error
        handling/printing.
        @raise Pass_failed if failed to load some plugin *)
    val passes_exn : ?library:string list -> unit -> string list

  end

  type project = Project.t

  (** Dwarf library
      This library gives an access to debugging information stored
      in a binary program.  *)
  module Dwarf : sig
    module Leb128 : sig
      (** an encoded value  *)
      type t with bin_io, compare, sexp

      (** [encode ~signed v] encodes value [v] in a LEB128 format. If
          signed is true, then uses signed encoding. *)
      type 'a encoder = ?signed:bool -> 'a -> t
      (** [decode leb] decodes a number from LEB128 representation.  *)
      type 'a decoder = t -> 'a Or_error.t

      (** [size leb] return size in bytes of the number stored in LEB128
          encoding.  *)
      val size: t -> int
      val read: ?signed:bool -> string -> pos_ref:int ref -> t Or_error.t
      val write: t -> string -> pos:int -> unit

      val to_int:   int   decoder
      val to_int32: int32 decoder
      val to_int64: int64 decoder

      val of_int:   int   encoder
      val of_int32: int32 encoder
      val of_int64: int64 encoder
    end

    (** File sections  *)
    module Section : sig
      type t =
        | Info
        | Abbrev
        | Str
      with sexp,bin_io,compare,variants
    end

    (** Debug Entry Tag  *)
    module Tag : sig
      type t =
        | Compile_unit
        | Partial_unit
        | Subprogram
        | Entry_point
        | Inlined_subroutine
        | Unknown of int
      with sexp,bin_io,compare,variants
    end


    (** Attribute  *)
    module Attr : sig
      type t =
        | Name
        | Low_pc
        | High_pc
        | Entry_pc
        | Unknown of int
      with sexp,bin_io,compare,variants
    end

    type lenspec =
      | Leb128
      | One
      | Two
      | Four
      | Eight
    with sexp,bin_io,compare

    (** Attribute form  *)
    module Form : sig
      type t =
        | Addr
        | String
        | Block of lenspec
        | Const of lenspec
        | Flag_present
        | Strp
        | Ref of lenspec
        | Indirect
        | Offset
        | Expr
        | Sig
      with sexp,bin_io,compare,variants
    end

    type tag  = Tag.t  with sexp,bin_io,compare
    type attr = Attr.t with sexp,bin_io,compare
    type form = Form.t with sexp,bin_io,compare
    type section = Section.t with sexp,bin_io,compare
    type fn with bin_io, compare, sexp

    (** Current function representation.  *)
    module Fn : sig
      type t = fn with bin_io, compare, sexp
      val pc_lo : t -> addr
      val pc_hi : t -> addr option
      include Identifiable.S with type t := t
    end


    (** Buffer is a light abstraction over [string] and [bigstring],
        that can allow one to share the same string for different sections
        without explicit copying.
    *)
    module Buffer : sig
      type 'a t
      (** [create ~pos:0 ] creates a buffer from a data  *)
      val create: ?pos:int -> 'a -> 'a t

      (** [with_pos buf pos] creates a new buffer that shares data with
          [buf], but has different starting position  *)
      val with_pos: 'a t -> int -> 'a t

      (** [with_off buf off] creates a new buffer that shares data with
          [buf], but has different starting position equal to [pos buf + off] *)
      val with_off: 'a t -> int -> 'a t

      (** [pos buf] starting position  *)
      val pos: 'a t -> int

      (** [data pos] actual data.

          Note: it doesn't start from [pos], it start from [0] *)
      val data: 'a t -> 'a
    end

    module Data : sig
      type 'a t
      type 'a buffer = 'a Buffer.t

      (** [create endian sections] creates data representation from a assoc list
          of sections. Will complain if there're repeating sections.  *)
      val create: endian -> (section * 'a buffer) list -> 'a t Or_error.t

      (** [section data] lookups for a [section] in [data]  *)
      val section: 'a t -> section -> 'a buffer Or_error.t

      (** [endian data] the endianness of [data]  *)
      val endian: 'a t -> endian
    end

    (** Function boundary identification.  *)
    module Fbi : sig
      type t

      (** [create data] tries to create a DWARF reader, from
          supplied [data]. May yield an error, if there wasn't sufficient
          sections, or if format is not understandable.

          To provide information about functions parser needs at least this
          three sections:

          - .debug_abbrev [Section.Abbr]
          - .debug_info   [Section.Info]
          - .debug_str    [Section.Str]
      *)
      val create : string Data.t -> t Or_error.t

      (** [functions searcher] enumerates functions  *)
      val functions : t -> (string * fn) seq
    end
  end

  (** Parse binary data in ELF format.  *)
  module Elf : sig
    module Types : sig
      type e_class =
        | ELFCLASS32
        | ELFCLASS64

      type e_data =
        | ELFDATA2LSB
        | ELFDATA2MSB

      type e_osabi =
        | ELFOSABI_SYSV
        | ELFOSABI_HPUX
        | ELFOSABI_NETBSD
        | ELFOSABI_LINUX
        | ELFOSABI_SOLARIS
        | ELFOSABI_AIX
        | ELFOSABI_IRIX
        | ELFOSABI_FREEBSD
        | ELFOSABI_TRU64
        | ELFOSABI_MODESTO
        | ELFOSABI_OPENBSD
        | ELFOSABI_ARM_AEABI
        | ELFOSABI_ARM
        | ELFOSABI_STANDALONE
        | ELFOSABI_EXT of int

      type e_type =
        | ET_NONE
        | ET_REL
        | ET_EXEC
        | ET_DYN
        | ET_CORE
        | ET_EXT of int

      type e_machine =
        | EM_NONE
        | EM_M32
        | EM_SPARC
        | EM_386
        | EM_68K
        | EM_88K
        | EM_860
        | EM_MIPS
        | EM_S370
        | EM_MIPS_RS3_LE

        | EM_PARISC
        | EM_VPP500
        | EM_SPARC32PLUS
        | EM_960
        | EM_PPC
        | EM_PPC64
        | EM_S390

        | EM_V800
        | EM_FR20
        | EM_RH32
        | EM_RCE
        | EM_ARM
        | EM_ALPHA
        | EM_SH
        | EM_SPARCV9
        | EM_TRICORE
        | EM_ARC
        | EM_H8_300
        | EM_H8_300H
        | EM_H8S
        | EM_H8_500
        | EM_IA_64
        | EM_MIPS_X
        | EM_COLDFIRE
        | EM_68HC12
        | EM_MMA
        | EM_PCP
        | EM_NCPU
        | EM_NDR1
        | EM_STARCORE
        | EM_ME16
        | EM_ST100
        | EM_TINYJ
        | EM_X86_64
        | EM_PDSP

        | EM_FX66
        | EM_ST9PLUS
        | EM_ST7
        | EM_68HC16
        | EM_68HC11
        | EM_68HC08
        | EM_68HC05
        | EM_SVX
        | EM_ST19
        | EM_VAX
        | EM_CRIS
        | EM_JAVELIN
        | EM_FIREPATH
        | EM_ZSP
        | EM_MMIX
        | EM_HUANY
        | EM_PRISM
        | EM_AVR
        | EM_FR30
        | EM_D10V
        | EM_D30V
        | EM_V850
        | EM_M32R
        | EM_MN10300
        | EM_MN10200
        | EM_PJ
        | EM_OPENRISC
        | EM_ARC_A5
        | EM_XTENSA
        | EM_AARCH64
        | EM_TILEPRO
        | EM_MICROBLAZE
        | EM_TILEGX
        | EM_EXT of int

      type p_type =
        | PT_NULL
        | PT_LOAD
        | PT_DYNAMIC
        | PT_INTERP
        | PT_NOTE
        | PT_SHLIB
        | PT_PHDR
        | PT_OTHER of int32

      type p_flag =
        | PF_X
        | PF_W
        | PF_R
        | PF_EXT of int

      type sh_type =
        | SHT_NULL
        | SHT_PROGBITS
        | SHT_SYMTAB
        | SHT_STRTAB
        | SHT_RELA
        | SHT_HASH
        | SHT_DYNAMIC
        | SHT_NOTE
        | SHT_NOBITS
        | SHT_REL
        | SHT_SHLIB
        | SHT_DYNSYM
        | SHT_EXT of int32

      type sh_flag =
        | SHF_WRITE
        | SHF_ALLOC
        | SHF_EXECINSTR
        | SHF_EXT of int

      type segment = {
        p_type   : p_type;
        p_flags  : p_flag list;
        p_vaddr  : int64;
        p_paddr  : int64;
        p_align  : int64;
        p_memsz  : int64;
        p_filesz : int64;
        p_offset : int64;
      }

      type section = {
        sh_name : int;
        sh_type : sh_type;
        sh_flags : sh_flag list;
        sh_addr : int64;
        sh_size : int64;
        sh_link : int32;
        sh_info : int32;
        sh_addralign : int64;
        sh_entsize : int64;
        sh_offset : int64;
      }

      type elf = {
        e_class : e_class;
        e_data : e_data;
        e_version : int;
        e_osabi : e_osabi;
        e_abiver : int;
        e_type : e_type;
        e_machine : e_machine;
        e_entry : int64;
        e_shstrndx : int;
        e_sections : section seq;
        e_segments : segment seq;
      }

      type table_info = {
        table_offset : int64;
        entry_size : int;
        entry_num : int;
      }
    end

    open Types

    type t = elf

    (** [from_bigstring data] parses data with optional offset
        provided as [pos] and length as [len]  *)
    val from_bigstring : ?pos:int -> ?len:int -> Bigstring.t -> t Or_error.t

    (** [section_name data elf section] retrieves name of [section]
        from [data] *)
    val section_name : Bigstring.t -> t -> section -> string Or_error.t

    (** [string_of_section data section] extracts the section data as
        string from the provided [data]   *)
    val string_of_section : Bigstring.t -> section -> string Or_error.t
  end

  (** Binary signatures storage  *)
  module Signatures : sig
    val save : ?comp:string -> mode:string -> path:string -> arch -> string -> unit
    val load : ?comp:string -> ?path:string -> mode:string -> arch -> string option
    val default_path : string
  end

  (** Byteweight Algorithm implementation *)
  module Byteweight : sig
    module type Corpus = sig
      type t
      type key
      val look : t -> length:int -> int -> key option
    end

    module type S = sig
      type t with bin_io, sexp
      type key
      type corpus

      val create : unit -> t
      val train : t -> max_length:int -> (key -> bool) -> corpus -> unit
      val length : t -> int

      val next : t ->
        length:int ->
        threshold:float ->
        corpus -> int -> int option

      val pp : t printer
    end

    module Make
        (Corpus : Corpus)
        (Trie : Trie with type key = Corpus.key) :
      S with type key = Corpus.key
         and type corpus = Corpus.t

    module Bytes : sig
      include S with type key = mem
                 and type corpus = mem
      val find : t -> length:int -> threshold:float -> corpus -> addr list
    end
  end
end
