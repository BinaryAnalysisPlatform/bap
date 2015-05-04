open Core_kernel.Std

(** {1 Binary Analysis Platform Library}  *)
module Std : sig

  (** {2 Overview}

      BAP has a layered architecture currently consisting of four
      layers:

      {[
        +-----------------------------------------+
        |  +-----------------------------------+  |
        |  |                                   |  |
        |  |       Foundation Library          |  |
        |  |                                   |  |
        |  +-----------------------------------+  |
        |                                         |
        |  +-----------------------------------+  |
        |  |                                   |  |
        |  |          Memory Model             |  |
        |  |                                   |  |
        |  +-----------------------------------+  |
        |                                         |
        |  +-----------------------------------+  |
        |  |                                   |  |
        |  |           Disassembly             |  |
        |  |                                   |  |
        |  +-----------------------------------+  |
        |                                         |
        |  +-----------------------------------+  |
        |  |                                   |  |
        |  |        Semantic Analysis          |  |
        |  |                                   |  |
        |  +-----------------------------------+  |
        +-----------------------------------------+
      ]}


      The {{!bfl}Foundation library} defines core types that are used
      throughout the BAP library for binary analysis. The
      {{!section:image}Memory model} layer is responsible for loading
      and parsing binary objects and representing them in memory. It
      also defines a few useful data structures that are used
      extensively by later layers. The next layer performs
      {{!section:disasm}disassembly} and lifting to BIL. Finally,
      the {{!section:sema}semantic analysis} layer transforms a
      binary into an IR representation, and further provides a set of
      handful analysis tools.

      Another important point of view is the BAP plugin architecture,
      similar to that of GIMP or Frama-C. BAP features a pluggable
      architecture with a number of extension points. For example,
      even the LLVM disassembler is considered a type of plugin.
      Currently we support three such extension points in BAP:

      - {{!Backend}loaders} - to add new binary object loaders;
      - disassemblers - to add new disassemblers;
      - {{!Project}program analysis} - to analyze programs.

      The latter category of plugins is most widely used. Therefore,
      when we use the term "plugin" without making a distinction, we
      refer to a program analysis plugin. The following figure
      provides an overview of the BAP system.

      {[
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
      ]}

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

      At this layer we define the core types that are tightly
      integrated with Binary Intermediate Language ({{!Bil}BIL}). The
      core types are:

      - {{!Arch}arch} - to denote computer architecture;
      - {{!Size}size} - to specify sizes of wors or addresses;
      - {{!Var}var}  - BIL variable;
      - {{!Type}typ} - OCaml type for BIL type;
      - {{!Exp}exp}  - BIL expression;
      - {{!Stmt}stmt} - BIL statement;
      - {{!Bitvector}word,addr} - a bitvector data structure
        to represent immediate data;
      - {{!Tag}'a tag} and {{!Tag}value} - an extensible variant type,
        aka existential, aka type [any];
      - {{!Seq}'a seq} - slightly extended Core [Sequence], aka lazy
        list.

      Every type implements the {{!Regular}Regular} interface. This
      interface is very similar to Core's [Identifiable], and is
      supposed to represent a type that is as common as a built-in
      type. One should expect to find any function that is
      implemented for such types as [int], [string], [char], etc.
      Namely, this interface includes:

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


      {3:tries Tries}

      The Foundation library also defines a prefix tree data
      structure that proves useful for binary analysis applications.
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
       symbols, sections and other meta information.

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

      {[
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
      ]}


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
      This part of the library is currently under heavy
      construction. We will provide information later.  *)


  (** {2:project Writing Program Analysis Plugins}

      To write a program analysis plugin you need to implement a
      function with one of the following interfaces:

      - [project -> project] and register it with
        {{!Project.register_plugin}register_plugin};
      - [project -> unit] and register it with
         {{!Project.register_plugin'}register_plugin'};
      - [string array -> project -> project] and register it with
        {{!Project.register_plugin_with_args}register_plugin_with_args};
      - [string array -> project -> unit] and register it with
        {{!Project.register_plugin_with_args'}register_plugin_with_args'}.

      Once loaded from the [bap] utility (see [man bap]) this function
      will be invoked with a value of type {{!Project.t}project} that
      provides access to all information gathered over the binary so
      far. If the registered function returns a non [unit] type, then it 
      can functionally update the project state, e.g., add
      annotations, discover new symbols, make corrections, and even
      change the architecture and re-disassemble everything.

      {3 Exchanging information}

      For exchanging information in a type safe manner, we use
      {{!Tag}universal values}. Values can be attached to a
      particular memory region, or to a key of type [string]. For the
      first case we use the {{!Memmap}memmap} data structure. It is
      an interval tree containing all the memory regions that are
      used during analysis. For the latter a regular [String.Map] is
      used.

      {3 Annotating memory}

      Depending on the analysis performed and input parameters, one can
      expect that memory may be annotated with the following tags:

      - [Image.region] -- for regions of memory that had a
      particular name in the original binary. For example, in ELF,
      sections have names that annotate a corresponding memory
      region.

      - [Image.section] -- if the binary data was loaded from a binary
      format that contains sections (aka segments), then the
      corresponding memory regions are be marked. Sections provide
      access to permission information.

      - [Image.symbol] -- with this tag we annotate each memory region
      that belongs to a particular symbol. Currently, the type of
      this tag is a string.
  *)


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
        val module_name : string
      end) : Regular with type t := M.t
  end

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

    (** [remove trie key] removes value bound with [key] if any.  *)
    val remove : 'a t -> key -> unit

    (** [longest_match trie key]   *)
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

    (** Predefined trie with [String] as a [Key]  *)
    module String : Trie with type key = string
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
    val to_bytes : t -> endian ->    t Sequence.t
    (** [to_bytes x order] returns bytes of [x] in a specified [order],
        with bytes represented by [char] type *)
    val to_chars : t -> endian -> char Sequence.t

    (** [to_bits x order] returns bits of [x] in a specified [order].
        [order] defines only the ordering of words in a bitvector, bits
        will always be in MSB first order. *)
    val to_bits  : t -> endian -> bool Sequence.t

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

  val bool_t  : typ             (** one bit  *)
  val reg8_t  : typ             (** 8-bit width value *)
  val reg16_t : typ             (** 16-bit width value *)
  val reg32_t : typ             (** 32-bit width value *)
  val reg64_t : typ             (** 64-bit width value *)
  val reg128_t: typ             (** 128-bit width value *)
  val reg256_t: typ             (** 256-bit width value *)

  (** [mem32_t size] 32-bit chunk of memory of a given [size] *)
  val mem32_t : size -> typ

  (** [mem64_t size]  32-bit chunk of memory of a given [size] *)
  val mem64_t : size -> typ


  (** bil variable   *)
  type var
  with bin_io, compare, sexp

  (** BIL variable.
      BIL variables are regular values. Every call to
      [create] will create a fresh variable.  *)
  module Var : sig

    type t = var

    (** implements [Regular] interface  *)
    include Regular with type t := t

    (** [create ?tmp name typ] creates a fresh new variable with
        assosiated [name] and type [typ]. The created variable is
        absolutely new, and is comparable to true only with itself.

        @param tmp designates variable is temporary, with whatever
        meaning assosiated to it by a caller. *)
    val create : ?tmp:bool -> string -> typ -> t

    (** [name var] returns a name assosiated with variable  *)
    val name : t -> string

    (** [typ var] returns a type assosiated with variable  *)
    val typ : t -> typ

    (** [is_tmp] true if variable is temporary  *)
    val is_tmp : t -> bool

    (** Serialization format  *)
    module V1 : sig
      type r = string * int * typ
      val serialize   : t -> r
      val deserialize : r -> t
    end
  end

  (** Main BIL module

      This module defines BIL language and is useful to write BIL
      programs and expressions.

      Example: {[Bil.([
          v := exp_of_op src lsr int32 1;
          r := exp_of_op src;
          s := int32 31;
          while_ (var v <> int32 0) [
            r := var r lsl int32 1;
            r := var r lor (var v land int32 1);
            v := var v lsr int32 1;
            s := var s - int32 1;
          ];
          dest := var r lsl var s;
        ]) cond]} *)
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
        | RSHIFT  (** Right shift, fill with 0. *)
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
        variable is referenced in the provided scope.
        {[
          let is_referenced x = find (object(self)
              inherit [unit] finder
              method! enter_var y cc =
                if Var.(x = y) then cc.return (Some ()); cc
            end)
        ]} *)
    class ['a] finder : object
      inherit ['a option return] visitor
      method find : stmt list -> 'a option
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

    (** [find_map finder bil] search in the BIL   *)
    val find_map : 'a #finder -> stmt list -> 'a option

    (** [find finder bil] returns true if [finder] finds
        something. Note a better name would be [exists] *)
    val find : unit #finder -> stmt list -> bool

    (** [iter visitor bil] apply a visitor for each node of a BIL
        forest. *)
    val iter : unit #visitor -> stmt list -> unit

    (** [fold visitor ~init bil] folds visitor over BIL, passing init
        value through the tree nodes.  *)
    val fold : 'a #visitor -> init:'a -> stmt list -> 'a

    (** [map mapper bil] map or transform BIL program.  *)
    val map : #mapper -> stmt list -> stmt list

    (** [is_referenced x p] is [true] if [x] is referenced in some expression or
        statement in program [p]:
        BUG: currently occurrences on the left hand side of the
        assignment operator is considered a reference too.
    *)
    val is_referenced : var -> stmt list -> bool

    (** [is_assigned x p] is [true] if there exists such [Move]
        statement, that [x] occures on the left side of it. If [strict]
        is true, then only unconditional assignments. By default,
        [strict] is [false] *)
    val is_assigned : ?strict:bool -> var -> stmt list -> bool

    (** [prune_unreferenced p] remove all assignments to variables that
        are not used in the program [p] *)
    val prune_unreferenced : stmt list -> stmt list

    (** [normalize_negatives p] transform [x + y] to [x - abs(y)] if [y < 0] *)
    val normalize_negatives : stmt list -> stmt list

    (** [substitute x y p] substitutes each occurrence of expression [x] by
        expression [y] in program [p] *)
    val substitute : exp -> exp -> stmt list -> stmt list


    (** [substitute_var x y p] substitutes all occurences of variable [x]
        by expression [y] *)
    val substitute_var : var -> exp -> stmt list -> stmt list

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
    include Regular with type t := t
    val pp_adt : t printer
  end

  (** [Regular] interface for BIL statements  *)
  module Stmt : sig
    type t = Bil.stmt
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


  (** Extensible variants

      This module creates an extensible variant type, that resembles
      extensible variant types, introduced in 4.02, but even more safe.

      To extend variant type with a new constructor, use

      [Tag.register constructor_name sexp_of_constructor], where

      constructor name can be any name, and can even clash with previous
      definitions it is guaranteed, that you will receive a new
      representation of the constructor, every time you're calling this
      function even if parameters are the same. The returned value is
      supposed to be exposed in a module, for later use in other
      modules, c.f., {{!Image}Image} module defines three constructors:
      - [Image.symbol] for Image symbols, that basically can be seen as
      [Image.Symbol of sym]
      - [Image.section] for image sections;
      - [Image.region] for other named image memory regions. *)
  module Tag : sig

    (** Tag constructor of type ['a]  *)
    type 'a t   with sexp_of

    (** a value injected into extensible variant  *)
    type value with sexp_of

    (** [register name sexp] creates a new variant constructor, i.e.,
        a new branch in a variant type. This function has no side-effects,
        it just returns a witness of a new constructor, that can be later
        used for storing into [value] and extracting from it. This
        witness should be shared between user and creator of the value *)
    val register : string -> ('a -> Sexp.t) -> 'a t

    (** [create cons x] creates a value using constructor [cons] and
        argument [x] *)
    val create : 'a t -> 'a -> value

    (** [value cons] extracts a value associated with a constructor [cons]
        (Essentially, performs a pattern match on the specified variant
        branch) *)
    val value : 'a t -> value -> 'a option

    (** [is cons v] true if value [v] was constructed with constructor
        [cons] *)
    val is  : 'a t -> value -> bool

    (** [tagname value] returns a constructor name of the [value]  *)
    val tagname : value -> string

    (** [name cons] returns a name of a constructor.  *)
    val name : 'a t -> string

    include Printable with type t := value
  end

  (** Lazy sequence  *)
  module Seq : sig
    type 'a t = 'a Sequence.t
    include module type of Sequence with type 'a t := 'a t
    val of_array : 'a array -> 'a t

    val cons : 'a -> 'a t -> 'a t

    val is_empty : 'a t -> bool
  end

  (** type abbreviation for ['a Sequence.t]  *)
  type 'a seq = 'a Seq.t

  (** [x ^:: xs] is a consing operator for sequences  *)
  val (^::) : 'a -> 'a seq -> 'a seq

  (** Access to BAP configuration variables  *)
  module Config : sig
    val pkg_version : string
  end

  type bil   = Bil.t       with bin_io, compare, sexp
  type binop = Bil.binop   with bin_io, compare, sexp
  type cast  = Bil.cast    with bin_io, compare, sexp
  type exp   = Exp.t       with bin_io, compare, sexp
  type stmt  = Stmt.t      with bin_io, compare, sexp
  type unop  = Bil.unop    with bin_io, compare, sexp
  type value = Tag.value   with sexp_of
  type 'a tag = 'a Tag.t     with sexp_of


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
    val to_sequence : ('a t -> (mem * 'a) Sequence.t) ranged


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
    module Section : sig
      type t = {
        name: string;
        perm: perm;         (** section's permissions  *)
        off: int;
        location : location;
      } with bin_io, compare, fields, sexp
    end

    (** Symbol definition, that can span several non-contiguous parts of
        memory *)
    module Sym : sig
      type t = {
        name : string;
        is_function : bool;
        is_debug : bool;
        locations : location * location list;
      } with bin_io, compare, fields, sexp
    end

    (** Just a named region of memory. (Known as section in ELF)  *)
    module Region : sig
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
        sections : Section.t * Section.t list;
        symbols  : Sym.t list;
        regions  : Region.t list;
      } with bin_io, compare, fields, sexp
    end

    (** the actual interface to be implemented  *)
    type t = Bigstring.t -> Img.t option
  end


  (** Binary Image.  *)
  module Image : sig
    (** {2 Type definitions}  *)

    type t = image with sexp_of            (** image   *)

    (** section *)
    type sec with bin_io, compare, sexp
    (** symbol  *)
    type sym with bin_io, compare, sexp

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
    val sections : t -> sec table
    val symbols : t -> sym table

    (** {2 Tags}  *)
    val section : sec tag
    val symbol  : string tag
    val region  : string tag

    (** returns memory, annotated with tags  *)
    val memory : t -> value memmap

    (** {2 Mappings }  *)
    val memory_of_section  : t -> sec -> mem
    (** [memory_of_symbol sym]: returns the memory of symbol in acending order. *)
    val memory_of_symbol   : t -> sym -> mem * mem seq
    val symbols_of_section : t -> sec -> sym seq
    val section_of_symbol  : t -> sym -> sec

    (** Image Section.
        Section is a contiguous region of memory that has
        permissions. The same as segment in ELF.    *)
    module Sec : sig
      type t = sec
      include Regular with type t := t
      val name : t -> string
      val is_writable   : t -> bool
      val is_readable   : t -> bool
      val is_executable : t -> bool
    end

    (** Symbol.  *)
    module Sym : sig
      type t = sym
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
    val to_sequence : 'a t -> (mem * 'a) Sequence.t

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
      Will take executable sections of the image and disassemble it,
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

    (** Tags to annotate memory  *)
    val insn : insn tag
    val block : block tag
    val error : error tag
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
    method return_value : exp option

    (** [args] returns a list of expressions that represents
        arguments of the given function. Each expression can be
        annotated with suggested name  *)
    method args : (string option * exp) list

    (** [vars] returns a list of expressions, that represents
        local variables of the function  *)
    method vars : (string option * exp) list

    (** [records] returns a list of records, found in the symbol.  *)
    method records : (string option * exp) list list
  end

  (** symbol name may be provided if known. Also an access
      to the whole binary image is provided if there is one. *)
  type abi_constructor =
    ?image:image -> ?sym:string -> mem -> block -> abi


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

    (** Program counter  *)
    val pc  : var

    (** Stack pointer  *)
    val sp  : var

    (** {4 Flag registers}  *)
    val zf  : var
    val cf  : var
    val vf  : var
    val nf  : var

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
    val create :
      ?merge:(abi list -> abi) ->
      ?image:image ->
      ?sym:string -> mem -> block -> abi

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


    (** [dfs ?next ?bound blk] searches from the [blk] using DFS.

        Search can be bound with [bound], i.e., the search will not continue
        on block that has no intersections with the specified bound. By
        default the search is unbound. Search direction can be also
        specified by the [next] parameter. By default search is performed
        in a forward direction, using [succs] function. To search in a
        reverse direction, use [preds] function. The search result is
        returned as a lazy sequence. Destinations of the block are visited
        in the order of execution, e.g., taken branch is visited before
        non taken. No such guarantee is made for the predecessors or any
        other function provided as a [next] arguments, since it is a
        property of a [next] function, not the algorithm itsef.
    *)
    val dfs :
      ?order:[`post | `pre] ->        (** defaults to pre *)
      ?next:(t -> t seq) ->           (** defaults to succs  *)
      ?bound:mem -> t -> t seq


    (** A classic control flow graph using OCamlgraph library.
        Graph vertices are made abstract, but the implement
        [Block_accessors] interface, including hash tables, maps, hash
        sets etc. *)
    module Cfg : sig
      module Block : sig
        type t with sexp_of
        include Block_accessors with type t := t and type insn := insn
      end

      (** Imperative graph *)
      module Imperative : Graph.Sig.I
        with type V.t = Block.t
         and type V.label = Block.t
         and type E.t = Block.t * edge * Block.t
         and type E.label = edge

      (** The default graph is persistant  *)
      include Graph.Sig.P
        with type V.t = Block.t
         and type V.label = Block.t
         and type E.t = Block.t * edge * Block.t
         and type E.label = edge

    end

    (** [to_graph ?bound entry] builds a graph starting with [entry] and
        spanning all reachable blocks that are bounded by a memory region
        [bound].
        @param bound defaults to infinite memory region.
    *)
    val to_graph : ?bound:mem -> t -> Cfg.Block.t * Cfg.t
    val to_imperative_graph :
      ?bound:mem -> t -> Cfg.Block.t * Cfg.Imperative.t
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

  (** [target_of_arch arch] creates a module for the given [arch], if
      [arch] is not lifted, the stub module is returned. *)
  val target_of_arch : arch -> (module Target)


  (** ARM architecture. *)
  module ARM  : sig

    val lift : mem -> ('a,'k) Disasm_expert.Basic.insn -> bil Or_error.t

    module ABI : sig
      include ABI

      (** [gnueabi] ABI  *)
      class gnueabi :
        ?image:image ->
        ?sym:string -> mem -> block -> abi

    end

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

  (** @deprecated will be removed soon  *)
  module Symtab : sig
    type t = string table

    (** [create roots base cfg]
        creates a symbol table from [cfg]. If no roots are
        provided, then only calls
    *)
    val create : addr list -> mem -> block table -> t

    module Graph : Graph.Sig.P
      with type V.label = mem * string (** function body *)
       and type E.label = addr         (** callsite  *)


    (** [to_graph blocks syms] creates a callgraph. Edges of the graph
        are labeled with a callsite *)
    val to_graph : block table -> t -> Graph.t
  end

  (** Target of analysis.  *)
  module Project : sig
    (** A project groups together all the information recovered from
        the underlying binary information. It is also used for
        interaction between plugins.  *)

    type t = {
      arch    : arch;               (** architecture  *)
      disasm  : disasm;             (** disassembly of a program *)
      memory  : value memmap;       (** annotations  *)
      storage : value String.Map.t; (** arbitrary data storage *)

      (** Deprecated fields, the will be removed in a next release. *)
      symbols : string table;       (** @deprecated symbol table  *)
      base    : mem;                (** @deprecated base memory  *)
    }

    type color = [
      | `black
      | `red
      | `green
      | `yellow
      | `blue
      | `magenta
      | `cyan
      | `white
    ] with sexp

    (** all string tags attached to a memory region supports the
        following substitutions:

        - [$region_{name,addr,min_addr,max_addr}] - name of region of file
        to which it belongs. For example, in ELF this name will
        correspond to the section name

        - [$symbol_{name,addr}] - name or address of the symbol to which this
        memory belongs

        - [$asm] - assembler listing of the memory region

        - [$bil] - BIL code of the tagged memory region

        - [$block_{name,addr}] - name or address of a basic block to which
        this region belongs

        - [$min_addr, $addr] - starting address of a memory region

        - [$max_addr] - address of the last byte of a memory region.
    *)


    (** an arbitrary text *)
    val text : string tag

    (** the associated data is an html markup *)
    val html : string tag

    (** associate a comment string with a memory region  *)
    val comment : string tag

    (** to assosiate a python command with a region *)
    val python : string tag

    (** to assosiate a shell command with a region  *)
    val shell : string tag

    (** just mark a region  *)
    val mark : unit tag

    (** attach a color  *)
    val color : color tag

    (** attach a weight to a memory  *)
    val weight : float tag

    (** [register plugin] registers [plugin] in the system  *)
    val register_plugin : (t -> t) -> unit

    (** [register' plugin] registers a [plugin] that will be
        evaluated only for side effect. *)
    val register_plugin': (t -> unit) -> unit

    val register_plugin_with_args : (string array -> t -> t) -> unit

    val register_plugin_with_args' : (string array -> t -> unit) -> unit

    (** A list of registered plugins in the order of registration  *)
    val plugins : unit -> (string array -> t -> t) list
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
      val functions : t -> (string * fn) Sequence.t
    end

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
