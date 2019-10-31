(** BAP Semantics Representation.


    {1 A Gentle Introduction to the Core Theory}

    The Core Theory is an intermediate language that is designed to
    express the semantics of computer programs. It focuses on programs
    that are represented in binary machine code and is capable of an
    accurate representation of the architectural and micro-architectural
    details of the program behavior.

    The language is extensible. It is possible to add new language
    features, without affecting existing analyses and even
    without recompilation. Adding new analyses also doesn't require
    any changes to the existing code, which also could be reused
    without recompilation. Thus the language doesn't suffer from the
    expression problem.

    The language is rigidly typed with types expressed as OCaml
    types. A type of a Core Theory term is inferred (and checked) by
    the OCaml type system, which can statically ascertain that the
    term is not ill-formed and no analysis will get stuck.

    The language is adaptable. Analysts can select a designated
    subset of the language which is relevant to their tasks without
    getting bogged down by the irrelevant architectural details.

    The language can express the semantics of the floating point
    operations including operations, but not limiting to, specified in
    the IEEE754 standard.

    The language facilitates developing custom intermediate
    representation and languages, which could be seamlessly introduced
    in the analysis pipeline without breaking existing components. The
    new language is compatible with BIL, BIR, and older variants of
    BIL. It is potentially compatible with any other intermediate
    representations.


    {2 The Language Hierarchy}

    The Core Theory is not really a language but a family of
    languages. If we will order the languages in this family by
    subsumption, then we will get the following Hasse diagram:

    {v

                             o Core
                             |
              Trans o--------+--------o Float
                             |        |
                             o Basic  o FBasic
                             |
                             o Minimal
                             |
            +-------+--------+--------+-------+
            |       |        |        |       |
            o       o        o        o       o
          Init    Bool     Bitv    Memory   Effect

    v}

    The Core language subsumes all other sub-languages and includes
    modular arithmetic and other operations on bitvectos, operations
    with memories, registers, floating points including
    transcendental functions.

    The reason to have so many languages is purely pragmatic: to
    enable specialized implementations of analyses and lifters. This
    structure is not really mandated, the languages are defined
    structurally, not nominally, so it is possible to combine
    languages in arbitrary ways, as well as define new languages.

    The Core language, despite being at the top of our hierarchy, is
    still very low-level and basic. It is intended to reflect
    operations carried by classical computers with Harvard or
    Princeton architectures. Therefore we chose the name "Core" to
    reflect our vision of the Core language as the base for
    higher-level hierarchies of languages.


    {2 Hierarchy of Terms}

    Terms and operations of the Core Theory languages are typed to
    prevent the creation of ill-formed programs. We use the word sort
    to denote a set of terms that share the same properties. The Core
    Theory comes with a collection of predefined sorts, which are used
    to specify the Core language, but it is possible to define new
    sorts, to keep the theory extensible.

    The terms of the Core Theory are divided in two classes - values
    and effects. A value term denotes the semantics of programs that
    produce values. Roughly, values correspond to the syntactic class of
    language expressions.

    Effects is a class of terms that do not produce values, but side
    effects or just effects, e.g., changing a value of a register,
    loading or storing memory location, performing I/O operation on a
    port or issuing a synchronization barrier, etc. Roughly, effects
    correspond to the syntactic class of language statements.

    Both values and effects are knowledge classes. Each class is
    further subdivided into an infinite set of sorts. The class of
    values is inhabited with terms of Bool, Bitv, Mem, Float, and
    Rmode. Some sorts are indexed, so they represent an (infinite)
    family of sorts. For example, Bitv[s] is a family of bitvector sorts
    indexed by their widths, e.g, Bitv[8], Bitv[32], etc.

    The class of effects is subdivided into two sorts of effect, those
    that affect the control flow and those that affect the data flow.


    {v
                              Term
                                o
                                |
                         +------+------+
                         |             |
                   Value o             o Effect
                         |             |
                 Bool o--+         +---+---+
                         |         |       |
              Bitv[s] o--+         o       o
                         |      Ctrl[i]  Data[i]
             Mem[k,s] o--+
                         |
           Float[f,s] o--+
                         |
                Rmode o--+


    v}


    {2 Variables}

    Variables are ubiquitous in programming languages and could be
    used to reference memory locations, CPU registers, or just
    be bound to expressions. Sometimes variables are typed, sometimes
    they are just identifiers with not associated type.

    In the Core Theory all variables are sorted, i.e., the have an
    associated value sort. Variables are also having scope and
    extent. Finally, variables could be mutable or immutable.

    A physical variable is a global mutable variable with infinite
    scope and extent. They are used to refer predefined (micro)
    architectural locations of a modeled system, e.g., registers,
    memory banks, caches, register files, etc. Global variables has
    identifiers that are the same as names, e.g., `RAX`, `R0`, `mem`,
    etc. The important thing, is that a global variable usually has
    some physical representation.

    Virtual variables are dual to physical variables and are further
    subdivided into mutable and immutable.

    A mutable virtual variable represents an unspecified scratch
    location that holds data of the specified sort. They could be used
    to abstract an actual physical location in a modeled system (when
    it is not relevant or just not known) or just to simplify the
    analysis. The mutable virtual variables have identifier of the
    form [#<number>], e.g, [#1], [#2048], etc.

    Finally, an immutable virtual variable is a local variable that
    holds a value of an expression. It has a limited scope and its
    immutability is ensured by the type system since the scope of a
    local binding can contain only pure terms, i.e., no
    side-effects. These variables have identifiers of the form
    [$<number>], e.g., [$1], [$2], etc, and since their scope is
    limited, those identifiers are reused in different scopes.


    {2 Theories and Semantics}

    Languages of the Core Theory, including the Core itself are
    represented as module signatures, i.e., they are pure abstractions
    or interfaces that do not define any data types, functions, or values.

    This approach is called tagless-final style [1], pioneered by
    Carette and Kiselyov and later rediscovered under the name "Object
    algebras" by Oliviera and Cook [2]. We encourage to read those
    papers and accompanying literature, but it is not strictly needed,
    especially since the underlying idea is pretty simple.

    In the final style, an embedded language is not represented by an
    abstract syntax tree or some intermediate representation data
    structure, but by denotations in a semantic algebra. Where the
    semantic algebra is an implementation (structure) of the language
    signature. Or, in other words, it is its denotational semantics.

    The structure may choose, basically, any denotations, as long as
    it fits the signature. For example, the language could be denoted
    with its textual representation, BIL code, LLVM IR, BIR, sets of
    reachable addresses, and so on. In other words, an implementation
    of the Core signature could be seen as an analysis that computes the
    property of a term.

    Unlike a classical final approach described in [1] the Core Theory
    signatures do not include any abstract types, all types mentioned
    in the theories are defined on the scope of the Core Theory. This
    constraint basically turns a structure, which implements the
    Theory, into a simple array of functions, a first class value with
    no caveats (like types escaping the scope, etc).

    To enable each semantic algebra to have its own denotation we
    employ ['a Knowledge.Value.t]. Thus both Core Theory values and
    effects are instances of the Knowledge Value, parametrized by
    corresponding class indices. The Knowledge Value is an instance of
    Domain, that makes it ideal for representing the denotational
    semantics of programs. The Knowledge value is an extensible
    record, where each field corresponds to a particular denotation,
    it is possible to store several denotations in one Knowledge
    value.

    Denotational semantics is composable, i.e., a denotation of
    a term is composed from denotations of its constituent terms.
    However, some denotations are context dependent. To enable
    this, we made a term denotation an instance of ['a knowledge],
    i.e., a knowledge dependent computation.

    To summarize, a denotation is a structure that implements methods
    of the corresponding structure. Each method corresponds to a
    language form, e.g, [val add  : 'a bitv -> 'a bitv -> 'a bitv],
    corresponds to an addition of two bitvectors. The implementation,
    builds the denotation of the term from the denotations of its
    inputs, e.g.,

    {[
      let add x y =
        x >>-> fun x ->
        y >>-> fun y ->
        match x, y with
        | Some x, Some y -> const (Bitvec.(x + y))
        | _ -> nonconst
    ]}

    Where [>>->] extracts the module specific denotation and [const],
    [noncost] put them back (assuming that the denotation is the
    classical constant folding lattice).

    The final style makes it easy to write a fold-style analysis, such
    as constant folding, taint analysis, etc. Since all terms are
    knowledge dependent computations, i.e., wrapped into the knowledge
    monad, which turns any computation into a fixed-point computation,
    it is also possible to write data-flow analysis and other forms of
    abstract interpretation. In fact, it was shown, that any
    optimization or analysis could be written in the final style in a
    modular and composable way [3]. However, the classical approach,
    that uses tagged AST and pattern matching is not denied at all.

    Since the denotation could be anything (that is an instance of
    domain), it is quite natural to use BIL, and BIR, or any other
    concrete syntax tree as a possible denotation. Therefore, it is
    possible to extract those denotations and write your analysis
    using the save haven of pattern matching.


    {2 Writing a new denotation}

    Any denotation must be an instance of the Core signature. However,
    it is not always required to implement all methods, as they could
    be inherited from other instance or filled in with the Empty
    Theory. Once analysis is written it should be declared, so that it
    could be later run, e.g., let's extend a hypothetical
    ["constant-tracker"] analysis:

    {[
      let () = Theory.declare "my-constant-tracker" (module struct
          include (val Theory.require "constant-tracker")

          (* probably a bad example, but we all do this :) *)
          let add x y =
            printf "add is called!\n%!";
            add x y
        end
    ]}


    The real analysis should store it results either in the knowledge
    base or directly in denotations of the terms (or in both places).


    {2 Instantiating a theory}

    To use a theory we need to instantiate it. In the previous section
    we instantiated a theory using the [Theory.require] function, that
    returns a previously declared theory. But what if we need to use
    several denotations, e.g., when we want to have both a
    constant-tracker and BIL for our analysis.

    The final style implementations, in Scala, OCaml, and Haskell,
    usually employ functors or type classes, which both require a user
    to specify an instance of a type class, which should be used in
    the given context. Some languages allow only one instance of class
    per type, other allow multiple, but still needs a declaration of
    some instances as canonical.

    In Core Theory we do not need to select any particular instance,
    but instead it is possible to have multiple instances of the Core
    theory running in parallel. The [Theory.instance ()] is a special
    instance of Core that runs all declared theories and combines their
    denotations. Since Knowledge value is a finite bounded domain, it
    is a well-defined process.

    Running all analyses in parallel might be an overkill, especially
    if the results of some analyses won't be used at all. To narrow
    down the set of denotations, it is possible to specify a context
    or explicitly request analyses that provide certain features.

    For example,

    {[
      module Core = (val Theory.instance ()
                        ~context:[""]
                        ~requires
                    )
    ]}





    The [Theory.instance ()] function also accepts parameters that
    enable cont



    [1]: http://okmij.org/ftp/tagless-final/JFP.pdf
    [2]: http://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf
    [3]: http://okmij.org/ftp/tagless-final/course/optimizations.html


*)

open Core_kernel
open Caml.Format
open Bap_knowledge

module KB = Knowledge

module Theory : sig
  module Value : sig
    type +'a sort
    type cls

    type 'a t = (cls,'a sort) KB.cls KB.value
    val cls : (cls,unit) KB.cls

    val empty : 'a sort -> 'a t
    val sort : 'a t -> 'a sort

    module Sort : sig
      type +'a t = 'a sort
      type +'a sym
      type +'a num
      type name

      val sym : name -> 'a sym sort
      val int : int -> 'a num sort
      val app : 'a sort -> 'b sort -> ('a -> 'b) sort
      val (@->) : 'a sort -> 'b sort -> ('a -> 'b) sort

      val value : 'a num sort -> int
      val name :  'a sym sort -> name

      val hd : ('a -> 'b) sort -> 'a sort
      val tl : ('a -> 'b) sort -> 'b sort

      val refine : name -> unit sort -> 'a t option

      val forget : 'a t -> unit t

      val same : 'a t -> 'b t -> bool

      val pp : formatter -> 'a t -> unit

      module Top : sig
        type t = unit sort [@@deriving bin_io, compare, sexp]
        include Base.Comparable.S with type t := t
      end

      module Name : sig
        type t
        val declare : ?package:string -> string -> name
        include Base.Comparable.S with type t := t
      end
    end
  end

  module Effect : sig
    type +'a sort
    type cls

    type 'a t = (cls,'a sort) KB.cls KB.value
    val cls : (cls,unit) KB.cls

    val empty : 'a sort -> 'a t
    val sort : 'a t -> 'a sort

    module Sort : sig
      type +'a t = 'a sort
      type data = private Data
      type ctrl = private Ctrl

      val data : string -> data t
      val ctrl : string -> ctrl t

      val top : unit t
      val bot : 'a t

      val both : 'a t -> 'a t -> 'a t
      val (&&) : 'a t -> 'a t -> 'a t
      val union : 'a t list -> 'a t
      val join : 'a t list -> 'b t list -> unit t

      val order : 'a t -> 'b t -> KB.Order.partial


      val rreg : data t
      val wreg : data t
      val rmem : data t
      val wmem : data t
      val barr : data t
      val fall : ctrl t
      val jump : ctrl t
      val cjmp : ctrl t
    end
  end

  type 'a value = 'a Value.t
  type 'a effect = 'a Effect.t

  module Bool : sig
    type t
    val t : t Value.sort
    val refine : unit Value.sort -> t Value.sort option
  end


  module Bitv : sig
    type 'a t
    val define : int -> 'a t Value.sort
    val refine : unit Value.sort -> 'a t Value.sort option
    val size : 'a t Value.sort -> int
  end

  module Mem : sig
    type ('a,'b) t
    val define : 'a Bitv.t Value.sort -> 'b Bitv.t Value.sort -> ('a,'b) t Value.sort
    val refine : unit Value.sort -> ('a,'b) t Value.sort option
    val keys : ('a,'b) t Value.sort -> 'a Bitv.t Value.sort
    val vals : ('a,'b) t Value.sort -> 'b Bitv.t Value.sort
  end

  module Float : sig
    module Format : sig
      type ('r,'s) t
      val define : 'r Value.sort -> 's Bitv.t Value.sort -> ('r,'s) t Value.sort
      val bits : ('r,'s) t Value.sort -> 's Bitv.t Value.sort
      val exp : ('r,'s) t Value.sort -> 'r Value.sort
    end

    type ('r,'s) format = ('r,'s) Format.t
    type 'f t

    val define : ('r,'s) format Value.sort -> ('r,'s) format t Value.sort
    val refine : unit Value.sort -> ('r,'s) format t Value.sort option
    val format : ('r,'s) format t Value.sort -> ('r,'s) format Value.sort
    val size : ('r,'s) format t Value.sort -> 's Bitv.t Value.sort
  end

  module Rmode : sig
    type t
    val t : t Value.sort
    val refine : unit Value.sort -> t Value.sort option
  end

  type 'a pure = 'a value knowledge
  type 'a eff = 'a effect knowledge

  type ('r,'s) format = ('r,'s) Float.format

  module Var : sig
    type 'a t
    type ident [@@deriving bin_io, compare, sexp]
    type ord

    val define : 'a Value.sort -> string -> 'a t
    val create : 'a Value.sort -> ident -> 'a t
    val forget : 'a t -> unit t
    val resort : 'a t -> 'b Value.sort -> 'b t

    val versioned: 'a t -> int -> 'a t
    val version : 'a t -> int

    val ident : 'a t -> ident
    val name : 'a t -> string
    val sort : 'a t -> 'a Value.sort
    val is_virtual : 'a t -> bool
    val is_mutable : 'a t -> bool
    val fresh : 'a Value.sort -> 'a t knowledge
    val scoped : 'a Value.sort -> ('a t -> 'b pure) -> 'b pure

    module Ident : sig
      type t = ident [@@deriving bin_io, compare, sexp]
      include Stringable.S with type t := t
      include Base.Comparable.S with type t := t
                                 and type comparator_witness = ord
    end

    module Top : sig
      type nonrec t = unit t [@@deriving bin_io, compare, sexp]
      include Base.Comparable.S with type t := t
    end
  end

  type data = Effect.Sort.data
  type ctrl = Effect.Sort.ctrl

  type word = Bitvec.t
  type 'a var = 'a Var.t

  type program
  type label = program KB.Object.t

  module Program : sig
    type t = (program,unit) KB.cls KB.value
    val cls : (program,unit) KB.cls
    module Semantics : sig
      type cls = Effect.cls
      type t = unit Effect.t
      val cls : (cls, unit Effect.sort) Knowledge.cls
      val slot : (program, t) Knowledge.slot
      include Knowledge.Value.S with type t := t
    end
    include Knowledge.Value.S with type t := t
  end

  module Label : sig
    type t = label

    val addr : (program, Bitvec.t option) KB.slot
    val name : (program, string option) KB.slot
    val ivec : (program, int option) KB.slot
    val aliases : (program, Set.M(String).t) KB.slot

    val is_valid : (program, bool option) KB.slot
    val is_subroutine : (program, bool option) KB.slot

    val for_addr : Bitvec.t -> t knowledge
    val for_name : string -> t knowledge
    val for_ivec : int -> t knowledge

    include Knowledge.Object.S with type t := t
  end


  type bool = Bool.t pure
  type 'a bitv = 'a Bitv.t pure
  type ('a,'b) mem = ('a,'b) Mem.t pure
  type 'f float = 'f Float.t pure
  type rmode = Rmode.t pure

  module type Init = sig
    val var : 'a var -> 'a pure
    val unk : 'a Value.sort -> 'a pure
    val let_ : 'a var -> 'a pure -> 'b pure -> 'b pure
  end

  module type Bool = sig
    val b0 : bool
    val b1 : bool
    val inv : bool -> bool
    val and_ : bool -> bool -> bool
    val or_ : bool -> bool -> bool
  end

  module type Bitv = sig
    val int : 'a Bitv.t Value.sort -> word -> 'a bitv
    val msb : 'a bitv -> bool
    val lsb : 'a bitv -> bool
    val neg  : 'a bitv -> 'a bitv
    val not    : 'a bitv -> 'a bitv
    val add  : 'a bitv -> 'a bitv -> 'a bitv
    val sub  : 'a bitv -> 'a bitv -> 'a bitv
    val mul  : 'a bitv -> 'a bitv -> 'a bitv
    val div  : 'a bitv -> 'a bitv -> 'a bitv
    val sdiv : 'a bitv -> 'a bitv -> 'a bitv
    val modulo : 'a bitv -> 'a bitv -> 'a bitv
    val smodulo : 'a bitv -> 'a bitv -> 'a bitv
    val logand : 'a bitv -> 'a bitv -> 'a bitv
    val logor  : 'a bitv -> 'a bitv -> 'a bitv
    val logxor  : 'a bitv -> 'a bitv -> 'a bitv
    val shiftr : bool -> 'a bitv -> 'b bitv -> 'a bitv
    val shiftl : bool -> 'a bitv -> 'b bitv -> 'a bitv
    val ite : bool -> 'a pure -> 'a pure -> 'a pure
    val sle : 'a bitv -> 'a bitv -> bool
    val ule : 'a bitv -> 'a bitv -> bool
    val cast : 'a Bitv.t Value.sort -> bool -> 'b bitv -> 'a bitv
    val concat : 'a Bitv.t Value.sort -> 'b bitv list -> 'a bitv
    val append : 'a Bitv.t Value.sort -> 'b bitv -> 'c bitv -> 'a bitv
  end

  module type Memory = sig
    val load : ('a,'b) mem -> 'a bitv -> 'b bitv
    val store : ('a,'b) mem -> 'a bitv -> 'b bitv -> ('a,'b) mem
  end

  module type Effect = sig
    val perform : 'a Effect.sort -> 'a eff
    val set : 'a var -> 'a pure -> data eff
    val jmp  : _ bitv -> ctrl eff
    val goto : label -> ctrl eff
    val seq : 'a eff -> 'a eff -> 'a eff
    val blk : label -> data eff -> ctrl eff -> unit eff
    val repeat : bool -> data eff -> data eff
    val branch : bool -> 'a eff -> 'a eff -> 'a eff
  end


  module type Minimal = sig
    include Init
    include Bool
    include Bitv
    include Memory
    include Effect
  end

  module type Basic = sig
    include Minimal
    val zero : 'a Bitv.t Value.sort -> 'a bitv
    val is_zero  : 'a bitv -> bool
    val non_zero : 'a bitv -> bool
    val succ : 'a bitv -> 'a bitv
    val pred : 'a bitv -> 'a bitv
    val nsucc : 'a bitv -> int -> 'a bitv
    val npred : 'a bitv -> int -> 'a bitv
    val high : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv
    val low  : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv
    val signed : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv
    val unsigned  : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv
    val extract : 'a Bitv.t Value.sort -> 'b bitv -> 'b bitv -> _ bitv -> 'a bitv
    val loadw : 'c Bitv.t Value.sort -> bool -> ('a, _) mem -> 'a bitv -> 'c bitv
    val storew : bool -> ('a, 'b) mem -> 'a bitv -> 'c bitv -> ('a, 'b) mem
    val arshift : 'a bitv -> 'b bitv -> 'a bitv
    val rshift : 'a bitv -> 'b bitv -> 'a bitv
    val lshift : 'a bitv -> 'b bitv -> 'a bitv
    val eq  : 'a bitv -> 'a bitv -> bool
    val neq : 'a bitv -> 'a bitv -> bool
    val slt : 'a bitv -> 'a bitv -> bool
    val ult : 'a bitv -> 'a bitv -> bool
    val sgt : 'a bitv -> 'a bitv -> bool
    val ugt : 'a bitv -> 'a bitv -> bool
    val sge : 'a bitv -> 'a bitv -> bool
    val uge : 'a bitv -> 'a bitv -> bool
  end

  module type Fbasic = sig
    val float : ('r,'s) format Float.t Value.sort -> 's bitv -> ('r,'s) format float
    val fbits : ('r,'s) format float -> 's bitv


    val is_finite : 'f float -> bool
    val is_nan : 'f float -> bool
    val is_inf : 'f float -> bool
    val is_fzero : 'f float -> bool
    val is_fpos : 'f float -> bool
    val is_fneg : 'f float -> bool

    val rne : rmode
    val rna : rmode
    val rtp : rmode
    val rtn : rmode
    val rtz : rmode
    val requal : rmode -> rmode -> bool

    val cast_float  : 'f Float.t Value.sort  -> rmode -> 'a bitv -> 'f float
    val cast_sfloat : 'f Float.t Value.sort -> rmode -> 'a bitv -> 'f float
    val cast_int    : 'a Bitv.t Value.sort -> rmode -> 'f float -> 'a bitv
    val cast_sint   : 'a Bitv.t Value.sort -> rmode -> 'f float -> 'a bitv

    val fneg    : 'f float -> 'f float
    val fabs    : 'f float -> 'f float

    val fadd    : rmode -> 'f float -> 'f float -> 'f float
    val fsub    : rmode -> 'f float -> 'f float -> 'f float
    val fmul    : rmode -> 'f float -> 'f float -> 'f float
    val fdiv    : rmode -> 'f float -> 'f float -> 'f float
    val fsqrt   : rmode -> 'f float -> 'f float
    val fmodulo : rmode -> 'f float -> 'f float -> 'f float
    val fmad    : rmode -> 'f float -> 'f float -> 'f float -> 'f float

    val fround   : rmode -> 'f float -> 'f float
    val fconvert : 'f Float.t Value.sort ->  rmode -> _ float -> 'f float

    val fsucc  : 'f float -> 'f float
    val fpred  : 'f float -> 'f float
    val forder : 'f float -> 'f float -> bool
  end

  module type Float = sig
    include Fbasic
    val pow      : rmode -> 'f float -> 'f float -> 'f float
    val powr     : rmode -> 'f float -> 'f float -> 'f float
    val compound : rmode -> 'f float -> 'a bitv -> 'f float
    val rootn    : rmode -> 'f float -> 'a bitv -> 'f float
    val pownn    : rmode -> 'f float -> 'a bitv -> 'f float
    val rsqrt    : rmode -> 'f float -> 'f float
    val hypot    : rmode -> 'f float -> 'f float -> 'f float
  end

  module type Trans = sig
    val exp      : rmode -> 'f float -> 'f float
    val expm1    : rmode -> 'f float -> 'f float
    val exp2     : rmode -> 'f float -> 'f float
    val exp2m1   : rmode -> 'f float -> 'f float
    val exp10    : rmode -> 'f float -> 'f float
    val exp10m1  : rmode -> 'f float -> 'f float
    val log      : rmode -> 'f float -> 'f float
    val log2     : rmode -> 'f float -> 'f float
    val log10    : rmode -> 'f float -> 'f float
    val logp1    : rmode -> 'f float -> 'f float
    val log2p1   : rmode -> 'f float -> 'f float
    val log10p1  : rmode -> 'f float -> 'f float
    val sin      : rmode -> 'f float -> 'f float
    val cos      : rmode -> 'f float -> 'f float
    val tan      : rmode -> 'f float -> 'f float
    val sinpi    : rmode -> 'f float -> 'f float
    val cospi    : rmode -> 'f float -> 'f float
    val atanpi   : rmode -> 'f float -> 'f float
    val atan2pi  : rmode -> 'f float -> 'f float -> 'f float
    val asin     : rmode -> 'f float -> 'f float
    val acos     : rmode -> 'f float -> 'f float
    val atan     : rmode -> 'f float -> 'f float
    val atan2    : rmode -> 'f float -> 'f float -> 'f float
    val sinh     : rmode -> 'f float -> 'f float
    val cosh     : rmode -> 'f float -> 'f float
    val tanh     : rmode -> 'f float -> 'f float
    val asinh    : rmode -> 'f float -> 'f float
    val acosh    : rmode -> 'f float -> 'f float
    val atanh    : rmode -> 'f float -> 'f float
  end


  module type Core = sig
    include Basic
    include Float
    include Trans
  end

  module Basic : sig
    module Make(S : Minimal) : Basic
    module Empty : Basic
  end

  module Core : sig
    module Empty : Core
  end


  (** [instance ()] creates an instance of the Core Theory.

      The theory is built from all instances that match the context
      specified by the [features] list and provide features specified
      by the [requires] list.

      If no instances satisfy this requirement than the empty theory
      is returned.

      @param features is a set of features that define the context,
      the more features specified, the more specific the context.

      @param requires is a set of semantic features that are
      required. Defaults to the set of all possible features, i.e., if
      unspecified, then all instances applicable to the context will
      be loaded.
  *)
  val instance :
    ?features:string list ->
    ?requires:string list ->
    unit -> (module Core)


  (** [declare name s] structure [s] as an instance of the Core Theory.
  *)
  val declare :
    ?desc:string ->
    ?requires:string list ->
    ?provides:string list ->
    ?package:string ->
    string -> (module Core) -> unit



  (** [require ?package name] looks a [package:name] theory in the context.

      @raise Invalid_arg if no such theory exists.
  *)
  val require : ?package:string -> string -> (module Core)

  module IEEE754 : sig
    type ('a,'e,'t) t
    type ('a,'e,'t) ieee754 = ('a,'e,'t) t
    (*  see IEEE754 3.6 *)
    type parameters = private {
      base : int;
      bias : int;
      k : int;
      p : int;
      w : int;
      t : int;
    }


    val binary16 : parameters
    val binary32 : parameters
    val binary64 : parameters
    val binary80 : parameters
    val binary128 : parameters
    val decimal32 : parameters
    val decimal64 : parameters
    val decimal128 : parameters

    val binary : int -> parameters option
    val decimal : int -> parameters option

    module Sort : sig
      val define : parameters -> (('b,'e,'t) ieee754,'s) format Float.t Value.sort
      val exps : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 'e Bitv.t Value.sort
      val sigs : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 't Bitv.t Value.sort
      val bits : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 's Bitv.t Value.sort
      val spec : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> parameters
    end
  end


  module Grammar : sig
    type ieee754 = IEEE754.parameters
    module type Bitv = sig
      type t
      type exp
      type rmode

      val error : t

      val unsigned : int -> exp -> t
      val signed : int -> exp -> t
      val high : int -> exp -> t
      val low : int -> exp -> t
      val cast : int -> exp -> exp -> t
      val extract : int -> exp -> exp -> exp -> t

      val add : exp -> exp -> t
      val sub : exp -> exp -> t
      val mul : exp -> exp -> t
      val div : exp -> exp -> t
      val sdiv : exp -> exp -> t
      val modulo : exp -> exp -> t
      val smodulo : exp -> exp -> t
      val lshift : exp -> exp -> t
      val rshift : exp -> exp -> t
      val arshift : exp -> exp -> t
      val logand : exp -> exp -> t
      val logor: exp -> exp -> t
      val logxor : exp -> exp -> t

      val neg : exp -> t
      val not : exp -> t

      val load_word : int -> exp -> exp -> exp -> t
      val load : exp -> exp -> t


      val var : string -> int -> t
      val int : word -> int -> t
      val unknown : int -> t
      val ite : exp -> exp -> exp -> t

      val let_bit : string -> exp -> exp -> t
      val let_reg : string -> exp -> exp -> t
      val let_mem : string -> exp -> exp -> t
      val let_float : string -> exp -> exp -> t

      val append : exp -> exp -> t
      val concat : exp list -> t

      val cast_int : int -> rmode -> exp -> t
      val cast_sint : int -> rmode -> exp -> t
      val fbits : exp -> t
    end

    module type Bool = sig
      type t
      type exp

      val error : t

      val eq : exp -> exp -> t
      val neq : exp -> exp -> t
      val lt : exp -> exp -> t
      val le : exp -> exp -> t
      val slt : exp -> exp -> t
      val sle : exp -> exp -> t
      val var : string -> t
      val int : word -> t
      val unknown : unit -> t
      val ite : exp -> exp -> exp -> t
      val let_bit : string -> exp -> exp -> t
      val let_reg : string -> exp -> exp -> t
      val let_mem : string -> exp -> exp -> t
      val let_float : string -> exp -> exp -> t

      val high : exp -> t
      val low : exp -> t
      val extract : int -> exp -> t

      val not : exp -> t
      val logand : exp -> exp -> t
      val logor: exp -> exp -> t
      val logxor : exp -> exp -> t

      val is_inf : exp -> t
      val is_nan : exp -> t
      val is_fzero : exp -> t
      val is_fpos : exp -> t
      val is_fneg : exp -> t

      val fle  : exp -> exp -> t
      val flt  : exp -> exp -> t
      val feq  : exp -> exp -> t
    end


    module type Mem = sig
      type t
      type exp

      val error : t

      (** [store mem key data] *)
      val store : exp -> exp -> exp -> t


      (** [store_word dir mem key data ]  *)
      val store_word : exp -> exp -> exp -> exp -> t
      val var : string -> int -> int -> t
      val unknown : int -> int -> t
      val ite : exp -> exp -> exp -> t
      val let_bit : string -> exp -> exp -> t
      val let_reg : string -> exp -> exp -> t
      val let_mem : string -> exp -> exp -> t
      val let_float : string -> exp -> exp -> t
    end

    module type Stmt = sig
      type t
      type exp
      type rmode
      type stmt

      val error : t

      val set_mem : string -> int -> int -> exp -> t
      val set_reg : string -> int -> exp -> t
      val set_bit : string -> exp -> t
      val set_ieee754 : string -> ieee754 -> exp -> t
      val set_rmode : string -> rmode -> t

      val tmp_mem : string -> exp -> t
      val tmp_reg : string -> exp -> t
      val tmp_bit : string -> exp -> t
      val tmp_float : string -> exp -> t
      val tmp_rmode : string -> rmode -> t

      val let_mem : string -> exp -> stmt -> t
      val let_reg : string -> exp -> stmt -> t
      val let_bit : string -> exp -> stmt -> t
      val let_float : string -> exp -> stmt -> t
      val let_rmode : string -> rmode -> stmt -> t

      val jmp : exp -> t
      val goto :  word -> t
      val call : string -> t
      val special : string -> t
      val cpuexn : int -> t

      val while_ : exp -> stmt list -> t
      val if_ : exp -> stmt list -> stmt list -> t

      val seq : stmt list -> t
    end

    module type Float = sig
      type t
      type exp
      type rmode

      val error : t

      val ieee754 : ieee754 -> exp -> t
      val ieee754_var : ieee754 -> string -> t
      val ieee754_unk : ieee754 -> t
      val ieee754_cast : ieee754 -> rmode -> exp -> t
      val ieee754_cast_signed : ieee754 -> rmode -> exp -> t
      val ieee754_convert : ieee754 -> rmode -> exp -> t

      val ite : exp -> exp -> exp -> t

      val fadd : rmode -> exp -> exp -> t
      val fsub : rmode -> exp -> exp -> t
      val fmul : rmode -> exp -> exp -> t
      val fdiv : rmode -> exp -> exp -> t
      val frem : rmode -> exp -> exp -> t
      val fmin : exp -> exp -> t
      val fmax : exp -> exp -> t

      val fabs : exp -> t
      val fneg : exp -> t
      val fsqrt : rmode -> exp -> t
      val fround : rmode -> exp -> t

      val let_bit : string -> exp -> exp -> t
      val let_reg : string -> exp -> exp -> t
      val let_mem : string -> exp -> exp -> t
      val let_float : string -> exp -> exp -> t
    end

    module type Rmode = sig
      type t
      type exp

      val error : t

      val rne : t
      val rtz : t
      val rtp : t
      val rtn : t
      val rna : t
    end
  end

  module Parser : sig
    type ('a,'e,'r) bitv_parser =
      (module Grammar.Bitv with type t = 'a
                            and type exp = 'e
                            and type rmode = 'r) ->
      'e -> 'a

    type ('a,'e,'r) bool_parser =
      (module Grammar.Bool with type t = 'a
                            and type exp = 'e) ->
      'e -> 'a

    type ('a,'e) mem_parser =
      (module Grammar.Mem with type t = 'a
                           and type exp = 'e) ->
      'e -> 'a

    type ('a,'e,'r,'s) stmt_parser =
      (module Grammar.Stmt with type t = 'a
                            and type exp = 'e
                            and type stmt = 's
                            and type rmode = 'r) ->
      's -> 'a

    type ('a,'e,'r) float_parser =
      (module Grammar.Float with type t = 'a
                             and type exp = 'e
                             and type rmode = 'r) ->
      'e -> 'a

    type ('a,'e) rmode_parser =
      (module Grammar.Rmode with type t = 'a
                             and type exp = 'e) ->
      'e -> 'a

    type ('e,'r,'s) t = {
      bitv : 'a. ('a,'e,'r) bitv_parser;
      bool : 'a. ('a,'e,'r) bool_parser;
      mem  : 'a. ('a,'e) mem_parser;
      stmt : 'a. ('a,'e,'r,'s) stmt_parser;
      float : 'a . ('a,'e,'r) float_parser;
      rmode : 'a . ('a,'r) rmode_parser;
    }

    type ('e,'r,'s) parser = ('e,'r,'s) t

    module Make(S : Core) : sig
      val run : ('e,'r,'s) parser -> 's list -> unit eff
    end
  end
end
