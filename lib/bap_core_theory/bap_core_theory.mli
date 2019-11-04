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


    {2:vars Variables}

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
    to select an instance of a type class, which should be used in
    the given context. Some languages allow only one instance of a class
    per type, others allow multiple, but still needs a declaration of
    some instances as canonical.

    The Core Theory addresses this issue by leveraging the structure
    of the Knowledge universal values and instantiating all theory
    instances simultaneously, so that for each language term the sum
    of all denotations is provided. To exclude the overhead of
    evaluating denotations that might be unused, it is possible to
    limit the set of instantiated theories by specifying a concrete
    list of required theories. The requirements are specified in the
    form of semantic tags instead of concrete theory names, to prevent
    explicit dependencies on implementations. However, it is still
    possible to explicitly request a particular theory.

    It is also possible to define the context of the theory, to enable
    those theories that are not generic and are applicable only to the
    specified context. For example,

    {[
      module Theory = (val Theory.instance ()
                          ~context:["arm"; "arm-gnueabi"]
                          ~requires:[
                            "variable-recovery";
                            "stack-frame-analysis";
                            "structural-analysis";
                            "floating-point";
                            "bap.std:bil-semantics";
                          ])
    ]}

    In the example above, theories that are specific to ARM
    architecture, in particular to the arm-gnueabi ABI, will be
    instantiated (in addition to other general theories). The
    [requires] parameter specifies a few semantic tags, describing
    what kind of semantic information is needed, as well as one theory
    explicitly, the [bap.std:bil-semantics], to ensure that each term
    has a BIL denotation.

    [1]: http://okmij.org/ftp/tagless-final/JFP.pdf
    [2]: http://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf
    [3]: http://okmij.org/ftp/tagless-final/course/optimizations.html


    {2 Parsing binary code}

    After a theory is instantiated it could be used to build terms,
    which will trigger analyses associated with each instantiated
    theory.

    However, a program is usually represented as a binary machine
    code, which should be parsed into the Core Theory terms. This
    process is called {i lifting} and program components that do
    lifting are called {i lifters}.

    Lifting is a notoriously hard task, since the machine code is an
    untyped representation and Core Theory terms are rigidly typed. To
    alleviate this problem, the Core Theory library provides a helper
    module [Parser] which could be used to lift an untyped
    representation into the typed Core Theory term.

    It is also possible to reuse lifters which translate the machine
    code in some IL, but writing a parser form that IL. The [Parser]
    module is especially useful here, since it was specifically
    designed for such use-cases.
*)

open Core_kernel
open Caml.Format
open Bap_knowledge


module KB = Knowledge


(** The Core Theory.  *)
module Theory : sig


  (** The denotation of expression.

      Values are used to express the semantics of terms that evaluate
      to a value, aka expressions. Values are sorted and value sorts
      hold static information about the value representation, like the
      number of bits in a bitvector or the representation format in a
      floating point value.

      All values belong to the same Knowledge class and thus share the
      same set of properties, with each property being a specific
      denotation provided by one or more theories. For example, the
      [bap.std:exp] slot holds the denotation of a value in terms of
      BIL expressions.
  *)
  module Value : sig

    (** a type for the value sort *)
    type +'a sort


    (** the class of the values.  *)
    type cls


    (** the value type is an instance of Knowledge.value   *)
    type 'a t = (cls,'a sort) KB.cls KB.value


    (** the class of all values.  *)
    val cls : (cls,unit) KB.cls


    (** [empty s] creates an empty value of sort [s].

        The empty value doesn't hold any denotations and represents an
        absence of information about the value.
    *)
    val empty : 'a sort -> 'a t


    (** [sort v] is the value sort.

        The value sort holds static information about values of that
        sort. *)
    val sort : 'a t -> 'a sort


    (** Value Sorts.

        A concrete and extensible representation of a value sort. The
        sort usually holds the static information about the value
        representation, like the width of a bitvector, the
        representation format of a floating point number, and so on.

        This module is mostly needed when a new sort is defined. The
        Core Theory provides a predefined collection of sorts, here is
        the list:

        - {!Bitv} - bitvectors, e.g., [BitVec(i)]
        - {!Mem} - memories, e.g., [Mem(BitVec(i), BitVec(j)]
        - {!Float} - floating points, e.g., [Float(IEEE754(2, 8, 23), BitVec(32)];
        - {!Rmode} - rounding mode, e.g., [Rmode].

        This module defines a simple DSL for specifying sorts, the DSL
        grammar is made only from three rules:
        {v
           sort = sym | int | sort(sort)
        v}

        The DSL is embedded into the host language with an infix
        operator [@->] for application, e.g., OCaml grammar for sorts is:

        [v
          sort = sym exp | int exp | sort "@->" sort
          exp  = ?a valid OCaml expression?
        v]

        Both symbols and numbers are indexed with a type index, which
        serves as a witness of the sort value, e.g.,
        {[
          type int8
          let int8 : int8 num sort = Sort.int 8
        ]}

        Type indices enable explicit reflection of the target language
        type system in the host type system, while still keeping the
        typing rules under designer's control.

        As a working example, let's develop a sort for binary
        fixed-point numbers. We need to encode the type of the
        underlying bitvector as well as the scaling factor. Suppose,
        we chose to encode the scaling factor by an integer position
        of the point, e.g., 8 means scaling factor 2^8, i.e., a point
        fixed on 8th bit from the left.

        The syntax of our sort will be [Fixpoint(<num>,BitVec(<num>))],
        but we will keep it private to enable further extensions. The
        structure of the sort is explicitly captured in its type, in
        our case, it will be ['p num -> 's Bitv.t -> fixpoint sym],
        but since we want to keep it is hidden by our type [('p,'s) t].
        The same as with the built-in [Bitv] and [Mem] sorts.

        We declare a [fixpoint] constructor and keep it private, to
        ensure that only we can construct (and refine) fixpoint
        sorts. Since the sort type is abstract, we also need to
        provide functions that access arguments of our sort.

        Finally, we need to provide the [refine] function, that will
        cast an untyped sort to its type representation, essentially
        proving that the sort is a valid fixpoint sort.
        {[

          module Fixpoint : sig
            type ('p, 's) t
            val define : int -> 's Bitv.t sort -> ('p,'s) t sort
            val refine : unit sort -> ('p,'s) t sort option
            val bits : ('p,'s) t sort -> 's Bitv.t sort
            val logscale : ('p,'s) t sort -> int
          end = struct
            type fixpoint

            type ('m,'s) t =
              'm Value.Sort.num ->
              's Bitv.t ->
              fixpoint Value.Sort.sym

            let fixpoint = Value.Sort.Name.declare "FixPoint"

            let define p s = Value.Sort.(int p @-> s @-> sym fixpoint)
            let refine s = Value.Sort.refine fixpoint s
            let bits s = Value.Sort.(hd (tl s))
            let logscale s = Value.Sort.(hd s)
          end

          (* Example of usage: *)


          type ('m,'s) fixpoint = Fixpoint.t Value.sort

          type u32              (* type index for 32 bit ints *)
          type p8               (* type index for points at 8th bit *)

          (* a sort of 32-bit bitvectors, usually provided by the CPU model *)
          let u32 : u32 Bitv.t Value.sort = Bitv.define 32

          (* a sort of 8.32 fixed-point numbers. *)
          let fp8_32 : (p8,u32) fixpoint = Fixpoint.define 8 u32
        ]}

    *)
    module Sort : sig
      type +'a t = 'a sort
      type +'a sym
      type +'a num
      type name


      (** [sym name] constructs a sort with the given name.

          A symbolic sort could represent an abstract data type with
          no further information available, e.g., some machine status
          word of unknown size or representation; it may also be used
          to denote data with obvious representation, e.g., the [Bool]
          sort; finally, a symbolic sort could be used as a
          constructor name for an indexed sort, e.g., (BitVec(width)).

          See the Example in the module description for more
          information.


      *)
      val sym : name -> 'a sym sort


      (** [int x] a numeric sort.

          While it is possible to create a standalone numeric sort, it
          wouldn't be possible to refine it, since only symbolic sorts
          re refinable.

          Numeric sorts are used mostly as parameters. See the Example
          section of the module documentation for more information.
      *)
      val int : int -> 'a num sort


      (** [app s1 s2] constructs a sort of sort [s1] and [s2].

          An application could be seen as a tuple building operators,
          thus this operation defines a sort that is described by two
          other sorts.

          Basically, the [app] operator builds a heterogenous list,
          with elements which should be other sorts. The list could be
          then traversed using the [Sort.hd] and [Sort.tl] operators,
          and individual elements could be read with the [value] and
          [name] operators. Since the structure of the sort is fully
          encoded in this type, those operations are total.
      *)
      val app : 'a sort -> 'b sort -> ('a -> 'b) sort


      (** [s1 @-> s2] is [app s1 s2]  *)
      val (@->) : 'a sort -> 'b sort -> ('a -> 'b) sort


      (** [value s] returns the number associated with the numeric sort.  *)
      val value : 'a num sort -> int

      (** [name s] returns the symbol associated with a symbolic sort *)
      val name :  'a sym sort -> name

      (** [hd s] the first argument of sort [s] *)
      val hd : ('a -> 'b) sort -> 'a sort


      (** [tl] the list of arguments of sort [s] excluding the first one*)
      val tl : ('a -> 'b) sort -> 'b sort


      (** [refine witness s] restores the type of the sort.

          The sort type is an index type which could be lost, e.g.,
          when the [forget] function is applied or when the sort is
          stored and read from its textual representation.

          The [refine] function will re-instantiate the type index if
          the constructor name of the sort [s] is the [name].

          This function gives a mandate for the refine function to
          index the sort [s] with any type, which will breach the sort
          type safety, therefore this function should be used with
          care and be hidden behind the abstraction barrier and have a
          concrete type.

          See the Example section in the module documentation for the
          demonstration of how refine should be used.
      *)
      val refine : name -> unit sort -> 'a t option


      (** [forget s] forgets the type index associated with the sort [s].

          This is effectively an upcasting function, that could be
          used when the typing information is not necessary
          anymore or is not representable. The type index could be
          later restored with the [refine] function.
      *)
      val forget : 'a t -> unit t


      (** [same x y] is true if [x] and [y] are of the same structure.  *)
      val same : 'a t -> 'b t -> bool


      (** prints the sort.  *)
      val pp : formatter -> 'a t -> unit


      (** Sort without a type index.

          This module enables construction of complex data structures
          on sorts, like maps, sets, etc, e.g.,

          [let sorts = Set.empty (module Value.Sort.Top)]

          Since such structures are required to be monomorphic, the
          sort type index should be removed using the [forget] function,
          before a sort could be stored in it.
      *)
      module Top : sig
        type t = unit sort [@@deriving bin_io, compare, sexp]
        include Base.Comparable.S with type t := t
      end


      (** The name registry.

          Names of symbols must be unique as the name is used as a
          witness of authenticity of the sort. Once obtained, the name
          should be kept secret beyond the module signature.

          See the Example section of the [Sort] module documentation
          for more information.
      *)
      module Name : sig
        type t


        (** [declare ?package name] declares a new [name].

            The declared name must be unique to the [package]. If such
            name is already declared in the [package], then the
            declaration fails.
        *)
        val declare : ?package:string -> string -> name
        include Base.Comparable.S with type t := t
      end
    end
  end


  (** The denotation of statements.

      An effect is a Knowledge value that is used to give a denotation
      to the language forms that do not evaluate to values but change
      the state of the system, i.e., to what is usually called
      "statement".

      All effects belong to the same Knowledge class and share the
      same set of properties, with each property being a specific
      denotation provided by on or more theories. For example,
      [bap.std:bil] slot holds the denotation of a value in terms of
      the BIL statements.
  *)
  module Effect : sig


    (** a type for the effect sort  *)
    type +'a sort


    (** the class of effects.  *)
    type cls


    (** the effect type is an instance of the Knowledge.value  *)
    type 'a t = (cls,'a sort) KB.cls KB.value


    (** the class of all effects.   *)
    val cls : (cls,unit) KB.cls


    (** [empty s] creates an empty effect value.

        The empty effect denotes an absence of any specific knowledge
        about the effects produced by a term.
    *)
    val empty : 'a sort -> 'a t


    (** [sort eff] returns the sort of the effect [eff].  *)
    val sort : 'a t -> 'a sort


    (** Effect sorts.

        The sort of an effect holds static information that is common
        to all effects of that sort.

        We distinguish two kinds of effects - [ctrl] effects that affect
        which instructions will be executed next and [data] effects that
        affect only the values in the computer storage.

        The [unit] effect represents an effect that is a mixture of
        [ctrl] and [data] effects.
    *)
    module Sort : sig
      type +'a t = 'a sort
      type data = private Data
      type ctrl = private Ctrl


      (** [data kind] defines a data effect of the [kind].  *)
      val data : string -> data t


      (** [ctrl kind] defines a ctrl effect of the [kind]. *)
      val ctrl : string -> ctrl t


      (** [top] is a set of all possible effects.

          This sort indicates that the statement can have any effect.
      *)
      val top : unit t


      (** [bot] is an empty set of effects.

          This sort indicates that the statement doesn't have any
          observable effects, thus it could be coerced to any other
          sort.
      *)
      val bot : 'a t


      (** [both s1 s2] an effect of both [s1] and [s2]  *)
      val both : 'a t -> 'a t -> 'a t


      (** [s1 && s2] is [both s1 s2].  *)
      val (&&) : 'a t -> 'a t -> 'a t


      (** [union [s1;...;sN] is [s1 && ... && sN].  *)
      val union : 'a t list -> 'a t


      (** [join xs ys] is [union [union xs; union ys ]].  *)
      val join : 'a t list -> 'b t list -> unit t


      (** [order xs ys] orders effects by the order of inclusion.

          [xs] is before [ys] if [ys] includes all effects of [xs],
          otherwise.
      *)
      val order : 'a t -> 'b t -> KB.Order.partial


      (** the register read effect.  *)
      val rreg : data t

      (** the register write effect.  *)
      val wreg : data t

      (** the memory read effect.  *)
      val rmem : data t

      (** is the memory write effect.  *)
      val wmem : data t

      (** the memory barrier effect  *)
      val barr : data t

      (** the normal control flow effect  *)
      val fall : ctrl t

      (** the jump effect.  *)
      val jump : ctrl t

      (**  the conditional branching effect  *)
      val cjmp : ctrl t
    end
  end

  type 'a value = 'a Value.t
  type 'a effect = 'a Effect.t


  (** The sort for boolean values.

      Booleans are one bit values.
  *)
  module Bool : sig
    type t

    (** the Bool sort.  *)
    val t : t Value.sort


    (** [refine s] if [s] is [Bool] then returns [Some t].  *)
    val refine : unit Value.sort -> t Value.sort option
  end


  (** Sorts of bitvectors  *)
  module Bitv : sig
    type 'a t

    (** [define size] defines a sort of bitvectors of the given [size].  *)
    val define : int -> 'a t Value.sort

    (** [refine s] if [s] is a bitvector sort, then restores its type.   *)
    val refine : unit Value.sort -> 'a t Value.sort option

    (** [size s] the [size] argument of [s].  *)
    val size : 'a t Value.sort -> int
  end


  (** Sorts of memories.

      A memory is an associative container of bitvectors indexed with
      bitvector keys.
  *)
  module Mem : sig
    type ('a,'b) t


    (** [define ks vs] a sort of memories with keys of type [ks] and
        data of type [vs].   *)
    val define : 'a Bitv.t Value.sort -> 'b Bitv.t Value.sort -> ('a,'b) t Value.sort


    (** [refine s] if [s] is a memory sort then restores its type.  *)
    val refine : unit Value.sort -> ('a,'b) t Value.sort option


    (** [keys s] returns the sort of keys.  *)
    val keys : ('a,'b) t Value.sort -> 'a Bitv.t Value.sort

    (** [vals s] returns the sort of values.  *)
    val vals : ('a,'b) t Value.sort -> 'b Bitv.t Value.sort
  end


  (** Sorts for floating point numbers.  *)
  module Float : sig


    (** Sort describing the representation format of a floating point number.  *)
    module Format : sig
      type ('r,'s) t


      (** [define r s] defines a sort given interpretation [r] of bitvector [s].   *)
      val define : 'r Value.sort -> 's Bitv.t Value.sort -> ('r,'s) t Value.sort


      (** [bits s] returns the sort of bitvectors that are used by
          floating point numbers of sort [s].  *)
      val bits : ('r,'s) t Value.sort -> 's Bitv.t Value.sort


      (** [exp s] returns an expression that describes the
          interpretation of the bits of the floating point numbers
          represented by the sort [s].  *)
      val exp : ('r,'s) t Value.sort -> 'r Value.sort
    end

    type ('r,'s) format = ('r,'s) Format.t
    type 'f t


    (** [define r s] defines a floating point sort, indexed by the
        floating point format [r] that gives the interpretation to
        the bits of bitvectors of sort [s]. *)
    val define : ('r,'s) format Value.sort -> ('r,'s) format t Value.sort


    (** [refine s] if [s] is a floating point sort then restores its type.  *)
    val refine : unit Value.sort -> ('r,'s) format t Value.sort option


    (** [format s] returns the format of floating points of sort [s].   *)
    val format : ('r,'s) format t Value.sort -> ('r,'s) format Value.sort


    (** [bits s] returns the sort of bitvectors that are used to
        represent floating point numbers of sort [s].   *)
    val bits : ('r,'s) format t Value.sort -> 's Bitv.t Value.sort
  end


  (** Rounding modes.  *)
  module Rmode : sig
    type t


    (** The sort of rounding modes.  *)
    val t : t Value.sort


    (** [refine s] if [s] is the rounding mode sort, then restores its type.*)
    val refine : unit Value.sort -> t Value.sort option
  end

  type 'a pure = 'a value knowledge
  type 'a eff = 'a effect knowledge
  type ('r,'s) format = ('r,'s) Float.format


  (** Variables.

      Variables give names to values, read the {{!vars}Variables}
      section for more information.
  *)
  module Var : sig
    type 'a t
    type ident [@@deriving bin_io, compare, sexp]
    type ord


    (** [define sort name] a global variable with [name] and [sort].  *)
    val define : 'a Value.sort -> string -> 'a t


    (** [create s id] a variable with sort [s] and identifier [id].

        The identifier encodes what kind of variable is created. This
        function is usually created by parsers, that parse a
        well-formed programs.
    *)
    val create : 'a Value.sort -> ident -> 'a t


    (** [forget v] forgets the type index describing the sort of the variable.  *)
    val forget : 'a t -> unit t


    (** [resort v] changes the sort of the variable.  *)
    val resort : 'a t -> 'b Value.sort -> 'b t


    (** [versioned v n] creates the [n]th version of the variable [v].

        Variable versions could be used to represent the same variable
        under different context or to ensure some normalization of the
        program, e.g., SSA.
    *)
    val versioned: 'a t -> int -> 'a t


    (** [version v] is the version of the variable [v].

        Variable versions could be used to represent the same variable
        under different context or to ensure some normalization of the
        program, e.g., SSA. *)
    val version : 'a t -> int


    (** [ident v] is variable's identifier.  *)
    val ident : 'a t -> ident


    (** [name v] is variable's name  *)
    val name : 'a t -> string


    (** [sort v] is variable's sort.  *)
    val sort : 'a t -> 'a Value.sort


    (** [is_virtual v] is [true] if [v] is virtual.

        Virtual variables do not have any physical representation.
    *)
    val is_virtual : 'a t -> bool


    (** [is_mutable v] is [true] if [v] is mutable.

        Only scoped variables are immutable.
    *)
    val is_mutable : 'a t -> bool


    (** [fresh s] creates a fresh virtual mutable variable of sort [s].  *)
    val fresh : 'a Value.sort -> 'a t knowledge


    (** [scoped s] creates a fresh immutable variable of sort [s].  *)
    val scoped : 'a Value.sort -> ('a t -> 'b pure) -> 'b pure


    (** Variable identifiers.  *)
    module Ident : sig
      type t = ident [@@deriving bin_io, compare, sexp]
      include Stringable.S with type t := t
      include Base.Comparable.S with type t := t
                                 and type comparator_witness = ord
    end


    (** Variables with erased sort index.

        This module enables construction of complex data structures on
        variables, e.g., [Set.empty (module Theory.Var.Top)].
    *)
    module Top : sig
      type nonrec t = unit t [@@deriving bin_io, compare, sexp]
      include Base.Comparable.S with type t := t
    end
  end

  type data = Effect.Sort.data
  type ctrl = Effect.Sort.ctrl


  (** a concrete representation of words in the Core Theory.  *)
  type word = Bitvec.t


  (**  a concrete representation of variables.  *)
  type 'a var = 'a Var.t


  (** a class index for class of programs.  *)
  type program


  (** label is an object of the program class.  *)
  type label = program KB.Object.t


  (** The denotation of programs.

      Values of class [program] are used to express the semantics of
      programs. With a [label], which is an abstract pointer to a
      program, we associate a value of type [Program.t] which denotes
      the program that will be executed when the control will be
      passed to that label.
  *)
  module Program : sig
    type t = (program,unit) KB.cls KB.value
    val cls : (program,unit) KB.cls


    (** The semantics of programs.

        The semantics of a program is denoted with effects that this
        program produces, so effectively [Program.Semantics = Effect],
        but we reexport it in a separate module here, to separate the
        concerns.
    *)
    module Semantics : sig
      type cls = Effect.cls
      type t = unit Effect.t

      (** the class of program semantics values.  *)
      val cls : (cls, unit Effect.sort) Knowledge.cls

      (** the slot to store program semantics.  *)
      val slot : (program, t) Knowledge.slot
      include Knowledge.Value.S with type t := t
    end
    include Knowledge.Value.S with type t := t
  end


  (** A program label.

      Labels are generalization of addresses and are use to uniquely
      identify a program location, even if this location is not having
      an address. Labels are Knowledge objects of the [program] class,
      therefore they have the semantics property, accessible via the
      [Program.Semantics.slot].

  *)
  module Label : sig
    type t = label


    (** {3 Properties}  *)

    (** the address of the label.  *)
    val addr : (program, Bitvec.t option) KB.slot


    (** the linkage name of the label  *)
    val name : (program, string option) KB.slot


    (** the interrupt vector of the label.

        Labels could also represent code in interrupt vector
        routines, therefore the might be referenced by a number, not
        by an address of a name.
    *)
    val ivec : (program, int option) KB.slot


    (** possible aliases under which the label might be known.

        This may include versioned names, demangled names, etc.
    *)
    val aliases : (program, Set.M(String).t) KB.slot


    (** a link is valid if it references a valid program.

        If a link references a memory location which is not
        executable, then it is not valid.
    *)
    val is_valid : (program, bool option) KB.slot


    (** a link is subroutine if it is an entry point to a subroutine.  *)
    val is_subroutine : (program, bool option) KB.slot


    (** [for_addr x] generates a link to address [x].

        It is guaranteed that every call [for_addr x] with the same
        [x] will return the same label.
    *)
    val for_addr : Bitvec.t -> t knowledge


    (** [for_name x] generates a link to program with linkage name [x].

        It is guaranteed that every call [for_name x] with the same
        [x] will return the same label.
    *)
    val for_name : string -> t knowledge


    (** [for_name x] generates a link to an interrupt service number [x].

        It is guaranteed that every call [for_name x] with the same
        [x] will return the same label.
    *)
    val for_ivec : int -> t knowledge

    include Knowledge.Object.S with type t := t
  end


  (** a boolean terms  *)
  type bool = Bool.t pure


  (** a bitvector term  *)
  type 'a bitv = 'a Bitv.t pure


  (** a memory term  *)
  type ('a,'b) mem = ('a,'b) Mem.t pure


  (** a floating point term  *)
  type 'f float = 'f Float.t pure


  (** a rounding mode term  *)
  type rmode = Rmode.t pure


  (** The initial theory. *)
  module type Init = sig

    (** [var v] is the value of the variable [v].  *)
    val var : 'a var -> 'a pure

    (** [unk s] an unknown value of sort [s].

        This term explicitly denotes a term with undefined or unknown
        value.  *)
    val unk : 'a Value.sort -> 'a pure

    (** [let_ v exp body] bind the value of [exp] to [v] [body]. *)
    val let_ : 'a var -> 'a pure -> 'b pure -> 'b pure
  end


  (** The theory of booleans.  *)
  module type Bool = sig

    (** [b0] is [false] aka [0] bit  *)
    val b0 : bool

    (** [b1] is [true] aka [1] bit  *)
    val b1 : bool

    (** [inv x] inverts [x].  *)
    val inv : bool -> bool

    (** [and_ x y] is a conjunction of [x] and [y].  *)
    val and_ : bool -> bool -> bool

    (** [or_ x y] is a disjunction of [x] and [y].  *)
    val or_ : bool -> bool -> bool

    (** [ite c x y] is [x] if [c] evaluates to [b1] else [y].  *)
    val ite : bool -> 'a pure -> 'a pure -> 'a pure

  end



  (** The theory of bitvectors.  *)
  module type Bitv = sig

    (** [int s x] is a bitvector constant [x] of sort [s]. *)
    val int : 's Bitv.t Value.sort -> word -> 's bitv

    (** [msb x] is the most significant bit of [x].  *)
    val msb : 's bitv -> bool

    (** [lsb x] is the least significant bit of [x].  *)
    val lsb : 's bitv -> bool

    (** [neg x] is two-complement unary minus  *)
    val neg  : 's bitv -> 's bitv

    (** [not x] is one-complement negation.  *)
    val not    : 's bitv -> 's bitv

    (** [add x y] addition modulo [2^'s]  *)
    val add  : 's bitv -> 's bitv -> 's bitv

    (** [sub x y] subtraction modulo [2^'s]  *)
    val sub  : 's bitv -> 's bitv -> 's bitv

    (** [mul x y] multiplication modulo [2^'s]  *)
    val mul  : 's bitv -> 's bitv -> 's bitv

    (** [div x y] unsigned division modulo [2^'s] truncating towards 0.

        The division by zero is defined to be a vector of all ones of
        size ['s].
    *)
    val div  : 's bitv -> 's bitv -> 's bitv


    (** [sdiv x y] is signed division of [x] by [y] modulo [2^'s],

        The signed division operator is defined in terms of the [div]
        operator as follows:
        {v
                             /
                             | div x y : if not mx /\ not my
                             | neg (div (neg x) y) if mx /\ not my
                 x sdiv y = <
                             | neg (div x (neg y)) if not mx /\ my
                             | div (neg x) (neg y) if mx /\ my
                             \

              where mx = msb x, and my = msb y.
          v}
    *)
    val sdiv : 's bitv -> 's bitv -> 's bitv


    (** [modulo x y] is the remainder of [div x y] modulo [2^'s].  *)
    val modulo : 's bitv -> 's bitv -> 's bitv

    (** [smodulo x y] is the signed remainder of [div x y] modulo [2^'s].


        This version of the signed remainder where the sign follows the
        dividend, and is defined via the [% = modulo] operation as follows

        {v
                           /
                           | x % y : if not mx /\ not my
                           | neg (neg x % y) if mx /\ not my
            x smodulo y = <
                           | neg (x % (neg y)) if not mx /\ my
                           | neg (neg x % neg y) mod m if mx /\ my
                           \

            where mx = msb x  and my = msb y.
       v}

    *)
    val smodulo : 's bitv -> 's bitv -> 's bitv


    (** [logand x y] is a bitwise logical and of [x] and [y].  *)
    val logand : 's bitv -> 's bitv -> 's bitv

    (** [logor x y] is a bitwise logical or of [x] and [y].  *)
    val logor  : 's bitv -> 's bitv -> 's bitv

    (** [logxor x y] is a bitwise logical xor of [x] and [y].  *)
    val logxor  : 's bitv -> 's bitv -> 's bitv

    (** [shiftr s x m] shifts [x] right by [m] bits filling with [s]. *)
    val shiftr : bool -> 's bitv -> 'b bitv -> 's bitv

    (** [shiftl s x m] shifts [x] left by [m] bits filling with [s].   *)
    val shiftl : bool -> 's bitv -> 'b bitv -> 's bitv

    (** [sle x y] binary predicate for singed less than or equal   *)
    val sle : 'a bitv -> 'a bitv -> bool

    (** [ule x y] binary predicate for unsigned less than or equal  *)
    val ule : 'a bitv -> 'a bitv -> bool


    (** [cast s b x] casts [x] to sort [s] filling with [b].

        If [m = size s - size (sort b) > 0] then [m] bits [b] are
        prepended to the most significant part of the vector.

        Otherwise, if [m <= 0], i.e., it is a narrowing cast, then the
        value of [b] doesn't affect the result of evaluation.
    *)
    val cast : 'a Bitv.t Value.sort -> bool -> 'b bitv -> 'a bitv


    (** [concat s xs] concatenates a list of vectors [xs].

        All vectors are concatenated from left to right (so that the
        most significant bits of the resulting vector are defined by
        the first element of the list and so on).

        The result of concatenation are then casted to sort [s] with
        all extra bits (if any) set to [zero].
    *)
    val concat : 'a Bitv.t Value.sort -> 'b bitv list -> 'a bitv


    (** [append s x y] appends [x] and [y] and casts to [s].

        Appends [x] and [y] so that in the resulting vector the vector
        [x] occupies the most significant part and [y] the least
        significant part. The resulting vector is then casted to the
        sort [s] with extra bits (if any) set to zero. *)
    val append : 'a Bitv.t Value.sort -> 'b bitv -> 'c bitv -> 'a bitv
  end


  (** The theory of memories.  *)
  module type Memory = sig


    (** [load m k] is the value associated with the key [k] in the memory [m].   *)
    val load : ('a,'b) mem -> 'a bitv -> 'b bitv

    (** [store m k x] a memory [m] in which the key [k] is associated
        with the word [x].     *)
    val store : ('a,'b) mem -> 'a bitv -> 'b bitv -> ('a,'b) mem
  end


  (** The theory of effects.  *)
  module type Effect = sig

    (** [perform s] performs a generic effect of sort [s].  *)
    val perform : 'a Effect.sort -> 'a eff


    (** [set v x] changes the value stored in [v] to the value of [x]. *)
    val set : 'a var -> 'a pure -> data eff

    (** [jmp dst] passes the control to a program located at [dst].  *)
    val jmp  : _ bitv -> ctrl eff


    (** [goto lbl] passes the control to a program labeled with [lbl].   *)
    val goto : label -> ctrl eff


    (** [seq x y] performs effect [x], after that perform effect [y].  *)
    val seq : 'a eff -> 'a eff -> 'a eff

    (** [blk lbl data ctrl] a labeled sequence of effects. *)
    val blk : label -> data eff -> ctrl eff -> unit eff


    (** [repeat c data] repeats data effects until the condition [c] holds. *)
    val repeat : bool -> data eff -> data eff


    (** [branch c lhs rhs] if [c] holds then performs [lhs] else [rhs].  *)
    val branch : bool -> 'a eff -> 'a eff -> 'a eff
  end



  (** The Minimal theory. *)
  module type Minimal = sig
    include Init
    include Bool
    include Bitv
    include Memory
    include Effect
  end


  (** The Basic theory of bitvector machines.

      The Basic theory could be derived from the [Minimal] theory
      using the Basic functor.
  *)
  module type Basic = sig
    include Minimal


    (** [zero s] creates a bitvector of all zeros of sort [s].  *)
    val zero : 'a Bitv.t Value.sort -> 'a bitv

    (** [is_zero x] holds if [x] is a bitvector of all zeros  *)
    val is_zero  : 'a bitv -> bool

    (** [non_zero x] holds if [x] is not a zero bitvector.  *)
    val non_zero : 'a bitv -> bool

    (** [succ x] is the successor of [x].  *)
    val succ : 'a bitv -> 'a bitv

    (** [pred x] is the predecessor of [x].  *)
    val pred : 'a bitv -> 'a bitv

    (** [nsucc x n] is the [n]th successor of [x].  *)
    val nsucc : 'a bitv -> int -> 'a bitv

    (** [npred x] is the [n]th predecessor of [x].  *)
    val npred : 'a bitv -> int -> 'a bitv

    (** [high s x] is the high cast of [x] to sort [s].

        if [m = size (sort x) - size s > 0]
        then [high s x] evaluates to [m] most significant bits of [x];
        else if [m = 0] then [high s x] evaluates to the value of [x];
        else [m] zeros are prepended to the most significant part of [x].
    *)
    val high : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv

    (** [low s x = cast s b0 x] is the low cast of [x] to sort [s].

        if [m = size (sort x) - size s  < 0]
        then [low s x] evaluates to [size s] least significant bits of [x];
        else if [m = 0] then [high s x] evaluates to the value of [x];
        else [m] zeros are prepended to the most significant part of [x].
    *)
    val low  : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv


    (** [signed s x = cast s (msb x) x] is the signed cast of [x] to sort [s].

        if [m = size s - (size (sort x)) > 0]
        then extends [x] on its most significant side with [m] bits equal to [msb x];
        else if [m = 0] then [signed s x] evaluates to [x]
        else it evaluates to [size s] least significant bits of [x].
    *)
    val signed : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv

    (** [unsigned s x = cast s b0 x] is the unsigned cast of [x] to sort [s].

        if [m = size s - (size (sort x)) > 0]
        then extends [x] on its most significant side with [m] zeros;
        else if [m = 0] then [unsigned s x] evaluates to [x]
        else it evaluates to [size s] least significant bits of [x].
    *)
    val unsigned  : 'a Bitv.t Value.sort -> 'b bitv -> 'a bitv


    (** [extract s hi lo x] extracts bits of [x] between [hi] and [lo].

        Extracts [hi-lo+1] consecutive bits of [x] from [hi] to [lo],
        and casts them to sort [s] with any excessive bits set to
        zero.

        Note that [hi-lo+1] is taken modulo [2^'b], so this operation
        is defined even if [lo] is greater or equal to [hi].
    *)
    val extract : 'a Bitv.t Value.sort -> 'b bitv -> 'b bitv -> _ bitv -> 'a bitv


    (** [loadw s e m k] loads a word from the memory [m].



        if [e] evaluates to [b1] (big endian case),
        then the term evaluates to [low s (m[k] @ m[k+1] @ ... @ m[k+n] )],
        else the term evaluates to [low s (m[k+n] @ m[k+n-1] @ ... @ m[k] )]
        where [x @ y] is [append (vals m) x y], and [n] is the smallest
        natural number such that [n * size (vals (sort m)) >= size s],
        an [m[i]] is the [i]'th value of memory [m].
        .

        This is a generic function that loads words using specified
        endianess ([e] could be read as [is_big_endian]) with
        arbitrary byte size.
    *)
    val loadw : 'c Bitv.t Value.sort -> bool -> ('a, _) mem -> 'a bitv -> 'c bitv


    (** [storew e m k x] stores a word in the memory [m].

        Splits the word [x] into chunks of size equal to the size of
        memory elements and lays them out in the big endian order, if
        [e] evaluates to [b1], or little endian order otherwise.
    *)
    val storew : bool -> ('a, 'b) mem -> 'a bitv -> 'c bitv -> ('a, 'b) mem


    (** [arshift x m = shiftr (msb x) m] arithmetically shifts [x] right by [m] bits.  *)
    val arshift : 'a bitv -> 'b bitv -> 'a bitv


    (** [rshift x m = shiftr b0 x m] shifts [x] right by [m] bits  *)
    val rshift : 'a bitv -> 'b bitv -> 'a bitv

    (** [lshift x y = shiftl b0 x m] shifts [x] left by [m] bits.  *)
    val lshift : 'a bitv -> 'b bitv -> 'a bitv

    (** [eq x y] holds if [x] and [y] are bitwise equal.  *)
    val eq  : 'a bitv -> 'a bitv -> bool

    (** [eq x y] holds if [x] and [y] are not bitwise equal.  *)
    val neq : 'a bitv -> 'a bitv -> bool

    (** [slt x y] signed strict less than. *)
    val slt : 'a bitv -> 'a bitv -> bool

    (** [ult x y] unsigned strict less than. *)
    val ult : 'a bitv -> 'a bitv -> bool

    (** [sgt x y] signed strict greater than. *)
    val sgt : 'a bitv -> 'a bitv -> bool

    (** [ugt x y] unsigned strict greater than. *)
    val ugt : 'a bitv -> 'a bitv -> bool

    (** [sge x y] signed greater or equal than. *)
    val sge : 'a bitv -> 'a bitv -> bool

    (** [sge x y] signed greater or equal than. *)
    val uge : 'a bitv -> 'a bitv -> bool
  end


  (** The Basic Theory of Floating Points.

      Floating point numbers represent a finite subset of the set of
      real numbers. Some formats also extend this set with special
      values to represent infinities or error conditions. This, in
      general, exceeds the scope of the floating point theory, however
      the theory includes predicates with domains that potentially may
      include this special numbers, e.g., [is_nan]. For floating point
      formats that do not support special values, such predicates will
      become constant functions.

      All operations in the Floating Point theory are defined in terms
      of operations on real numbers. Since floating point numbers
      represent only a subset of the real set,
      denotations select a number from the set of numbers of the
      floating point sort using concrete rules expressed in terms of
      rounding modes. The rounding mode is a parameter of many
      operations, denoted with a term of sort [rmode].

  *)
  module type Fbasic = sig

    (** [float s x] interprets [x] as a floating point number.  *)
    val float : ('r,'s) format Float.t Value.sort -> 's bitv -> ('r,'s) format float

    (** [fbits x] is a bitvector representation of the floating point number [x].  *)
    val fbits : ('r,'s) format float -> 's bitv

    (** [is_finite x] holds if [x] represents a finite number.

        A floating point number is finite if it represents a
        number from the set of real numbers [R].

        The predicate always holds for formats in which only finite
        floating point numbers are representable.
    *)
    val is_finite : 'f float -> bool

    (** [is_nan x] holds if [x] represents a not-a-number.

        A floating point value is not-a-number if it is neither finite
        nor infinite number.

        The predicated never holds for formats that represent only
        numbers.
    *)
    val is_nan : 'f float -> bool

    (** [is_inf x] holds if [x] represents an infinite number.

        Never holds for formats in which infinite numbers are not
        representable.
    *)
    val is_inf : 'f float -> bool

    (** [is_fzero x] holds if [x] represents a zero. *)
    val is_fzero : 'f float -> bool

    (** [is_fpos x] holds if [x] represents a positive number.

        The denotation is not defined if [x] represents zero.
    *)
    val is_fpos : 'f float -> bool

    (** [is_fneg x] hold if [x] represents a negative number.

        The denotation is not defined if [x] represents zero.
    *)
    val is_fneg : 'f float -> bool

    (** {3 Rounding modes}

        Many operations in the Theory of Floating Point numbers are
        defined using the rounding mode parameter.

        The rounding mode gives a precise meaning to the phrase
        "the closest floating point number to [x]", where [x] is a
        real number. When [x] is not representable by the given
        format, some other number [x'] is selected based on
        rules of the rounding mode.
    *)

    (** rounding to nearest, ties to even.

        The denotation is the floating point number nearest to the
        denoted real number. If the two nearest numbers are equally
        close, then the one with an even least significant digit shall
        be selected. The denotation is not defined, if both numbers
        have an even least significant digit.
    *)
    val rne : rmode

    (** rounding to nearest, ties away.

        The denotation is the floating point number nearest to the
        denoted real number. If the two nearest numbers are equally
        close, then the one with larger magnitude shall be selected.
    *)
    val rna : rmode

    (** rounding towards positive.

        The denotation is the floating point number that is nearest
        but no less than the denoted real number.
    *)
    val rtp : rmode

    (** rounding towards negative.

        The denotation is the floating point number that is nearest
        but not greater than the denoted real number.
    *)
    val rtn : rmode

    (** rounding towards zero.

        The denotation is the floating point number that is nearest
        but not greater in magnitude than the denoted real number.
    *)
    val rtz : rmode

    (** [requal x y] holds if rounding modes are equal.  *)
    val requal : rmode -> rmode -> bool

    (** [cast_float s m x] is the closest to [x] floating number of sort [s].

        The bitvector [x] is interpreted as an unsigned integer in the
        two-complement form.
    *)
    val cast_float  : 'f Float.t Value.sort  -> rmode -> 'a bitv -> 'f float


    (** [cast_sfloat s rm x] is the closest to [x] floating point number of sort [x].

        The bitvector [x] is interpreted as a signed integer in the
        two-complement form.
    *)
    val cast_sfloat : 'f Float.t Value.sort -> rmode -> 'a bitv -> 'f float

    (** [cast_int s rm x] returns an integer closest to [x].

        The resulting bitvector should be interpreted as an unsigned
        two-complement integer.
    *)
    val cast_int    : 'a Bitv.t Value.sort -> rmode -> 'f float -> 'a bitv

    (** [cast_sint s rm x] returns an integer closest to [x].

        The resulting bitvector should be interpreted as a signed
        two-complement integer.
    *)
    val cast_sint   : 'a Bitv.t Value.sort -> rmode -> 'f float -> 'a bitv


    (** [fneg x] is [-x]  *)
    val fneg    : 'f float -> 'f float

    (** [fabs x] the absolute value of [x].  *)
    val fabs    : 'f float -> 'f float

    (** [fadd m x y] is the floating point number closest to [x+y].  *)
    val fadd    : rmode -> 'f float -> 'f float -> 'f float

    (** [fsub m x y] is the floating point number closest to [x-y].  *)
    val fsub    : rmode -> 'f float -> 'f float -> 'f float

    (** [fmul m x y] is the floating point number closest to [x*y].  *)
    val fmul    : rmode -> 'f float -> 'f float -> 'f float

    (** [fdiv m x y] is the floating point number closest to [x/y].  *)
    val fdiv    : rmode -> 'f float -> 'f float -> 'f float

    (** [fsqrt m x] is the floating point number closest to [sqrt x].

        The denotation is not defined if
    *)
    val fsqrt   : rmode -> 'f float -> 'f float

    (** [fdiv m x y] is the floating point number closest to the
        remainder of [x/y].  *)
    val fmodulo : rmode -> 'f float -> 'f float -> 'f float

    (** [fmad m x y z] is the floating point number closest to [x * y + z].  *)
    val fmad    : rmode -> 'f float -> 'f float -> 'f float -> 'f float


    (** [fround m x] is the floating point number closest to [x]
        rounded to an integral, using the rounding mode [m].   *)
    val fround   : rmode -> 'f float -> 'f float

    (** [fconvert f r x] is the closest to [x] floating number in format [f].  *)
    val fconvert : 'f Float.t Value.sort ->  rmode -> _ float -> 'f float

    (** [fsucc m x] is the least floating point number representable
        in (sort x) that is greater than [x].  *)
    val fsucc  : 'f float -> 'f float

    (** [fsucc m x] is the greatest floating point number representable
        in (sort x) that is less than [x].  *)
    val fpred  : 'f float -> 'f float

    (** [forder x y] holds if floating point number [x] is less than [y].

        The denotation is not defined if either of arguments do not
        represent a floating point number.
    *)
    val forder : 'f float -> 'f float -> bool
  end


  (** The theory of floating point numbers modulo transcendental functions.

      This signature includes several functions that can be expressed
      in terms of a finite sequence of the operations of addition,
      multiplication, and root extraction. Despite the fact, that
      those operation are defined in the Basic Theory of Floating
      points ([Fbasic)], operations in this signature couldn't be
      expressed in terms of [Fbasic] because the set of floating
      points is finite. Therefore the operations in this signature are
      defined in terms of real numbers arithmetic.

      For example, the following expression is not true for all [x,y,m]:

      [fmul m x (fmul m x x) = pown m x (succ (succ one))],

      E.g., when [fmul m x x] is not denoted with [x^2], but the
      number that is closest to [x^2] with respect to the rounding
      mode [m].

  *)
  module type Float = sig
    include Fbasic


    (** [pow m b a] is a floating point number closest to [b^a].

        Where [b^a] is [b] raised to the power of [a].

        Values, such as [0^0], as well as [1^infinity] and
        [infinity^1] in formats that have a representation for
        infinity, are not defined.
    *)
    val pow : rmode -> 'f float -> 'f float -> 'f float


    (** [compound m x n] is the floating point number closest to [(1+x)^n].

        Where [b^a] is [b] raised to the power of [a].

        The denotation is not defined if [x] is less than [-1], or if
        [x] is [n] represent zeros, or if [x] doesn't represent a
        finite floating point number.
    *)
    val compound : rmode -> 'f float -> 'a bitv -> 'f float


    (** [rootn m x n] is the floating point number closest to [x^(1/n)].

        Where [b^a] is [b] raised to the power of [a].

        The denotation is not defined if:
        - [n] is zero;
        - [x] is zero and n is less than zero;
        - [x] is not a finite floating point number;
    *)
    val rootn    : rmode -> 'f float -> 'a bitv -> 'f float


    (** [pown m x n] is the floating point number closest to [x^n].

        Where [b^a] is [b] raised to the power of [a].

        The denotation is not defined if [x] and [n] both represent
        zero or if [x] doesn't represent a finite floating point
        number.
    *)
    val pown    : rmode -> 'f float -> 'a bitv -> 'f float


    (** [rsqrt m x] is the closest floating point number to [1 / sqrt x].

        The denotation is not defined if [x] is less than or equal to
        zero or doesn't represent a finite floating point number.
    *)
    val rsqrt    : rmode -> 'f float -> 'f float


    (** [hypot m x y] is the closest floating point number to [sqrt(x^2 + y^2)].

        The denotation is not defined if [x] or [y] do not represent
        finite floating point numbers. *)
    val hypot    : rmode -> 'f float -> 'f float -> 'f float
  end


  (** The Theory of Transcendental Functions. *)
  module type Trans = sig


    (** [exp m x] is the floating point number closes to [e^x],

        where [b^a] is [b] raised to the power of [a] and [e] is the
        base of natural logarithm.
    *)
    val exp      : rmode -> 'f float -> 'f float

    (** [expm1 m x] is the floating point number closes to [e^x - 1],

        where [b^a] is [b] raised to the power of [a] and [e] is the
        base of natural logarithm.
    *)
    val expm1    : rmode -> 'f float -> 'f float

    (** [exp2 m x] is the floating point number closes to [2^x],

        where [b^a] is [b] raised to the power of [a].
    *)
    val exp2     : rmode -> 'f float -> 'f float

    (** [exp2 m x] is the floating point number closes to [2^x - 1],

        where [b^a] is [b] raised to the power of [a].
    *)
    val exp2m1   : rmode -> 'f float -> 'f float

    (** [exp10 m x] is the floating point number closes to [10^x],

        where [b^a] is [b] raised to the power of [a].
    *)
    val exp10    : rmode -> 'f float -> 'f float


    (** [exp10m1 m x] is the floating point number closes to [10^x - 1],

        where [b^a] is [b] raised to the power of [a].
    *)
    val exp10m1  : rmode -> 'f float -> 'f float


    (** [log m x] is the floating point number closest to [log x].  *)
    val log      : rmode -> 'f float -> 'f float

    (** [log2 m x] is the floating point number closest to [log x / log 2].  *)
    val log2     : rmode -> 'f float -> 'f float

    (** [log10 m x] is the floating point number closest to [log x / log 10].  *)
    val log10    : rmode -> 'f float -> 'f float

    (** [logp1 m x] is the floating point number closest to [log (1+x)].  *)
    val logp1    : rmode -> 'f float -> 'f float

    (** [logp1 m x] is the floating point number closest to [log (1+x) / log 2].  *)
    val log2p1   : rmode -> 'f float -> 'f float

    (** [logp1 m x] is the floating point number closest to [log (1+x) / log 10].  *)
    val log10p1  : rmode -> 'f float -> 'f float

    (** [sin m x] is the floating point number closest to [sin x].  *)
    val sin      : rmode -> 'f float -> 'f float

    (** [cos m x] is the floating point number closest to [cos x].  *)
    val cos      : rmode -> 'f float -> 'f float

    (** [tan m x] is the floating point number closest to [tan x].  *)
    val tan      : rmode -> 'f float -> 'f float

    (** [sinpi m x] is the floating point number closest to [sin (pi*x)].  *)
    val sinpi    : rmode -> 'f float -> 'f float

    (** [cospi m x] is the floating point number closest to [cos (pi*x)].  *)
    val cospi    : rmode -> 'f float -> 'f float

    (** [atanpi m y x] is the floating point number closest to [atan(y/x) / pi].  *)
    val atanpi   : rmode -> 'f float -> 'f float

    (** [atanpi m y x] is the floating point number closest to [atan(y/x) / (2*pi)].  *)
    val atan2pi  : rmode -> 'f float -> 'f float -> 'f float

    (** [asin m x] is the floating point number closest to [asin x].  *)
    val asin     : rmode -> 'f float -> 'f float

    (** [acos m x] is the floating point number closest to [acos x].  *)
    val acos     : rmode -> 'f float -> 'f float

    (** [atan m x] is the floating point number closest to [atan x].  *)
    val atan     : rmode -> 'f float -> 'f float

    (** [atan2 m y x] is the floating point number closest to [atan (y/x)].  *)
    val atan2    : rmode -> 'f float -> 'f float -> 'f float

    (** [sinh m x] is the floating point number closest to [sinh x].  *)
    val sinh     : rmode -> 'f float -> 'f float

    (** [cosh m x] is the floating point number closest to [cosh x].  *)
    val cosh     : rmode -> 'f float -> 'f float

    (** [tanh m x] is the floating point number closest to [tanh x].  *)
    val tanh     : rmode -> 'f float -> 'f float

    (** [asinh m x] is the floating point number closest to [asinh x].  *)
    val asinh    : rmode -> 'f float -> 'f float

    (** [acosh m x] is the floating point number closest to [acosh x].  *)
    val acosh    : rmode -> 'f float -> 'f float

    (** [atanh m x] is the floating point number closest to [atanh x].  *)
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
      only those theories that have a context that is a subset of
      provided will be instantiated.

      @param requires is a set of semantic features that are
      required. Defaults to the set of all possible features, i.e., if
      unspecified, then all instances applicable to the context will
      be loaded.
  *)
  val instance :
    ?context:string list ->
    ?requires:string list ->
    unit -> (module Core)


  (** [declare name s] structure [s] as an instance of the Core
      Theory.

      @param context defines a context in which a theory is
      applicable.

      @param provides is a set of semantic tags that describe
      capabilities provided by the theory. The fully qualified name of
      the theory is automatically provided.
  *)
  val declare :
    ?desc:string ->
    ?context:string list ->
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
