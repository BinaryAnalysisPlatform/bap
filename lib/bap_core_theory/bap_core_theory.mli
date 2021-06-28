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

    In the Core Theory all variables are sorted, i.e., they have an
    associated value sort. Variables are also having scope (lexical
    visibility), and extent (lifetime) Finally, variables could be
    mutable or immutable.

    A physical variable is a global mutable variable with the infinite
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
      let () =
        Theory.declare "my-constant-tracker"
          Theory.instance ~require:["bap.std:constant-tracker"] >>=
        Theory.require >>|
        fun (module Base) : Theory.core -> (module struct
          include Base
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

      Theory.instance ()
        ~context:["arm"; "arm-gnueabi"]
        ~requires:[
          "variable-recovery";
          "stack-frame-analysis";
          "structural-analysis";
          "floating-point";
          "bap.std:bil-semantics"
        ] >>=
      Theory.require >>= fun (module Theory) ->

    ]}

    In the example above, theories that are specific to ARM
    architecture, in particular to the arm-gnueabi ABI, will be
    instantiated (in addition to other general theories). The
    [requires] parameter specifies a few semantic tags, describing
    what kind of semantic information is needed, as well as one theory
    explicitly, the [bap.std:bil-semantics], to ensure that each term
    has a BIL denotation.

    References:

    - [1]: http://okmij.org/ftp/tagless-final/JFP.pdf
    - [2]: http://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf
    - [3]: http://okmij.org/ftp/tagless-final/course/optimizations.html


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

  (** The class index for all Core Theories.  *)
  type cls


  (** A theory instance.
      To create a new theory instance use the {!instance} function.
      To manifest a theory into an OCaml module, use the {!require}
      function. *)
  type theory = cls KB.obj


  (** Theory.t is theory.  *)
  type t = theory

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

    type 'a value = 'a t

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


    (** [resort refine x] applies [refine] to the sort of [x].

        Returns the value [x] with the refined sort, if applicable,
        otherwise returns the original value.

        @since 2.3.0
    *)
    val resort : ('a sort -> 'b sort option) -> 'a t -> 'b t option


    (** [forget v] erases the type index of the value.

        The returned value has the monomorphized [Top.t] type and can
        be stored in containers, serialized, etc.

        To restore the type index use the {!refine} function.

        @since 2.3.0

        Note: this is a convenient function that just does
        [Knowledge.Value.refine v @@ Sort.forget @@ sort v]
    *)
    val forget : 'a t -> unit t

    (** A value with an erased sort type index.

        The monomorphized value could be stored in a container,
        serialized and deserialized and otherwise treated as a
        regular value. To erase the type index, use the
        [Value.forget] function.

        The type index could be restored using [Value.refine] or
        [Value.Sort.refine] functions.

        @since 2.3.0
    *)
    module Top : sig
      type t = (cls,unit sort) KB.cls KB.value
      val cls : (cls, unit sort) KB.cls
      include KB.Value.S with type t := t
    end


    (** A eDSL for dispatching on multiple types.

        The syntax involves only two operators, [can] that
        applys a sort refinining function, and [let|]
        glues several cases together. Let's start with a simple
        example,
        {[
          let f x = Match.(begin
              let| x = can Bool.refine x @@ fun x ->
                (* here x has type [Bool.t value] *)
                `Bool x in
              let| x = can Bitv.refine x @@ fun x ->
                (* and here x is ['a Bitv.t value] *)
                `Bitv x in
              let| x = can Mem.refine x @@ fun x ->
                (* and now x is [('a,'b) Mem.t value] *)
                `Mem x in
              `Other x
            end)
        ]}

        In general, the syntax is
        {v
          let| x = can s1 x @@ fun (x : t1) ->
            body1 in
          ...
          let| x = can sN x @@ fun (x : tN) ->
            bodyN in
          default
        v}

        where [s1],...,[sN] a refiners to types [t1],...,[tN],
        respectively.

        {3 Semantics}

        If in [can s1 x body] the sort of [x] can be refined to [t1] using
        the refiner [s1] then [body] is applied to the value [x] with
        the refined sort (and freshly generated type index if
        needed) and the result of the whole expression is [body x]
        and the nested below expressions are never
        evaluated. Otherwise, if there is no refinement, the
        expression [can s1 x body] is evaluated to [()]
        and the next case is tried until the [default] case is hit.

        @since 2.3.0
    *)
    module Match : sig
      type 'a t
      type 'a refiner = unit sort -> 'a sort option
      val (let|) : 'b t -> (unit -> 'b) -> 'b


      (** [let| () = can s x f in can't] refines [x] to [s].

          If the sort of [x] could be refined with [s] then [f]
          is called with the refined value [x'] and the whole
          expression is evaluated to [f x']. Otherwise, the control is
          passed to [can't].
      *)
      val can : 'a refiner -> unit value -> ('a value -> 'b) -> 'b t


      (** [let| () = both sx x sy y f in no] applies two refiners in parallel.

          If both [x] and [y] could be refined with [sx] and [sy]
          respectively then [f x' y'] is called with the refined
          values and becomes the value of the expression. Otherwise,
          [no] is evaluated and becomes the value of the whole
          expression.
      *)
      val both :
        'a refiner -> unit value ->
        'b refiner -> unit value ->
        ('a value -> 'b value -> 'c) -> 'c t
    end


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


      (** Sorts with erased type indices.

          This module enables construction of complex data structures
          on sorts, like maps, sets, etc, e.g.,

          [let sorts = Set.empty (module Value.Sort.Top)]

          Since such structures are required to be monomorphic, the
          sort type index should be removed using the [forget] function,
          before a sort could be stored in it.

          Note, that the type index is only removed from the meta
          language (OCaml) type, but is preserved in the value term,
          so it could be reconstructed (refined) later.
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


    (** [printf "%a" Theory.Var.pp v] pretty-prints the identifier
        of the variable [v].

        @since 2.3.0  *)
    val pp : Format.formatter -> 'a t -> unit

    (** Variable identifiers.

        Identifiers are compared caseless, otherwise the order loosely
        matches the lexicographical order of the textual
        representation. Identifiers of virtual variables are ordered
        before identifiers of physical variables and mutable virtual
        variables are ordered before immutable. Identifiers of a
        versioned variable are ordered in the ascending order of their
        versions. And identifiers of virtual variables are ordered in
        the ascending order of their numeric values, e.g., `#2`
        is ordered before `#123`.

        @before 2.4.0 the ordering was unspecified but wasn't caseless.
        @since 2.4.0 the ordering is caseless
    *)
    module Ident : sig
      type t = ident [@@deriving bin_io, compare, sexp]
      include Stringable.S with type t := t
      include Base.Comparable.S with type t := t
                                 and type comparator_witness = ord
    end


    (** Variables with erased sort index.

        This module enables construction of complex data structures on
        variables, e.g., [Set.empty (module Theory.Var.Top)].

        The variables are ordered by their identifiers so that two
        variables with the same name but different sorts are compared
        equal.
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

  (** The target execution system.
      @since 2.2.0 *)
  type target

  (** The ordering of the bytes.
      @since 2.2.0  *)
  type endianness

  (** The operating system.
      @since 2.2.0 *)
  type system

  (** The application binary interface.
      @since 2.2.0  *)
  type abi

  (** The floating-point ABI.
      @since 2.2.0*)
  type fabi

  (** The file type.
      @since 2.2.0 *)
  type filetype

  (** source to code transformer.
      @since 2.2.0*)
  type compiler

  (** the name of the code encoding.
      @since 2.2.0 *)
  type language

  (** a target-specific role of a variable or other entity.

      @since 2.3.0
  *)
  type role

  (** The semantics of programs.

      The semantics of a program is denoted with effects that this
      program produces, so effectively [Program.Semantics = Effect],
      but we reexport it in a separate module here, to separate the
      concerns.

      @since 2.2.0 (was {!Program.Semantics} before that
  *)
  module Semantics : sig
    type cls = Effect.cls
    type t = unit Effect.t

    (** the class of program semantics values.  *)
    val cls : (cls, unit Effect.sort) Knowledge.cls

    (** the slot to store program semantics.  *)
    val slot : (program, t) Knowledge.slot

    val value : (cls, unit Value.t) Knowledge.slot

    include Knowledge.Value.S with type t := t
  end


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

    module Semantics = Semantics
    [@@deprecated "[since 2020-10] use [Semantics] (without Program)"]

    include Knowledge.Value.S with type t := t
  end


  (** The source code artifact of a compilation unit.

      Contains the information about the source code of a program
      unit. Note, that it is not an attribute of a program that
      denotes the semantics of that program, but an artifact
      that is associated with the compile unit.

      The information about the source code is represented as an
      extesnsible {!KB.Value.t}. To add a new property of the Source
      class use {!KB.Class.property},
        to access existing properties use {!KB.Value.get}
        and {!KB.Value.put}.


      @since 2.2.0
  *)
  module Source : sig
    type cls
    include KB.Value.S with type t = (cls,unit) KB.cls KB.Value.t

    (** the class index for the core-theory:source class  *)
    val cls : (cls,unit) KB.cls

    (** the language of the source code  *)
    val language : (cls,language) KB.slot

    (** the source code text  *)
    val code : (cls,string) KB.slot

    (** the file name of the unit's source code  *)
    val file : (cls,string option) KB.slot
  end

  (** Description of the execution target.

      An abstract description of the system on which a program is
      intended to be run. The description precisely describes various
      architectual and microarchitectual details of the target system,
      and could be extended with further details either internally, by
      adding more fields (and functions to this module) or storing
      those options in [Options.t]; or externally, by maintaining
      finite mappings from [Target.t] to corresponding properties.

      The [Target.t] has a lightweight immediate representation, which
      is portable across OCaml runtime and persistent across versions
      of BAP and OCaml.

      @since 2.2.0  *)
  module Target : sig
    (** the abstract type representing the target.

        This type is a unique identifier of the target,
        represented as [KB.Name.t] underneath the hood.
    *)
    type t = target
    include Base.Comparable.S with type t := t
    include Binable.S with type t := t
    include Stringable.S with type t := t
    include Pretty_printer.S with type t := t

    (** The set of target-specific options.  *)
    type options = (options_cls,unit) KB.cls KB.Value.t and options_cls


    (** [declare ?package name] declares a new execution target.

        The packaged name of the target must be unique and the target
        shall be declared during the module registration (commonly as
        a toplevel definition of a module that implements the target
        support package).

        The newly declared target inherits all the parameters from the
        [parent] target unless they are explicitly overriden.

        For the description of parameters see the corresponding
        accessor functions in this module.

        @since 2.3.0 has the regs optional parameter.
    *)
    val declare :
      ?parent:t ->               (** defaults to [unknown] *)
      ?bits:int ->               (** defaults to [32] *)
      ?byte:int ->               (** defaults to [8]  *)
      ?data:_ Mem.t Var.t ->     (** defaults to [mem : Mem(bits,byte)] *)
      ?code:_ Mem.t Var.t ->     (** defaults to [mem : Mem(bits,byte)] *)
      ?data_alignment:int ->     (** defaults to 8 bit *)
      ?code_alignment:int ->     (** defaults to 8 bit *)
      ?vars:unit Var.t list ->   (** defaults to [[]] *)
      ?regs:(role list * unit Var.t list) list -> (** defaults to [[]] *)
      ?endianness:endianness ->  (** defaults to [Endian.big] *)
      ?system:system ->          (** defaults to [System.unknown]  *)
      ?abi:abi ->                (** defaults to [Abi.unknown] *)
      ?fabi:fabi ->              (** defaults to [Fabi.unknown] *)
      ?filetype:filetype ->      (** defaults to [Filetype.unknown] *)
      ?options:options ->        (** defaults to [Options.empty] *)
      ?nicknames:string list ->  (** defaults to [[]] *)
      ?package:string ->         (** defaults to ["user"] *)
      string -> t

    (** [lookup ?package name] lookups a target with the given [name].

        If [name] is unqualified then it is qualified with the
        [package] (which itself defaults to "user"), otherwise the
        [package] parameter is ignored.

        Returns [None] if the target with the given name wasn't declared.
    *)
    val lookup : ?package:string -> string -> t option

    (** [get ?package name] returns the target with the given [name].

        If the target with the given name wasn't declared raises an
        exception.

        See {!lookup} for the details on the [name] and [package] parameters.
    *)
    val get : ?package:string -> string -> t

    (** [read ?package name] is a synonym for [get ?package name].

        Introduces for the consistency with the [Enum.S] interface.
    *)
    val read : ?package:string -> string -> t

    (** [declared ()] is the list of declared targets.
        The order is unspecified, see also {!families}. The list
        doesn't include the [unknown] target. *)
    val declared : unit -> t list

    (** [unknown] the unknown architecture.

        The [core-theory:unknown] is the ancestor of all other
        architectures with all fields set to defaults as described
        in {!declare}. *)
    val unknown : t

    (** [is_unknown t] is [true] if [t] is equal to [unknown].  *)
    val is_unknown : t -> bool

    (** [name target] is the unique name of the target.  *)
    val name : t -> KB.Name.t

    (** [matches target name] is true if [name] matches either
        the unqualified name of the target itsef or one of its
        ancestors; or if the name matches one of the target
        nicknames. E.g., [matches target "mips"].
    *)
    val matches : t -> string -> bool

    (** [order t t'] the order of [t] and [t'] in the target hierarchy.

        - [order t t'] is [NC] if [t] and [t'] are different and
          neither [t] is an ancestor of [t'] nor [t'] is an ancestor of [t];
        - [order t t'] is [LT] if [t] is an ancestor of [t'];
        - [order t t'] is [GT] if [t'] is an ancestor of [t];
        - [order t t'] is [EQ] if [t] and [t'] are equal.
    *)
    val order : t -> t -> KB.Order.partial

    (** [belongs t t'] iff [order t t'] is [GT] or [EQ].

        The [belongs t] predicate defines a {!family} of targets that
        are derived from the target [t], e.g., [belongs arm] defines
        a set of all ARM targets. *)
    val belongs : t -> t -> bool

    (** [parent t] is the closest ancestor of [t] or [unknown]
        if [t] is unknown  *)
    val parent : t -> t

    (** [parents t] returns an ordered list of [t]'s ancestors.

        The closest ancestor comes first in the list and the last
        element of the list is [unknown].
    *)
    val parents : t -> t list

    (** [family p] returns an ordered list of targets that {!belongs} [p].

        The family members are ordered according to their hierarchy
        with [p] comming first.
    *)
    val family : t -> t list

    (** [partition targets] partitions targets into families.

        The partition is a list where each element is a list of family
        members with the family parent coming as the first member and
        all other members ordered in the ascending order of their
        hierarchy, i.e., for each [p::fs] in [families ()]
        [family p] is [p:fs]. The families itself are partitioned by
        the lexical order of the unqualified names of the family
        parents.
    *)
    val partition : t list -> t list list

    (** [families ()] partitions all declared targets into families,
        i.e., it is [partition @@ declared ()].
    *)
    val families : unit -> t list list

    (** [bits target] is the bitness of the target architecture.

        It is commonly the number of bits of the machine word in the
        given architecture and corresponds to the logical width of the
        data bus.
    *)
    val bits : t -> int

    (** [byte target] is the size of the target's byte. *)
    val byte : t -> int

    (** [data_addr_size target] is the number of bits in the data address.

        The logical size of the data address, which is taken
        from the sort of the keys of the [data] memory variable. *)
    val data_addr_size : t -> int

    (** [code_addr_size target] the size of the program counter.

        The size of the instruction address, which is taken
        from the sort of the keys of the [data] memory variable. *)
    val code_addr_size : t -> int


    (** The required data addresses alignment in bits.

        The target [t] requires that all data addresses are
        multiples of [data_alignment t]-bit words.
    *)
    val data_alignment : t -> int

    (** The required code addresses alignment in bits.

        The target [t] requires that all code addresses are
        multiples of [code_alignment t]-bit words.
    *)
    val code_alignment : t -> int

    (** [data target] the main memory variable. *)
    val data : t -> (unit,unit) Mem.t Var.t

    (** [code target] the code memory variable. *)
    val code : t -> (unit,unit) Mem.t Var.t

    (** [vars target] is the set of all registers and memories.

        The set includes both general-purpose, floating-points, and
        status registers, as well variables that denote memories and
        other entities specific to the target. The set includes
        all variables that were passed to the target definition,
        through [data], [code], [vars], and [regs] variables.

        @see also [regs].

    *)
    val vars : t -> Set.M(Var.Top).t


    (** [regs ?exclude ?roles target] returns a set of registers.

        If the [roles] list is passed then narrows down the list to
        registers that have all the specified roles. If [exclude] is
        specified then excludes all registers that have those roles.

        Example,

        {[regs ~roles:[general; integer] ~exclude:[stack_pointer; frame_pointer]}

        @since 2.3.0
    *)
    val regs :
      ?exclude:role list ->
      ?roles:role list ->
      t -> Set.M(Var.Top).t

    (** [reg target role] returns a register with the given [role].

        Returns a register from a set of registers
        [regs ~roles:[role] ?exlude t]. If the set is not singleton
        and [unique] is [true] (defaults to [false]) returns [None].

        @since 2.3.0
    *)
    val reg : ?exclude:role list -> ?unique:bool -> t -> role -> unit Var.t option


    (** [var target name] returns a target variable with the given name.

        The variable is searched in all variables and registers
        provided in target declaration. The search is O(log(N)).

        @since 2.3.0
    *)
    val var : t -> string -> unit Var.t option


    (** [has_roles t roles v] is true if [v] has all the [roles] in [t].

        @since 2.3.0 *)
    val has_roles : t -> role list -> 'a Var.t -> bool

    (** [endianness target] describes the byte order.

        Describes how multibyte words are stored in the main memory. *)
    val endianness : t -> endianness

    (** [system target] the target operating system.  *)
    val system : t -> system

    (** [abi target] the target application binary interface.  *)
    val abi : t -> abi

    (** [fabi target] the target floating-point ABI.  *)
    val fabi : t -> fabi

    (** [filetype target] the target executable file type.  *)
    val filetype : t -> filetype

    (** [options target] the target-specific set of options.  *)
    val options : t -> options

    (** the domain type class for the targets.

        Targets form a flat domain with [unknown] in the bottom. *)
    val domain : t KB.domain

    (** the persistence type class, derived from [KB.Name.persistent] *)
    val persistent : t KB.persistent

    (** An extensible set of the target options.

        The set is represented with the {!KB.Value.t},
        to add a new property, use {!KB.Class.property},
        to access existing properties use {!KB.Value.get}
        and {!KB.Value.put}.
    *)
    module Options : sig
      type cls = options_cls
      include KB.Value.S with type t = options
      include Binable.S with type t := t
      include Pretty_printer.S with type t := t

      (** the class index of the target properties  *)
      val cls : (cls, unit) KB.cls


      (** the textual representation of the set of properties.  *)
      val to_string : t -> string
    end
  end


  (** A unit of code.

      A unit of code is a generic piece of code, i.e., a set of
      instructions that share some common properties, such as the
      instruction set architecture. The whole set of instructions
      in the knowledge base is partitioned into units, so that each
      instruction belongs to at most one code unit, see the
      {!Label.unit} property.

      @since 2.2.0
  *)
  module Unit : sig

    (** the class of all code units  *)
    type cls

    (** the meta type of the unit object  *)
    type t = cls KB.Object.t


    (** the base class for all units.

        Right now we have only one sort of units, indexed with the
        [unit] type. But later we may introduce more unit sorts.
    *)
    val cls : (cls,unit) KB.Class.t


    (** [for_file name] creates a new unit denoting a file with the
        given [name].

        This function creates a symbol that interns [name] in the
        [file] package and sets the {!path} property to [name].
    *)
    val for_file : string -> t knowledge


    (** [for_region ~lower ~upper] creates a new unit denoting an
        anonymous memory region.

        The [lower] and [upper] labels are interned in the current
        package and the symbol, built from their concatenation, is
        interned in the [region] package. That enables distinguishing
        between anonymous memory regions that belong to different
        projects/files but having intersecting set of addresses,
        provided that every project is setting the current package to
        some unique name.
    *)
    val for_region : lower:word -> upper:word -> t knowledge


    (** [path] is the path of the file from which the unit originates.  *)
    val path : (cls, string option) KB.slot


    (** [bias] is the bias of all addresses in the unit.

        If a unit is biased, then all addresses in this unit have
        [Some bias] with respect to the real addresses in the unit
        representation. To obtain the real address the [bias] shall
        be subtracted from the address that is stored in the knowledge
        base. To get the biased address the [bias] shall be added to
        the real address.

        Any knowledge provider that also operates with the real view
        on the program must take [bias] into account.
    *)
    val bias : (cls, Bitvec.t option) KB.slot

    (** [target] the target on which this unit should be executed.   *)
    val target : (cls, Target.t) KB.slot

    (** [source] the source of this unit. *)
    val source : (cls, Source.t) KB.slot

    (** [compiler] the program that translated the unit from the [source].  *)
    val compiler : (cls, compiler option) KB.slot

    include Knowledge.Object.S with type t := t
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


    (** a possible (and opinionated) name.

        Use this slot when the name of a program is not really known
        or when it is possible that other name providers will have a
        conflicting opinion.  *)
    val possible_name : (program, string option KB.opinions) KB.slot

    (** the interrupt vector of the label.

        Labels could also represent code in interrupt vector
        routines, therefore the might be referenced by a number, not
        by an address of a name.
    *)
    val ivec : (program, int option) KB.slot

    (** the program encoding.

        The language used to encode the program.

        @since 2.2.0
    *)
    val encoding : (program, language) KB.slot

    (** possible aliases under which the label might be known.

        This may include versioned names, demangled names, etc. *)
    val aliases : (program, Set.M(String).t) KB.slot


    (** a compilation unit (file/library/object) to which this label belongs  *)
    val unit : (program, Unit.t option) KB.slot

    (** [target label] is the [Unit.target] of the label's [unit].   *)
    val target : label -> Target.t knowledge

    (** a link is valid if it references a valid program.

        If a link references a memory location which is not
        executable, then it is not valid.
    *)
    val is_valid : (program, bool option) KB.slot


    (** a link is subroutine if it is an entry point to a subroutine.  *)
    val is_subroutine : (program, bool option) KB.slot


    (** [fresh] a fresh label (a shortcut for [KB.create cls]).

        @since 2.4.0   *)
    val fresh : t knowledge

    (** [null] is a shortcut for [KB.null cls].

        @since 2.4.0  *)
    val null : t

    (** [for_addr x] generates a link to address [x].

        It is guaranteed that every call [for_addr ~package x] with
        the same arguments will return the same label.

        The [addr] property of the created object is set to [x].

        If the [package] parameter is not specified then the created
        object is interned in the currently selected package otherwise
        it is interned in the provided [package].

        @since 2.2.0 the [package] parameter is added
        @since 2.2.0 the object is interned in the currently selected package
    *)
    val for_addr : ?package:string -> Bitvec.t -> t knowledge


    (** [for_name x] generates a link to program with linkage name [x].

        It is guaranteed that every call [for_name ~package x] with
        the same arguments will return the same label, which is the
        same object as the [Knowledge.Symbol.intern ?package name].

        The [name] property of the created object is set to [x].

        If the [package] parameter is not specified then the created
        object is interned in the currently selected package otherwise
        it is interned in the provided [package].

        @since 2.2.0 the [package] parameter is added
        @since 2.2.0 the object is interned in the currently selected package

    *)
    val for_name : ?package:string -> string -> t knowledge


    (** [for_ivec x] generates a link to an interrupt service number [x].

        It is guaranteed that every call [for_addr ~package x] with
        the same arguments will return the same label, which is the
        same object as the [Knowledge.Symbol.intern ?package name],
        where [name] is [sprintf "ivec-%x" x].

        The [addr] property of the created object is set to [x].

        If the [package] parameter is not specified then the created
        object is interned in the currently selected package otherwise
        it is interned in the provided [package].

        @since 2.2.0 the [package] parameter is added
        @since 2.2.0 the object is interned in the currently selected package

    *)
    val for_ivec : ?package:string -> int -> t knowledge

    include Knowledge.Object.S with type t := t
  end

  module Enum = KB.Enum [@@deprecated "[since 2021-02] use KB.Enum instead"]



  (** A target-specific role of program entities.

      An extensible enumeration for target and application-specific
      roles of variables, registers, and other entities.

      @since 2.3.0
  *)
  module Role : sig
    type t = role

    (** Common roles for registers.

        This module enumerates the blessed set of the register roles
        that have common meaning among various architectures. Feel
        free to create target-specific register roles and publish them
        via your target support libraries. Look for such
        target-specific roles in the XXX_target modules.

        The register roles are specifically grouped in a single module
        to enable local-opening of this module.
    *)
    module Register : sig


      (** the general purpose register

          The general purpose registers is a class of register that
          are used for arithmetic and logic units. On targets with
          floating-point and vector arithmetic unit this class
          includes the floating-point and vector registers as well.

          To narrow down the list, specify additional roles, e.g., to
          get the list of general-purpose integer arithmetic registers
          use [[general; integer]].
      *)
      val general : t

      (** the special-purpose register.

          The special-purpose register include (but are not limited to)
          status registers and commonly are accessed using special
          instructions (couldn't be the targets of common load/store
          and arithmetic operations of a computation unit).
      *)
      val special : t



      (** the pseudo-register.

          The pseudo-registers do not correspond to a real physical or
          logical register in the instruction set but rather an alias
          or a hard-wired register, such as constant zero or an
          instruction register or a program counter.
      *)
      val pseudo : t

      (** the register is used by the integer arithmetic unit

          This role can be assigned both to general and special
          purpose registers. E.g., to get integer arithemtic status
          control registers use [[special; integer]].  *)
      val integer : t

      (** the register is used by the floating-point arithmetic unit


          This role can be assigned both to general and special
          purpose registers. E.g., to get floating-point arithemtic status
          control registers use [[special; floating]].
      *)
      val floating : t

      (** the register is used by the vector processing unit

          This role can be assigned both to general and special
          purpose registers. E.g., to get floating-point arithemtic status
          control registers use [[special; vector]].
      *)
      val vector : t

      (** the register is used to track the run-time stack  *)
      val stack_pointer : t

      (** the register is used to track stack frames  *)
      val frame_pointer : t

      (** the register holds the return address in subroutine calls   *)
      val link : t

      (** the register is used to store thread-local storage  *)
      val thread : t

      (** the register is used in the privileged mode. *)
      val privileged : t

      (** the register holds a constant and is read-only.  *)
      val constant : t

      (** the constant register that always holds zero.  *)
      val zero : t


      (** the program status register  *)
      val status : t

      (** the zero flag register

          Is set when an arithmetic operation results in zero.*)
      val zero_flag : t


      (** the sign (aka negative) flag register

          Is set when an arithmetic operation results in a negative value.*)
      val sign_flag : t

      (** the carry flag register

          Is set when an arithmetic operation results in carry.*)
      val carry_flag : t

      (** the overflow flag register

          Is set when an arithmetic operation results in overflow.*)
      val overflow_flag : t

      (** the parity flag register

          Is set depending on the parity of the number of bits of a
          recently set value .*)
      val parity_flag : t

      (** the non-CPU register  *)
      val hardware : t

      (** the reserved register with undefined behavior.  *)
      val reserved : t


      (** {3 Calling Conventions} *)

      (** the register is used to pass function arguments  *)
      val function_argument : t

      (** the register is used to return values from functions  *)
      val function_return : t


      (** the register is volatile and should be preserved by the caller  *)
      val caller_saved : t


      (** the register is preserved across calls and must be
          preserved by the callee.   *)
      val callee_saved : t

    end

    include KB.Enum.S with type t := t
  end

  (** The source code language.

      @since 2.2.0 *)
  module Language : KB.Enum.S with type t = language

  (** Defines how multibyte words are stored in the memory.

      The number of variants are essentially infinite, given that
      there is an infinite number of variants of the byte and word
      sizes, but the two orderings are the most common: little and
      big endian. More orderings could be declared when necessary.

      @since 2.2.0  *)
  module Endianness : sig
    include KB.Enum.S with type t = endianness
    (** In the big endian ordering the most significant byte of the
        word is stored at the smallest address.   *)
    val eb : endianness

    (** In the little endian ordering the least significant byte of
        the word is stored at the largest address. *)
    val le : endianness

    (** In the bi-endian order the endianness is essentially
        unspecified and depends on the execution context, e.g.,
        on the status register or memory page descriptor.  *)
    val bi : endianness
  end


  (** The Operating System.
      @since 2.2.0  *)
  module System : KB.Enum.S with type t = system

  (** The Application Binary Interface name.
      @since 2.2.0 *)
  module Abi : KB.Enum.S with type t = abi

  (** The Application Floating-point Binary Interface name.
      @since 2.2.0 *)
  module Fabi : KB.Enum.S with type t = fabi

  (** The file type that is used to pack the unit.
      @since 2.3.0 *)
  module Filetype : KB.Enum.S with type t = filetype

  (** Information about the compiler.

      A compiler is a translator that was used to translate
      the code in this unit from the source to the target
      representation.

      @since 2.2.0
  *)
  module Compiler : sig
    include Base.Comparable.S with type t = compiler
    include Binable.S with type t := t
    include Pretty_printer.S with type t := t

    (** [declare name] declares a compiler.

        The compiler here represents more of a process that compiled
        the unit rather than a specific program, thus it includes
        the configuration parameters and command-line options.
    *)
    val create :
      ?specs:(string * string) list ->
      ?version:string list ->
      ?options:string list ->
      string -> compiler

    (** [version] the compiler version.

        The least specific (aka major) version comes first in the
        list, with more detailed versions added after. The exact
        meaning of the version consituents is specific to the compiler.
    *)
    val version : compiler -> string list

    (** [options] the list of options that were used to compile the unit.

        Options are specified as a list of command-line options with
        possible repetitions. The meaning of the options is specific
        to the compiler.

        Note, that [options] are the options passed to the compiler
        when this compilation unit was compiled from the source, not
        the options of the compiler itsef, for the latter see [specs].
    *)
    val options : compiler -> string list


    (** [specs] the configuration of the compiler.

        A key-value storage of the configuration parameters of the
        compiler itself. *)
    val specs : compiler -> string Map.M(String).t

    (** is the textual representation of the compiler descriptor  *)
    val to_string : compiler -> string
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

    (** [ite c x y] is [x] if [c] evaluates to [b1] else [y].  *)
    val ite : bool -> 'a pure -> 'a pure -> 'a pure
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


  (** The Core Theory signature.

      The Core Theory signature includes operations on booleans, bitvectors,
      floating point numbers, and memories, as well as denotations of
      various control-flow and data-flow effects.
  *)
  module type Core = sig
    include Basic
    include Float
    include Trans
  end


  (** a type abbreviation for the core theory module type.  *)
  type core = (module Core)


  (** The Basic Theory.

      Implements the empty basic theory and provides a module for
      expressing the Basic Theory in terms of the Minimal theory.
  *)
  module Basic : sig


    (** Expresses Basic operations in terms of the Minimal operations.  *)
    module Make(S : Minimal) : Basic


    (** The Empty theory.

        All operations have empty denotations. *)
    module Empty : Basic
  end


  (** The empty theory.  *)
  module Empty : Core

  (** [declare name s] that structure [s] as an instance of the Core
      Theory.

      The qualified with the [package] [name] shall be unique,
      otherwise the declaration fails.

      The [extends] list shall contain all theories that are included
      in the structure [s] (except the [Empty] theory, which is
      the base theory of all core theories, so there is no need to add
      it). Failure to list a theory in the [extends] list will prevent
      optimization during theory instantiation and may lead to less
      efficient theories (when the same denotation is computed twice)
      or even conflicts, when the extension overrides the extended
      theory.

      @param context defines a context in which a theory is
      applicable. The declared theory could be only instantiated if
      the context, provided during the instantiation, contains all
      features specified in the [context] argument.

      @param provides is a set of semantic tags that describe
      capabilities provided by the theory. The fully qualified name of
      the theory is automatically provided.
  *)
  val declare :
    ?desc:string ->
    ?extends:string list ->
    ?context:string list ->
    ?provides:string list ->
    ?package:string ->
    name:string ->
    (module Core) knowledge ->
    unit

  (** [instance ()] creates an instance of the Core Theory.

      The instance is built from all theories that match the context
      specified by the [features] list and provide features specified
      by the [requires] list. If any theory is subsumed by other
      theory, then it is excluded.

      If no instances satisfy this requirement than the empty theory
      is returned. If only one theory satisfies the requirement, then
      it is returned. If many theories match, then a join of all
      theories is computed, stored in the knowledge base, and the
      resulting theory is returned.

      To manifest a theory into an structure, use the [require] function.

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
    unit -> theory knowledge

  (** [require theory] manifests the [theory] as an OCaml structure.

      It is only possible to create an instance of theory using the
      {!instance} function. For example, the following will create an
      theory that is a join all currently declared theories (which are
      not specific to any context),

      {[
        Theory.(instance>=>require) () -> fun (module Core) ->
      ]}

      To a create an instance for a specific context and requirements,

      {[
        Theory.instance ()
          ~context:["arm-gnueabi"; "arm"]
          ~require:["bil"; "stack-frame-analysis"] >>=
        Theory.require >>= fun (module Core) ->
      ]}

      Finally, to manifest a theory with a specific name, specify the
      name of the theory in the list of requirements.

      {[
        Theory.instance ~requires:["bap.std:bil"] () >>=
        Theory.require >>= fun (module Core) ->
      ]}
  *)
  val require : theory -> (module Core) knowledge

  (** Sorts implementing IEEE754 formats.

      This module provides an infinite set of indexed sorts for
      floating point numbers defined in the IEEE754 standard.

      This sorts are not referenced in the Core Theory (or any other
      theory in this library) and provided for user convenience. Any
      other format could be used with the Core Theory.
  *)
  module IEEE754 : sig


    (** a type for IEEE754 format.

        The type is indexed with three indices:
        - ['b] the value of the base;
        - ['e] the size of the exponent;
        - ['t] the size of the significand.
    *)
    type ('b,'e,'t) t


    type ('a,'e,'t) ieee754 = ('a,'e,'t) t


    (** A named tuple of representation parameters.

        The names of parameter are taken from the IEEE 754-1985
        standard section 3.6, table 3.5 on page 13. For the
        convenience, here is their meaning (refer the original
        standard for more detailed definitions):

        - [k] - storage width in bits;
        - [p] - precision in bits;
        - [w] - exponent field width in bits;
        - [t] - trailing significand field width in bits;

        The module provides a set of common predefined formats as well
        as safe constructors to define uncommon formats.
    *)
    type parameters = private {
      base : int;
      bias : int;
      k : int;
      p : int;
      w : int;
      t : int;
    }

    (** Parameters for the binary16 IEEE754 format.  *)
    val binary16 : parameters

    (** Parameters for the binary32 IEEE754 format.  *)
    val binary32 : parameters

    (** Parameters for the binary64 IEEE754 format.  *)
    val binary64 : parameters

    (** Parameters for the binary80 IEEE754 format.  *)
    val binary80 : parameters

    (** Parameters for the binary128 IEEE754 format.  *)
    val binary128 : parameters

    (** Parameters for the decimal32 IEEE754 format.  *)
    val decimal32 : parameters

    (** Parameters for the decimal64 IEEE754 format.  *)
    val decimal64 : parameters

    (** Parameters for the decimal128 IEEE754 format.  *)
    val decimal128 : parameters


    (** [binary N] are parameters for the binary<N> IEEE754 format.

        Where [N] is the storage width in bits, and parameters are
        defined for the following N: 16,80, k*32, for all
        natural k that are greater than 0.
    *)
    val binary : int -> parameters option

    (** [decimal N] are parameters for the decimal<N> IEEE754 format.

        Where [N] is the storage width in bits, and parameters are
        defined for all [N = k*32] where [k] is a natural
        number greater than zero.
    *)
    val decimal : int -> parameters option


    (** IEEE754 sorts.  *)
    module Sort : sig

      (** [define p] defines the IEEE754 format sort. *)
      val define : parameters -> (('b,'e,'t) ieee754,'s) format Float.t Value.sort

      (** [exps s] is the sort of bitvectors for the exponent field.  *)
      val exps : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 'e Bitv.t Value.sort

      (** [sigs s] is the sort of bitvectors for the significand field.  *)
      val sigs : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 't Bitv.t Value.sort

      (** [bits s] is the sort of bitvectors for the storage.  *)
      val bits : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> 's Bitv.t Value.sort

      (** [spec s] is the encoding parameters of the sort [s].  *)
      val spec : (('b,'e,'t) ieee754,'s) format Float.t Value.sort -> parameters
    end
  end



  (** Generates parsers of untyped ASTs into type Core Theory terms.

      The [Parser.Make] functor produces a recursive-descent parser
      for a family of untyped BIL-style languages.

      In general, the problem of parsing an untyped representation into
      richly-typed term, such as the Core Theory term is convoluted if
      not tedious. This module along with the signatures in the
      Grammar module facilitates this task, at least for typical
      intermediate languages such as BIL and its variants.

      The recursive-descent parser has six top-level rules, for
      each supported term sort:
      - [bitv] - produces terms of type ['a bitv];
      - [bool] - produces terms of type [bool];
      - [mem] - produces terms of type [('k,'v) mem];
      - [stmt] - produces terms of type ['a effect];
      - [float] - produces terms of type [('f,'s) float];
      - [rmode] - produces terms of type [rmode].

      The target language is not required to represent all those terms
      or to distinguish types in the same way as this parser do.

      To implement a parser for a target language all five rules must
      be provided, but they could be incomplete or empty.

      Each rule is a function, that takes a parser that recognizes a
      sub-language of a term that is parsed by the function, and the
      expression or statement of the target language. The function has
      to map each non-terminal of the target language to a rule in the
      provided subgrammar, i.e.,

      {v
        let bitv : type t r. (t,exp,r) bitv_parser =
          fun (module S) -> function
          | symbol .. symbol -> S.rule1 symbol .. symbol
          | ..
          | symbol .. symbol -> S.ruleN symbol .. symbol

      v}

      Let's take the BIL parser as a concrete example,

      {[
        let bitv : type t r. (t,exp,r) bitv_parser =
          fun (module S) -> function
            | Cast (HIGH,n,x) -> S.high n x
            | .. -> ..
            | Cast (SIGNED,n,x) -> S.signed n x
            | BinOp(PLUS,x,y) -> S.add x y
            | .. -> ..
            | BinOp(XOR,x,y) -> S.logxor x y
            | Let (v,y,z) when is_bit v -> S.let_bit (Var.name v) y z
            | .. -> ..
            | Let (v,y,z) when is_mem v -> S.let_mem (Var.name v) y z
            | .. -> ..
              (* ill-formed expressions *)
            | Let _
            | Store (_, _, _, _, _)
            | Unknown (_, (Mem _|Unk)) as exp -> fail exp `Bitv; S.error
      ]}

      We omitted most of the rules for brevity, see the full
      definition in the BIL plugin. The mapping is pretty
      straightforward. As this example highlights, the parser is also
      playing the role of a type checker. Expressions that are not valid
      in the given context has to be mapped to the [error] term. The
      term itself doesn't have any payload and works as an abort
      symbol for the parser. Since parser are expected to be
      implemented in a plugin code, they can utilize to full extend
      the interface with the user to provide fancy error messages.

      The context of the expression is automatically inferred by the
      parser generator with the help of the top-level syntactic rules
      provided by the user. In the example above, the context of the
      let-bound expression is determined from the type of the
      variable. For some languages, it might be necessary to write a
      type-inference algorithm.

      Note, that the parser must return a value of type [S.t] which is
      abstract and couldn't be unified with the type of the Core
      Theory term. Therefore, the rule implementer is limited to the
      language that is defined by the transitive closure of the
      sub-grammar that is passed to the function that implements the
      rule. That basically makes this parser infrastructure
      non-extensible, which is alleviated by the fact that provided
      sub-grammars are very rich. The task of writing an extensible
      parser is still a research question.


      {3 Notes on future extensions}

      We're reserving the right to extend the grammar signatures in
      the [Parser.Grammar] module without changing the major version
      of the library, since those modules are designed to be used
      co-inductively (as types of modules that are  provided by the
      library, not required), therefore adding more definitions to
      the grammars which could be used by the library user makes the
      library strictly more powerful (stronger). To prevent any
      issues, it is recommended not to open the provided module (i.e.,
      always use the grammar rules via the dot notation, e.g., S.add),
      since adding a new rule may hide the existing functions in the
      scope if the module is opened. For alternative implementations
      of this parser generator it is recommended not to reuse these
      module types literally but to copy them (or define your own
      rules).

  *)
  module Parser : sig

    (** An untyped grammar for a subset of Core Theory languages.

        This module defines grammars for six sub-languages for each
        sort of Core Theory terms.


        {3 Notation}

        Each rule in the grammar [S] returns a value of type [S.t],
        which is a semantic action that will eventually build a Core
        Theory term of a corresponding to the rule type. This type is
        abstract and is totally co-inductive (i.e., it could also
        produced, there are no grammar rules (functions) that consume
        values of this type.

        Since each grammar rule will eventually build a Core Theory
        term, we will describe rules using the Core Theory
        denotations. The parser generator will invoke recursively the
        top-level rules on each non-terminal. Those rules are
        referenced using the following names:

        - [bitv] - parses the language of bitvectors;
        - [bool] - parses the language of booleans;
        - [mem]  - parses the language of memories;
        - [stmt] - parses the language of statements;
        - [float] - parses the language of floats;
        - [rmode] - parses the language of rounding modes.

        The parsing rules occasionally need to refer to the sorts of
        therms, we use the following short-hand notation for sorts:
        - [bits m = Bitv.define m] - a sort of bitvectors with [m] bits;
        - [Bool.t] - the sort of booleans;
        - [mems k v = Mem.define k v] - a sort of memories;

        Finally, the width associated with the sort [s] is denoted
        with [size s = Bitv.size s].

        Example,

        {[
          (** [add x y] is [add (bitv x) (bitv y)].  *)
          val add : exp -> exp -> t
        ]}

        says that the grammar rule [add] interprets its arguments in
        the [bitv] context (recursively calls the [bitv] function) and
        have the denotation of the [add p q] term of the Core Theory,
        where [p = bitv x] and [q = bitv y].

        {4 Contexts}

        To ensure the freshness of generated variables and to enable a
        higher-order abstract syntax style (a generalization of the De
        Bruijn notation) we wrap each semantic action in a context
        that holds the binding rules.

        The [var] family of grammar rules rename the passed names if
        they are bound in the renaming context, which is denoted with
        [context s] which is a function that returns the name bound to
        [s] in the context or [s] if it is unbound.

        The [let_<T> n ...] and [tmp_<T> n ...] take the old name
        [n], create a fresh variable [n'] and append the [n,n']
        binding to the context in which the grammar rule is invoked,
        denoted with [rule [<non-term>|n->n']], e.g.,

        {[
          [let_reg s x y] is [scoped @@ fun v -> (bitv x) (bitv [y|s->v])]
        ]}

        As a result of the [let_reg] rule applications any free occurrence
        of the variable [s] will be substituted with the freshly
        generated variable [v]. This will ensure alpha-equivalence of
        expressions that use the [let_<T>] forms.

        The [tmp_<T>] rules are basically the same as [let_<T>] except
        that the scope of the freshly created variable is indefinite,
        so these forms could be used to create hygienic symbol
        generators.
    *)
    module Grammar : sig
      type ieee754 = IEEE754.parameters


      (** Bitvectors.  *)
      module type Bitv = sig

        (** an abstract type denoting a Core Theory bitvector term.  *)
        type t


        (** the type of expressions of the target language.  *)
        type exp


        (** an abstract type denoting a Core Theory rounding mode term,   *)
        type rmode


        (** the error term.

            Denotes an ill-typed term. The parsing is immediately
            stopped. *)
        val error : t

        (** [unsigned m x] is [unsinged (bits m) (bitv x)].    *)
        val unsigned : int -> exp -> t

        (** [signed m x] is [signed (bits m) (bitv x)].    *)
        val signed : int -> exp -> t

        (** [high m x] is [high (bits m) (bitv x)].    *)
        val high : int -> exp -> t

        (** [low m x] is [low (bits m) (bitv x)].    *)
        val low : int -> exp -> t

        (** [cast m x y] is [cast (bits m) (bool b) (bitv y)]. *)
        val cast : int -> exp -> exp -> t

        (** [extract m hi lo x] is [extract (bits m) (bitv hi) (bitv lo) (bitv x)].  *)
        val extract : int -> exp -> exp -> exp -> t

        (** [add x y] is [add (bitv x) (bitv y)]  *)
        val add : exp -> exp -> t

        (** [sub x y] is [sub (bitv x) (bitv y)]  *)
        val sub : exp -> exp -> t

        (** [mul x y] is [mul (bitv x) (bitv y)]  *)
        val mul : exp -> exp -> t

        (** [div x y] is [div (bitv x) (bitv y)]  *)
        val div : exp -> exp -> t

        (** [sdiv x y] is [sdiv (bitv x) (bitv y)]  *)
        val sdiv : exp -> exp -> t

        (** [modulo x y] is [modulo (bitv x) (bitv y)]  *)
        val modulo : exp -> exp -> t

        (** [smodulo x y] is [smodulo (bitv x) (bitv y)]  *)
        val smodulo : exp -> exp -> t

        (** [lshift x y] is [lshift (bitv x) (bitv y)]  *)
        val lshift : exp -> exp -> t

        (** [rshift x y] is [rshift (bitv x) (bitv y)]  *)
        val rshift : exp -> exp -> t

        (** [arshift x y] is [arshift (bitv x) (bitv y)]  *)
        val arshift : exp -> exp -> t

        (** [logand x y] is [logand (bitv x) (bitv y)]  *)
        val logand : exp -> exp -> t

        (** [logor x y] is [logor (bitv x) (bitv y)]  *)
        val logor: exp -> exp -> t

        (** [logxor x y] is [logxor (bitv x) (bitv y)]  *)
        val logxor : exp -> exp -> t

        (** [neg x] is [neg (bitv x)]  *)
        val neg : exp -> t

        (** [not x] is [not (bitv x)]  *)
        val not : exp -> t

        (** [load_word m d s a] is [loadw (bits m) (bool d) (mem s) (bitv a)]. *)
        val load_word : int -> exp -> exp -> exp -> t

        (** [load s k] is [load (mem s) (bitv k)]  *)
        val load : exp -> exp -> t

        (** [var s m] is [var (ctxt s) (bits m)] *)
        val var : string -> int -> t


        (** [int x m] is [int (bits m) x]  *)
        val int : word -> int -> t


        (** [unknown m] is [unk (bits m)]  *)
        val unknown : int -> t


        (** [ite c x y] is [ite (bool c) (bitv x) (bitv y)]  *)
        val ite : exp -> exp -> exp -> t


        (** [let_bit s x y] is [scoped @@ fun v -> (bool x) (bitv [y|s->v])].

            Note, the [let_bit] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_bit : string -> exp -> exp -> t

        (** [let_reg s x y] is [scoped @@ fun v -> (bitv x) (bitv [y|s->v])].

            Note, the [let_reg] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_reg : string -> exp -> exp -> t

        (** [let_mem s x y] is [scoped @@ fun v -> (mem x) (bitv [y|s->v])].

            Note, the [let_mem] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_mem : string -> exp -> exp -> t

        (** [let_float s x y] is [scoped @@ fun v -> (float x) (bitv [y|s->v])].

            Note, the [let_float] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_float : string -> exp -> exp -> t

        (** [append x y] is [append s (bitv x) (bitv y)], where

            [s] is [bits (size (sort (bitv x)) + size (sort (bitv x)))].
        *)
        val append : exp -> exp -> t


        (** [concat xs] is [concat (bits (size s * n)) xs],

            where [s] is the sort of the [xs] element,
            and [n] is the total number of elements in [xs].
        *)
        val concat : exp list -> t

        (** [cast_int m r x] is [cast_int (bits m) (rmode r) (float x)]. *)
        val cast_int : int -> rmode -> exp -> t

        (** [cast_sint m r x] is [cast_sint (bits m) (rmode r) (float x)]. *)
        val cast_sint : int -> rmode -> exp -> t

        (** [fbits x] is [fbits (float x)].  *)
        val fbits : exp -> t
      end


      (** Booleans.  *)
      module type Bool = sig

        (** an abstract type denoting a Core Theory boolean term.   *)
        type t

        (** the type of expressions of the target language  *)
        type exp


        (** an ill-formed term.  *)
        val error : t

        (** [eq x y] is [eq (bitv x) (bitv y)].  *)
        val eq : exp -> exp -> t

        (** [neq x y] is [neq (bitv x) (bitv y)].  *)
        val neq : exp -> exp -> t

        (** [lt x y] is [lt (bitv x) (bitv y)].  *)
        val lt : exp -> exp -> t

        (** [le x y] is [le (bitv x) (bitv y)].  *)
        val le : exp -> exp -> t

        (** [slt x y] is [slt (bitv x) (bitv y)].  *)
        val slt : exp -> exp -> t

        (** [sle x y] is [sle (bitv x) (bitv y)].  *)
        val sle : exp -> exp -> t

        (** [var s] is [var Bool.t (ctxt s)].  *)
        val var : string -> t

        (** [int x] is [b0] is [Bitvec.equal x 0] else [b1].  *)
        val int : word -> t

        (** [unknown ()] is [unk Bool.t]  *)
        val unknown : unit -> t

        (** [ite c x y] is [ite (bool x) (bool x) (bool y)]  *)
        val ite : exp -> exp -> exp -> t

        (** [let_bit s x y] is [scoped @@ fun v -> (bool x) (bool [y|s->v])].

            Note, the [let_bit] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_bit : string -> exp -> exp -> t

        (** [let_reg s x y] is [scoped @@ fun v -> (bitv x) (bool [y|s->v])].

            Note, the [let_reg] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_reg : string -> exp -> exp -> t

        (** [let_mem s x y] is [scoped @@ fun v -> (mem x) (bool [y|s->v])].

            Note, the [let_mem] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_mem : string -> exp -> exp -> t

        (** [let_float s x y] is [scoped @@ fun v -> (float x) (bool [y|s->v])].

            Note, the [let_float] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_float : string -> exp -> exp -> t

        (** [high x] is [msb (bitv x)].  *)
        val high : exp -> t

        (** [low x] is [lsb (bitv x)].  *)
        val low : exp -> t

        (** [extract p x] is [extract (const p) (const p) (bitv x)].  *)
        val extract : int -> exp -> t

        (** [not x] is [inv (bool x)].  *)
        val not : exp -> t

        (** [logand x y] is [and_ (bool x) (bool y)]  *)
        val logand : exp -> exp -> t

        (** [logor x y] is [or_ (bool x) (bool y)]  *)
        val logor: exp -> exp -> t

        (** [logxor x y] is [xor_ (bool x) (bool y)]  *)
        val logxor : exp -> exp -> t

        (** [is_inf x] is [is_inf (float x)]  *)
        val is_inf : exp -> t

        (** [is_nan x] is [is_nan (float x)]  *)
        val is_nan : exp -> t

        (** [is_fzero x] is [is_fzero (float x)]  *)
        val is_fzero : exp -> t

        (** [is_fpos x] is [is_fpos (float x)]  *)
        val is_fpos : exp -> t

        (** [is_fneg x] is [is_fneg (float x)]  *)
        val is_fneg : exp -> t

        (** [fle x y] is [p < q \/ p = q],

            where [p = float x],
              and [q = float y],
              and [p < q] if [forder p q],
              and [r \/ s] is [or_ r s],
              and [p = q] if (not (p < q) /\ not (q < p)),
              and [r /\ s] is [and_ r s],
              and [not r] is [inv r].
        *)
        val fle  : exp -> exp -> t

        (** [flt x y] is [forder (float x) (float y)]  *)
        val flt  : exp -> exp -> t

        (** [feq x y] is [x = y],

            where [p = q] if (not (p < q) /\ not (q < p)),
              and [p < q] if [forder p q],
              and [r /\ s] is [and_ r s],
              and [not r] is [inv r].
        *)
        val feq  : exp -> exp -> t
      end


      module type Mem = sig

        (** an abstract type denoting a Core Theory memory term.   *)
        type t

        (** the type of expressions of the target language  *)
        type exp

        (** an ill-formed term.  *)
        val error : t

        (** [store s k x] is [store (mem s) (bitv k) (bitv x)] *)
        val store : exp -> exp -> exp -> t

        (** [store_word d s k x] is [storew (bool d) (mem s) (bitv k) (bitv x)].  *)
        val store_word : exp -> exp -> exp -> exp -> t

        (** [var s m n] is [var (ctxt s) (mems (bits m) (bits n))]   *)
        val var : string -> int -> int -> t

        (** [unknown m n] is [unk (mems (bits m) (bits n))]  *)
        val unknown : int -> int -> t

        (** [ite c x y] is [ite (bool c) (mem x) (mem y)]  *)
        val ite : exp -> exp -> exp -> t

        (** [let_bit s x y] is [scoped @@ fun v -> (bool x) (mem [y|s->v])].

            Note, the [let_bit] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_bit : string -> exp -> exp -> t

        (** [let_reg s x y] is [scoped @@ fun v -> (bitv x) (mem [y|s->v])].

            Note, the [let_reg] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_reg : string -> exp -> exp -> t

        (** [let_mem s x y] is [scoped @@ fun v -> (mem x) (mem [y|s->v])].

            Note, the [let_mem] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_mem : string -> exp -> exp -> t

        (** [let_float s x y] is [scoped @@ fun v -> (float x) (mem [y|s->v])].

            Note, the [let_float] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_float : string -> exp -> exp -> t
      end

      (** Statements.  *)
      module type Stmt = sig

        (** an abstract type denoting effects of the Core Theory.  *)
        type t

        (** the type of expressions of the target language.  *)
        type exp

        (** the type for representing rounding modes in the target language.  *)
        type rmode

        (** the type of statements in the target language.  *)
        type stmt

        (** an ill-formed term.  *)
        val error : t


        (** [set_mem s m n x] is [set v (mem x)],

            where [v = Var.create (mems (bits m) (bits n) s].
        *)
        val set_mem : string -> int -> int -> exp -> t

        (** [set_reg s m x] is [set v (bitv x)],

            where [v = Var.create (bits m) s.
        *)
        val set_reg : string -> int -> exp -> t


        (** [set_bit s x] is [set v (bool x)],

            where [v = Var.create Bool.t s].
        *)
        val set_bit : string -> exp -> t

        (** [set_ieee754 s p x] is [set v (float x)],

            where [v = Var.create (IEEE754.Sort.define p) s].
        *)
        val set_ieee754 : string -> ieee754 -> exp -> t


        (** [set_rmode s x] is [set v (rmode x)],

            where [v = Var.create Rmode.t x]. *)
        val set_rmode : string -> rmode -> t

        (** [tmp_mem s x] is [set v (mem x)],

            where [v] is a freshly created variable,
            and all occurrences of [s] are substituted
            with the identifier of [v] in all subsequent
            statements.
        *)
        val tmp_mem : string -> exp -> t

        (** [tmp_reg s x] is [set v (bitv x)],

            where [v] is a freshly created variable,
            and all occurrences of [s] are substituted
            with the identifier of [v] in all subsequent
            statements.
        *)
        val tmp_reg : string -> exp -> t

        (** [tmp_bit s x] is [set v (bool x)],

            where [v] is a freshly created variable,
            and all occurrences of [s] are substituted
            with the identifier of [v] in all subsequent
            statements.
        *)
        val tmp_bit : string -> exp -> t

        (** [tmp_float s x] is [set v (float x)],

            where [v] is a freshly created variable,
            and all occurrences of [s] are substituted
            with the identifier of [v] in all subsequent
            statements.
        *)
        val tmp_float : string -> exp -> t


        (** [tmp_rmode s x] is [set v (rmode x)],

            where [v] is a freshly created variable,
            and all occurrences of [s] are substituted
            with the identifier of [v] in all subsequent
            statements.
        *)
        val tmp_rmode : string -> rmode -> t


        (** [let_mem s x p] is [seq (set v (mem x)) (stmt p)],

            where [v] is freshly created variable, and all occurrences
            of [s] will be substituted with the identifier of [v] in
            the statement [p].
        *)
        val let_mem : string -> exp -> stmt -> t

        (** [let_reg s x p] is [seq (set v (bitv x)) (stmt p)],

            where [v] is freshly created variable, and all occurrences
            of [s] will be substituted with the identifier of [v] in
            the statement [p].
        *)
        val let_reg : string -> exp -> stmt -> t

        (** [let_bit s x p] is [seq (set v (bool x)) (stmt p)],

            where [v] is freshly created variable, and all occurrences
            of [s] will be substituted with the identifier of [v] in
            the statement [p].
        *)
        val let_bit : string -> exp -> stmt -> t

        (** [let_float s x p] is [seq (set v (float x)) (stmt p)],

            where [v] is freshly created variable, and all occurrences
            of [s] will be substituted with the identifier of [v] in
            the statement [p].
        *)
        val let_float : string -> exp -> stmt -> t

        (** [let_rmode s x p] is [seq (set v (rmode x)) (stmt p)],

            where [v] is freshly created variable, and all occurrences
            of [s] will be substituted with the identifier of [v] in
            the statement [p].
        *)
        val let_rmode : string -> rmode -> stmt -> t


        (** [jmp x] is [jmp (bitv x)],

            where [x] is a non-constant expression.

            If [x] is a constant use [goto].
        *)
        val jmp : exp -> t


        (** [goto x] is [goto lbl],

            where [lbl = Label.for_addr x].
        *)
        val goto :  word -> t


        (** [call x] is [goto lbl],

            where [lbl = Label.for_name x]
        *)
        val call : string -> t

        (** [special s] is [pass].  *)
        val special : string -> t

        (** [cpuexn x] is [goto lbl],

            where [lbl = Label.for_ivec x].
        *)
        val cpuexn : int -> t

        (** [while_ c ps] is [repet (bool c) (map stmt ps)].  *)
        val while_ : exp -> stmt list -> t


        (** [if_ c xs ys] is [branch (bool c) (map stmt xs) (map stmt ys)]. *)
        val if_ : exp -> stmt list -> stmt list -> t

        (** [seq xs] is [map stmt xs]  *)
        val seq : stmt list -> t
      end


      (** Floating point expressions.  *)
      module type Float = sig

        (** an abstract type denoting a Core Theory floating point term.  *)
        type t

        (** the type of expressions of the target language.    *)
        type exp

        (** the type for representing rounding modes in the target language.  *)
        type rmode

        (** an ill-formed term.  *)
        val error : t

        (** [ieee754 p x] is [|float| s (bitv x)],

            where [s = IEEE754.Sort.define p],
              and [|float|] is the [float] operation from the Core Theory.
        *)
        val ieee754 : ieee754 -> exp -> t

        (** [ieee754_var p x] is [var v x],

            where [v = Var.define v s],
              and [s = IEEE754.Sort.define p].
        *)
        val ieee754_var : ieee754 -> string -> t

        (** [ieee754 p x] is [unk s],

            where [s = IEEE754.Sort.define p]. *)
        val ieee754_unk : ieee754 -> t

        (** [ieee754_cast p m x] is [cast_float s (rmode m) (bitv x)],

            where [s = IEEE754.Sort.define p]. *)
        val ieee754_cast : ieee754 -> rmode -> exp -> t

        (** [ieee754_cast_signed p m x] is [cast_sfloat s (rmode m) (bitv x)],

            where [s = IEEE754.Sort.define p]. *)
        val ieee754_cast_signed : ieee754 -> rmode -> exp -> t

        (** [ieee754_convert p m x] is [fconvert s (rmode m) (float x)],

            where [s = IEEE754.Sort.define p]. *)
        val ieee754_convert : ieee754 -> rmode -> exp -> t

        (** [ite c x y] is [ite (bool c) (float x) (float y)]  *)
        val ite : exp -> exp -> exp -> t

        (** [fadd m x y] is [fadd (rmode m) (float x) (float y)]. *)
        val fadd : rmode -> exp -> exp -> t

        (** [fsub m x y] is [fsub (rmode m) (float x) (float y)]. *)
        val fsub : rmode -> exp -> exp -> t

        (** [fmul m x y] is [fmul (rmode m) (float x) (float y)]. *)
        val fmul : rmode -> exp -> exp -> t

        (** [fdiv m x y] is [fdiv (rmode m) (float x) (float y)]. *)
        val fdiv : rmode -> exp -> exp -> t

        (** [frem m x y] is [fmodulo (rmode m) (float x) (float y)]. *)
        val frem : rmode -> exp -> exp -> t

        (** [fmin m x y] is [ite c p q],

            where [p = float x],
              and [q = float y],
              and [c = forder p q].
        *)
        val fmin : exp -> exp -> t

        (** [fmax m x y] is [ite c q p],

            where [p = float x],
              and [q = float y],
              and [c = forder p q].
        *)
        val fmax : exp -> exp -> t


        (** [fabs x] is [fabs (float x)].  *)
        val fabs : exp -> t

        (** [fneg x] is [fneg (float x)].  *)
        val fneg : exp -> t


        (** [fsqrt x] is [fsqrt (float x)].  *)
        val fsqrt : rmode -> exp -> t

        (** [fround x] is [fround (float x)].  *)
        val fround : rmode -> exp -> t

        (** [let_bit s x y] is [scoped @@ fun v -> (bool x) (float [y|s->v])].

            Note, the [let_bit] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_bit : string -> exp -> exp -> t

        (** [let_reg s x y] is [scoped @@ fun v -> (bitv x) (float [y|s->v])].

            Note, the [let_reg] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_reg : string -> exp -> exp -> t

        (** [let_mem s x y] is [scoped @@ fun v -> (mem x) (float [y|s->v])].

            Note, the [let_mem] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_mem : string -> exp -> exp -> t

        (** [let_float s x y] is [scoped @@ fun v -> (float x) (float [y|s->v])].

            Note, the [let_float] rule is not mapped to the [let_] term,
            but instead a scoped fresh variable [v] is created and [s]
            is substituted with [v] in [y]. *)
        val let_float : string -> exp -> exp -> t
      end


      (** Rounding modes.  *)
      module type Rmode = sig

        (** an abstract type denoting a Core Theory rounding mode term.   *)
        type t


        (** the type for representing rounding modes in the target language.  *)
        type exp


        (** an ill-formed rounding mode term.  *)
        val error : t


        (** [rne] is [rne].  *)
        val rne : t

        (** [rtz] is [rtz].  *)
        val rtz : t

        (** [rtp] is [rtp].  *)
        val rtp : t

        (** [rtn] is [trp].  *)
        val rtn : t

        (** [rna] is [rna].  *)
        val rna : t
      end
    end

    (** [bitv grammar exp] parses [exp] using [grammar]. *)
    type ('a,'e,'r) bitv_parser =
      (module Grammar.Bitv with type t = 'a
                            and type exp = 'e
                            and type rmode = 'r) ->
      'e -> 'a

    (** [bool grammar exp] parses [exp] using [grammar]. *)
    type ('a,'e,'r) bool_parser =
      (module Grammar.Bool with type t = 'a
                            and type exp = 'e) ->
      'e -> 'a

    (** [mem grammar exp] parses [exp] using [grammar]. *)
    type ('a,'e) mem_parser =
      (module Grammar.Mem with type t = 'a
                           and type exp = 'e) ->
      'e -> 'a

    (** [stmt grammar stmt] parses [stmt] using [grammar]. *)
    type ('a,'e,'r,'s) stmt_parser =
      (module Grammar.Stmt with type t = 'a
                            and type exp = 'e
                            and type stmt = 's
                            and type rmode = 'r) ->
      's -> 'a

    (** [float grammar exp] parses [exp] using [grammar]. *)
    type ('a,'e,'r) float_parser =
      (module Grammar.Float with type t = 'a
                             and type exp = 'e
                             and type rmode = 'r) ->
      'e -> 'a

    (** [rmode grammar exp] parses [exp] using [grammar]. *)
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


    (** [parser] is a tuple of top-level parsing routines.

        Parser defines a Core Theory denotation of an untyped AST
        with expressions of type ['e], statements of type ['s] and
        rounding modes represented with type ['r].
    *)
    type ('e,'r,'s) parser = ('e,'r,'s) t

    (** [Make(Theory)] parses AST to the specified [Theory] terms.  *)
    module Make(S : Core) : sig


      (** [run parser program] the starting rule of the parser.

          Applies the parser to a sequence of statements and computes
          a denotation of [program] in a Core Theory terms.
      *)
      val run : ('e,'r,'s) parser -> 's list -> unit eff
    end
  end


  (** Documents all declared theories. *)
  module Documentation : sig


    (** Theory documentation.  *)
    module Theory : sig
      (** the documentation  *)
      type t

      (** [name theory] is the fully qualified name of [theory].  *)
      val name : t -> Knowledge.Name.t

      (** [desc theory] is the description provided for [theory].  *)
      val desc : t -> string

      (** [requires theory] is the list of theory requirements.  *)
      val requires : t -> string list

      (** [provides theory] is the list of theory capabilities.  *)
      val provides : t -> string list
    end

    (** [theories ()] the declared theories.  *)
    val theories : unit -> Theory.t list
  end
end
