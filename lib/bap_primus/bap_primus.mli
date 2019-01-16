open Core_kernel
open Regular.Std
open Monads.Std
open Bap_future.Std
open Bap_strings.Std

open Bap_knowledge
open Bap_core_theory

module Std : sig
  (** Primus - The Microexecution Framework.


      Primus is a microexecution framework that can be used to build
      CPU and full system emulators, symbolic executers, static
      fuzzers, policy checkers, tracers, quickcheck-like test suites,
      etc.

      The underlying idea is quite simple - Primus interprets a lifted
      program. The interpreter provides a set of extension points
      through which it is possible to observe what is happening inside
      the interpreter, and even to change the interpreter
      behavior. This extension points are called "observations" in
      Primus parlance. A simple publish/subscriber architecture is
      used to watch for the interpreter events, where subscribers are
      allowed to arbitrary change the interpreter state.

      A novel idea is that the interpreter is non-deterministic
      in the same sense as a non-deterministic Turing machine. That
      means that any computation may have more than one result. Every
      time there is a non-determinism in the computation the machine
      state is cloned. Different scheduling policies mixed with
      different non-deterministic startegies provide an analyst a vast
      selection of avenues to investigate.

      Primus is build around an idea of a component base linearly
      extensible interpreter. That means, that an analysis can be
      built from basic building blocks, with minimal coupling between
      them. The central component is the Interpreter itself. It
      evaluates a program and interacts with three other components:
       - Linker
       - Env
       - Memory

      The Linker is responsible for linking code into the program
      abstraction. The [Env] component defines the environment
      behavior, i.e., variables. Finally, the [Memory] component is
      responsible for the memory representation.

      Primus framework is implemented as a monad transformer that
      wraps any monad into the [Machine] monad. The [Machine] monad
      denotes a computation, and is implemented as a composition of
      state, exception, and continuation passing monad.

      Each user component is a functor that is parametrized by a
      Machine monad. It is require to provide only one function -
      [init]. Usually, this function subscribes to observations, but
      it can modify other components (depending on their interface).

   *)
  [@@@warning "-D"]

  module Primus : sig
    (** Machine Exception.

        The exn type is an extensible variant, and components
        usually register their own error constructors. *)
    type exn = ..

    (** [an observation] of a value of type [an].*)
    type 'a observation

    (** [a statement] is used to make an observation of type [a].    *)
    type 'a statement

    (** a result of computation  *)
    type value [@@deriving bin_io, compare, sexp]


    type word = Bap.Std.word
    type addr = word

    (** Machine exit status.
        A machine may terminate normally, or abnormally with the
        specified exception. *)
    type exit_status =
      | Normal
      | Exn of exn

    (** value generator  *)
    type generator

    (** Machine Observation.

        The Primus Framework is built on top of the Machine
        observation. The Machine components make their own
        observations, based on observation made by other components.

        A value of type ['a observation] is a first-class
        representation of an event of type ['a]. While machine
        components are functors, the values of type observation should
        not depenend on the type of the functor.*)
    module Observation : sig

      (** An observation provider.
          A provider facilitates introspection of the Primus Machine,
          for the sake of debugging and dumping the effects. The
          provider shoud not (and can't be) used for affecting the
          behavior of a machine, or for the analysis, as its main
          purpose is debugging, logging, and tracing the execution.*)
      type provider


      (** [provide ?inspect name] returns a pair of two handlers. The
          first element is used to observe values, the second is used
          to provide values for the observation.

          The [inspect] function may provide a sexp representation of
          an observed value, that will be used for introspection and
          pretty-printing (it is not required, and if it is provided, it
          is not necessary to disclose everything *)
      val provide : ?inspect:('a -> Sexp.t) -> string -> 'a observation * 'a statement


      (** [name observation] is a name of the observed attribute.  *)
      val name : 'a observation -> string


      (** [inspect observation value] returns a sexp representation of
          an observed [value] *)
      val inspect : 'a observation -> 'a -> Sexp.t

      (** enumerate all currently available observation providers  *)
      val list_providers : unit -> provider list


      (** Data interface to the provider.

          This interface provides access to the data stream of all
          providers expresses as a stream of s-expressions.
       *)
      module Provider : sig
        type t = provider

        (** unique name of a provider *)
        val name : t -> string

        (** a total number of observers that subscribed to this provider  *)
        val observers : t -> int

        (** triggers a stream of occurences of this observation  *)
        val triggers : t -> unit stream

        (** a data stream from this observation *)
        val data : t -> Sexp.t stream
      end
    end

    (** Primus Machine.

        The Machine is the core of Primus Framework.  The Machine
        behavior is extended/changed with Machine Components. A
        component is a functor that takes a machine instance, and
        registers reactions to different events, that can happen
        during the machine evaluation. Events can be obtained from the
        observations made by the core components of the Machine, such
        as the Interpreter, or by other components, if their
        implementors provide any observations.

        A machine is usually instantiated and ran only once. For
        example, the [run] analysis creates a machine parameterized by
        the static model of a binary and runs a machine from the
        specified entry point, until it terminates.

        The user analysis is usually written in a form of a component,
        and is registered with the [register_component] function.*)
    module Machine : sig

      (** [init] event occurs just after all components have been
          initialized, and before the execution starts*)
      val init : unit observation

      (** The [finished] event occurs when the machine terminates.   *)
      val finished : unit observation

      (** [exn_raised exn] occurs every time an abnormal control flow
          is initiated *)
      val exn_raised : exn observation


      (** Machine identifier type.   *)
      type id = Monad.State.Multi.id

      (** Machine State.

          Any component can have its own state. In fact, components
          can have a global state and a local state.

          The Primus Machine is an implementation of the
          Non-deterministic abstract machine, and thus can have more
          than one state. Basically, every time a non-deterministic
          event happens a machine can be forked (cloned). The [Global]
          state is never replicated, and a machine can have only one
          global state, that is shared across all clones of a machine,
          and can be used as a communication channel between the
          clones. The [local] state is duplicated at each clone. *)
      module State : sig


        (** ['a t] is a type of state that holds a value of type
            ['a], and can be constructed from the base context of type
            ['c]. *)
        type 'a t

        type 'a state = 'a t

        (** a type that has no values *)
        type void


        (** [uuid] is a string literal representing an UUID.

            It should have the form:

            [XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX],

            where [X] is a hex-digit, e.g.,

            [53dcf68a-c7c8-4915-ae38-9f5b6f574201] *)
        type uuid = (void,void,void) Caml.format



        (** [declare ~inspect ~uuid ~name make] declares a state with
            the given [uuid] and [name]. The name is not required to be
            unique, while [uuid] is obviously required to be unique.

            See [uuid] type description for the uuid representation. A
            new [uuid] can be obtained in the Linux system is provided
            by the [uuidgen] command.*)
        val declare :
          ?inspect:('a -> Sexp.t) ->
          uuid:uuid ->
          name:string ->
          (unit -> 'a) -> 'a t


        (** [inspect state value] introspects given [value] of the state.  *)
        val inspect : 'a t -> 'a -> Sexp.t


        (** [name state] a state name that was given during the construction.  *)
        val name : 'a t -> string
      end


      type 'a state = 'a State.t


      (** An interface to the state.

          An interface gives an access to operations that query and
          modify machine state. *)
      module type State = sig
        type 'a m
        type 'a t

        (** [get state] extracts the state.  *)
        val get : 'a t -> 'a m

        (** [put state x] saves a machine state  *)
        val put : 'a t -> 'a -> unit m

        (** [update state ~f] updates a state using function [f]. *)
        val update : 'a t -> f:('a -> 'a) -> unit m
      end

      (** The Machine interface.*)
      module type S = sig

        (** the machine computation  *)
        type 'a t


        (** an external monad in which the machine computation is wrapped  *)
        type 'a m

        (** Observations interface.  *)
        module Observation : sig

          (** [observe obs on_observation] subscribes to the given
              observation [obs]. Every time the observation [obs] is
              made a function [on_observation] is called. The
              function can perform arbitrary computations in the
              machine monad, e.g., make its own computations, or access
              other components via their interfaces.  *)
          val observe : 'a observation -> ('a -> unit t) -> unit t


          val watch : Observation.provider -> (Sexp.t -> unit t) -> unit t

          (** [make observation event] make an [observation] of the
              given [event].  *)
          val make : 'a statement -> 'a -> unit t
        end


        (** Computation Syntax.*)
        module Syntax : sig
          include Monad.Syntax.S with type 'a t := 'a t


          (** [event >>> action] is the same as
              [Observation.observe event action] *)
          val (>>>) : 'a observation -> ('a -> unit t) -> unit t
        end



        include Monad.State.Multi.S with type 'a t := 'a t
                                     and type 'a m := 'a m
                                     and type id := id
                                     and module Syntax := Syntax
                                     and type 'a e =
                                                (exit_status * 'a) m

        (** Local state of the machine.  *)
        module Local  : State with type 'a m := 'a t
                               and type 'a t := 'a state


        (** Global state shared across all machine clones.  *)
        module Global : State with type 'a m := 'a t
                               and type 'a t := 'a state


        (** [raise exn] raises the machine exception [exn], intiating
            an abonormal control flow *)
        val raise : exn -> 'a t


        (** [catch x f] creates a computation that is equal to [x] if
            it terminates normally, and to [f e] if [x] terminates
            abnormally with the exception [e]. *)
        val catch : 'a t -> (exn -> 'a t) -> 'a t

        (** [args] program command line arguments  *)
        val args : string array t


        (** [envp] program environment variables.   *)
        val envp : string array t

      end

      (** Machine component interface.

          A machine component is a functor, that is applied every time
          the Machine is instantiated. The [init] function is called
          when the Machine computation is started (the order in which
          components are initialized is not specified, but since all
          components store their state in the machine it doesn't
          matter).

          The [init] function can perform any computation in the
          machine monad. But usually, it registers event
          observations.*)
      module type Component = functor (Machine : S) -> sig


                                (** [init ()] component initialization function. *)
                                val init : unit -> unit Machine.t
                              end


      (** The Machine component.  *)
      type component = (module Component)

      (** [Make(Monad)] a monad transformer that wraps the Machine
          into an arbitrary [Monad].  *)
      module Make(M : Monad.S) : S with type 'a m := 'a M.t

      (** [add_component comp] registers a machine component in the
          Primus Framework.  *)
      val add_component : component -> unit
    end

    (** type abbreviation for the Machine.state  *)
    type 'a state = 'a Machine.state


    (** The Machine component.  *)
    type component = Machine.component

    (** A result of computation.

        Each computation that terminates normally produces a machine
        word that has a unique identifier. Basically, [value] is an
        abstract pair, that consists of the [word] and an identifier.
     *)
    module Value : sig
      type id [@@deriving bin_io, compare, sexp]
      module Id : Regular.S with type t = id

      type t = value [@@deriving bin_io, compare, sexp]


      (** [to_word x] projects [x] to a machine word  *)
      val to_word : t -> word

      (** [id value] returns the [value] identifier *)
      val id : t -> id


      (** [Make(Machine)] provides an interface to the Value type
          lifted into the [Machine] monad.  *)
      module Make(Machine : Machine.S) : sig
        type t = value
        type 'a m = 'a Machine.t


        (** [id x] is a unique identifier of a value. Every
            evaluation of non-trivial computation produces a value
            with new identifier. Only seting and reading a variable
            preserves value identifiers. Each new constaint or
            arithmentic, or memory expression produces a value with a
            new identifier.   *)
        val id : t -> id

        (** [to_word x] projects [x] to a machine [word]. Note, many
            operations from the [Word] module are lifted into the
            [Machine] monad by this functor, so this operation is not
            usually necessary. *)
        val to_word : t -> word

        (** [of_word x] computes a fresh new value from [x]  *)
        val of_word : word -> t m

        (** [of_string s] computes a fresh new value from a textual
            representation of a machine word [x]. See {!Bap.Std.Word}
            module for more details.  *)
        val of_string : string -> t m

        (** [of_bool x] creates a fresh new value from the boolean [x].  *)
        val of_bool : bool -> t m

        (** [of_int ~width x] creates a fresh new value of the given
            [width] from the integer [x] *)
        val of_int : width:int -> int -> t m

        (** [of_int32 x] creates a fresh new value from [x]  *)
        val of_int32 : ?width:int -> int32 -> t m

        (** [of_int64 x] creates a fresh new value from [x]  *)
        val of_int64 : ?width:int -> int64 -> t m

        (** a fresh new [false] computation  *)
        val b0 : t m

        (** a fresh new [true] computation  *)
        val b1 : t m

        (** [one x] same as [of_word @@ one x]  *)
        val one : int -> t m

        (** [zero x] same as [of_word @@ zero x]  *)
        val zero : int -> t m

        (** [signed x] same as [of_word @@ signed x]  *)
        val signed : t -> t m

        (** [is_zero] is [lift1 Word.is_zero]  *)
        val is_zero : t -> bool

        (** [is_one] is [lift1 Word.is_one]  *)
        val is_one : t -> bool

        (** [is_positive] is [lift1 Word.is_positive]  *)
        val is_positive : t -> bool

        (** [is_negative] is [lift1 Word.is_negative]  *)
        val is_negative : t -> bool

        (** [is_non_positive] is [lift1 Word.is_non_positive]  *)
        val is_non_positive : t -> bool

        (** [is_non_negative] is [lift1 Word.is_non_negative]  *)
        val is_non_negative : t -> bool

        (** [bitwidth] is [lift1 Word.bitwidth]  *)
        val bitwidth : t -> int


        (** [extracts ?hi ?lo] is [lift1 (Word.extract ?hi ?lo)]  *)
        val extract : ?hi:int -> ?lo:int -> t -> t m

        (** [concat] is [lift2 Word.concat]  *)
        val concat : t -> t -> t m

        (** [succ] is [lift1 Word.succ]  *)
        val succ : t -> t m

        (** [pred] is [lift1 Word.pred]  *)
        val pred : t -> t m

        (** [nsucc] see {!Word.nsucc}  *)
        val nsucc : t -> int -> t m

        (** [npred] see {!Word.npred}  *)
        val npred : t -> int -> t m


        (** see {!Word.abs}  *)
        val abs : t -> t m

        (** see {!Word.neg}  *)
        val neg : t -> t m

        (** see {!Word.add}  *)
        val add : t -> t -> t m

        (** see {!Word.sub}  *)
        val sub : t -> t -> t m

        (** see {!Word.mul}  *)
        val mul : t -> t -> t m

        (** see {!Word.div}  *)
        val div : t -> t -> t m

        (** see {!Word.modulo}  *)
        val modulo : t -> t -> t m

        (** see {!Word.lnot}  *)
        val lnot : t -> t m

        (** see {!Word.logand}  *)
        val logand : t -> t -> t m

        (** see {!Word.logor}  *)
        val logor : t -> t -> t m

        (** see {!Word.logxor}  *)
        val logxor : t -> t -> t m

        (** see {!Word.lshift}  *)
        val lshift : t -> t -> t m

        (** see {!Word.rshift}  *)
        val rshift : t -> t -> t m

        (** see {!Word.arshift}  *)
        val arshift : t -> t -> t m


        (** Int-like syntax.  *)
        module Syntax : sig

          (** see {!Word.(~-)}  *)
          val ( ~-) : t -> t m

          (** see {!Word.(+)}  *)
          val ( + ) : t -> t -> t m

          (** see {!Word.(-)}  *)
          val ( - ) : t -> t -> t m

          (** see {!Word.( * )}  *)
          val ( * ) : t -> t -> t m

          (** see {!Word.(/)}  *)
          val ( / ) : t -> t -> t m

          (** see {!Word.(mod)}  *)
          val (mod) : t -> t -> t m

          (** see {!Word.(lor)}  *)
          val (lor) : t -> t -> t m

          (** see {!Word.(lsl)}  *)
          val (lsl) : t -> t -> t m

          (** see {!Word.(lsr)}  *)
          val (lsr) : t -> t -> t m

          (** see {!Word.(asr)}  *)
          val (asr) : t -> t -> t m

          (** see {!Word.(lxor)}  *)
          val (lxor) : t -> t -> t m

          (** see {!Word.(land)}  *)
          val (land) : t -> t -> t m
        end


        (** Symbol Value Isomorphism.

            A value can have a symbolic representation that is usefull
            to embed analysis in the machine computation. We inject
            symbols, represented with the [string] data type, into the
            value, using interning, i.e., each symbol is mapped to its
            index (see the Index module).

            The relation between values and symbols is not bijective,
            since not all values represent interned symbols, moreover
            it depends on the order of statements, i.e., a symbol shall
            be interned (with the [to_value] call) before it can be
            translated back into a symbolic representation.

            Implementors of Primus components are encouraged to use the
            [Index] module and implement their own mapping with bijection
            enforced by the abstraction.
         *)
        module Symbol : sig


          (** [to_value sym] returns a value corresponding to the
              provided symbolic representation.  *)
          val to_value : string -> value Machine.t



          (** [of_value v] returns a symbolic representation of the
              value [v].

              If the symbolic representation of a value wasn't
              established, then returns an empty string. *)
          val of_value : value -> string Machine.t
        end

        include Regular.S with type t := t
      end


      (** Indexing strings by values.   *)
      module Index : sig

        (** the width of keys in the index.   *)
        val key_width : int
        include Strings.Index.Persistent.S with type key := value
      end

      include Regular.S with type t := t

    end


    (** Machine Linker.

        The Linker dynamically extends program with the new code.

        The code is represented as a functor that performs a
        computation using a provided machine.*)
    module Linker : sig

      type name = Label.t

      (** Call tracing.

          Linker doesn't operate in terms of functions or subroutines,
          but rather in terms of executable chunks of code. It is
          convenient, though, to track called functions, i.e., there
          are names and arguments (data-flow). Since a code in Primus
          is an uniterpreted computation it is the responsibility of
          the code provider to make corresponding observations, when a
          subroutine is entered or left.

          By default, the code is provided by the BIR Interpeter and
          Primus Interpreter. Both care to provide corresponding
          observations. However, the Primus Lisp Interpreter provides call
          observations only when an externally visible function is
          called, e.g., malloc, free.
       *)
      module Trace : sig

        (** occurs when a subroutine is called.
            Argument values are specified in the same order in which
            corresponding input arguments of a corresponding subroutine
            term are specified.

            Example,

            (call (malloc 4))
         *)
        val call : (string * value list) observation

        (** occurs just before a subroutine returns.

            Context-wise, an observation is made when the interpreter is
            still in the subroutine. The argument list are in the same
            order as arguments of a corresponding subroutine. Values of
            all arguments are provided, including output and input
            arguments.

            Example,

            (call-return (malloc 4 0xDEADBEEF))
         *)
        val return : (string * value list) observation

        (** {3 Notification interface}

            Use [Machine.Observation.make] function, where [Machine]
            is a module implementing [Machine.S] interface, to provide
            observations.
         *)

        (** the statement that makes [call] observations. *)
        val call_entered : (string * value list) statement

        (** the statement that makes [return] observations  *)
        val call_returned : (string * value list) statement

      end

      (** The Linker error  *)
      type exn += Unbound_name of name


      (** occurs before a piece of code is executed *)
      val exec : name observation

      (** occurs when an unresolved name is called, just before the
            unresolved trap is signaled. Could be used to install the
            trap handler.

            @since 1.5 *)
      val unresolved : name observation

      (** [unresolved_handler] is called instead of an unbound name.

          @since 1.5

       *)
      val unresolved_handler : string

      module Name : Regular.S with type t = name


      (** Code representation.

          A code representation is abstract and hides how the code
          itself is represented. It is just a function, that takes a
          machine and performs a computation using this machine.*)
      module type Code = functor (Machine : Machine.S) -> sig

                           (** [exec] computes the code.  *)
                           val exec : unit Machine.t
                         end


      (** code representation  *)
      type code = (module Code)

      (** [Make(Machine)] parametrize the [Linker] with the [Machine].

          Note that the Linker, as well as all other Primus Machine
          components, is stateless, i.e., the functor itself doesn't
          contain any non-syntactic values and thus it is purely
          functional. All the state is stored in the [Machine]
          state. Thus it is absolutely safe, and correct, to create
          multiple instances of components, as they needed. The
          functor instatiation is totaly side-effect free.*)
      module Make(Machine : Machine.S) : sig
        type 'a m = 'a Machine.t

        (** [link ~addr ~name ~tid code] links the given [code]
            fragment into the Machine. The code can be invoked by one
            of the provided identifier. If no idetifiers were
            provided, then apparently code will not be ever invoked. If
            an identifier was alread bound to some other code
            fragment, then the old binding will be substituted by the new
            one.  *)
        val link : name -> code -> unit m

        (** [unlink name] removes code linked with the provided [name].

            Also, removes all aliases of the given [name]. *)
        val unlink : name -> unit m

        (** [lookup name] returns code linked with the given [name].  *)
        val lookup : name -> code option m

        (** [exec name] executes a code fragment associated with the
            given name. Terminates the computation with the
            [Linker.Unbound_name name] condition, if the [name] is not
            associated with any code fragment.  *)
        val exec : name -> unit m


        (** [resolve_addr name] returns the address associated with the
            given [name].  *)
        val resolve_addr : name -> addr option m


        (** [resolve_symbol name] returns the symbolic name associated
            with the given [name].

            @since 1.5.0
         *)
        val resolve_symbol : name -> string option m

        (** [is_linked name] computes to [true] if the [name] is
            associated with some code.

            @since 1.5.0 *)
        val is_linked : name -> bool m
      end
    end


    (** Iterator is a sequence of values of some domain.

        Iterator is a just another abstraction that represent a
        sequence of values.  *)
    module Iterator : sig

      (** Base interface of all iterators.  *)
      module type Base = sig

        (** iterator type  *)
        type t

        (** iterator domain  *)
        type dom

        (** minimum value in the iterator domain  *)
        val min : dom

        (** maximum value in the iterator domain  *)
        val max : dom

        (** current value  *)
        val value : t -> dom
      end


      (** Finite iterators produce finite sequences of values.  *)
      module type Finite = sig
        include Base


        (** [next iterator] moves an [iterator] to the next element of
            the sequence, or returns [None] if there are no more
            elements.*)
        val next : t -> t option
      end


      (** Inifinite iterators produces infinite sequences.  *)
      module type Infinite = sig
        include Base

        (** [next iterator] moves the iterator to the next element of
            the sequence.  *)
        val next : t -> t
      end
    end


    (** Value generators *)
    module Generator : sig
      type t = generator [@@deriving sexp_of]


      (** [create (module Iterator) seed] creates a integer generator
          from the provided [Iterator], and initializes it with the
          given seed.  *)
      val create :
        (module Iterator.Infinite
                with type t = 'a
                 and type dom = int) -> 'a -> t


      (** [static value] returns a generator that always produces the
          same [value].  *)
      val static : int -> t

      (** [unfold ~min ~max ~seed ~f] creates a generator that
          generates values by applying a function [f] to a pair of
          a generator state and previous value.   *)
      val unfold : ?min:int -> ?max:int -> ?seed:int ->
                   f:('a * int -> 'a * int) -> 'a -> t


      (** Random Number Generators  *)
      module Random : sig


        (** [lcg ~min ~max seed] a linear congruential generator, that
            produces a sequence of pseudorandom values that lies in the
            range between [min] and [max] (all inclusive).

            @param min (defaults to 0)
            @param max (defaults to 1^30)
         *)
        val lcg : ?min:int -> ?max:int -> int -> t


        (** [byte seed] the same as [lcg ~min:0 ~max:255 seed]  *)
        val byte : int -> t


        (** Self seeded generators.

            These generators will be seeded by a value derived from
            the Machine clone identifier.  *)
        module Seeded : sig

          (** [create init] creates a self-seeded generator from a
              regular generator.  *)
          val create : (int -> t) -> t


          (** [lcg ~min ~max ()] a linear congruential generator.  *)
          val lcg : ?min:int -> ?max:int -> unit -> t


          (** [byte] is the same as [lcg ~min:0 ~max:255 ()]  *)
          val byte : t
        end

      end


      (** [Make(Machine)] lifts the generator interface into the
          Machine monad.  *)
      module Make( Machine : Machine.S) : sig

        (** [next iter] switches the internal state of [iter] to the
            next state and returns the current value *)
        val next : t -> int Machine.t


        (** [word iter bitwidth] constructs a word of the given [bitwidth],
            with bytes obtained from consequitive calls to [next].*)
        val word : t -> int -> word Machine.t
      end
    end

    (** Evaluation environemnt.

        The Environment binds variables to values.*)
    module Env : sig

      (** A variable is undefined, if it was never [add]ed to the
          environment.  *)
      type exn += Undefined_var of Var.ident


      (** [Env = Make(Machine)]  *)
      module Make(Machine : Machine.S) : sig

        (** [get var] returns a value associated with the variable.
            Todo: it looks like that the interface doesn't allow
            anyone to save bottom or memory values in the environemnt,
            thus the [get] operation should not return the
            [Bil.result].*)
        val get : Var.ident -> value Machine.t

        (** [set var value] binds a variable [var] to the given [value].  *)
        val set : Var.ident -> value -> unit Machine.t


        (** [add var generator] adds a variable [var] to the
            environment. If a variable is read before it was defined
            with the [set] operation, then a value produces by the
            generator will be automatically associated with the
            variable and returned. *)
        val add : Var.ident -> Generator.t -> unit Machine.t


        (** [all] is a sequence of all variables defined in the
            environment. Note, the word _defined_ doesn't mean
            initialized.   *)
        val all : Var.ident seq Machine.t
      end
    end


    (** Machine Memory.

        Provides storage facilities. A machine can have multiple memories,
        e.g., RAM, ROM, HDD, cache, register files, etc. They are all accessed
        via the unified memory inteface using [get] and [set]  primitives wich
        read and store bytes from the current memory. The current memory could
        be switched with the [switch] operation and its descriptor could be
        queried using the [memory] operation.

        Each memory device has an independent address space and address bus width
        (which could be different from the virtual memory address size).
        Each memory could be segmented and can have its own TLB, which is usually
        implemented via the [pagefault] handlers.
     *)
    module Memory : sig


      (** abstract memory descriptor, see [Descriptor.t]  *)
      type memory

      (** Abstract memory descriptor.

          A desciptor uniquely identifies a memory device by its name.
          In addition, it holds meta information about memory address
          and data bus sizes.

       *)
      module Descriptor : sig
        type t = memory [@@deriving compare, sexp_of]

        (** [create ~addr_size:m ~data_size:n name] constructs a
            memory descriptor for a storage [name] with [m] lines in
            the address bus, and [n] bits in data. *)
        val create : addr_size:int -> data_size:int -> string -> memory


        (** [unknown ~addr_size:m ~data_size:n] constructs a
            memory descriptor for an arbitrary storage with [m] lines in
            the address bus, and [n] bits in data. *)
        val unknown : addr_size:int -> data_size:int -> memory


        (** [name memory] returns [memory] identifier. *)
        val name : memory -> string

        include Comparable.S with type t := t
      end

      (** occurs when a memory operation for the given addr cannot be satisfied. *)
      type exn += Pagefault of addr


      (** [Make(Machine)] lifts the memory interface into the
          [Machine] monad.  *)
      module Make(Machine : Machine.S) : sig


        (** [switch memory] switches the memory module to [memory].

        All consecutive operations until the next switch will affect
        only this memory.  *)
        val switch : memory -> unit Machine.t


        (** [memory] a descriptor of currently active [memory]  *)
        val memory : memory Machine.t

        (** [get a] loads a byte from the address [a].

            raises the [Pagefault] machine exception if [a] is not mapped.
         *)
        val get : addr -> value Machine.t


        (** [set a x] stores the byte [x] at the address [a].

            raises the [Pagefault] machine exception if [a] is not mapped,
            or not writable.

            Precondition: [Value.bitwidth x = 8].
         *)
        val set : addr -> value -> unit Machine.t

        (** [load a] loads a byte from the given address [a].

            Same as [get a >>= Value.to_word]
         *)
        val load : addr -> word Machine.t

        (** [store a x] stores the byte [x] at the address [a].

            Same as [Value.of_word x >>= set a].

            Precondition: [Value.bitwidth x = 8].
         *)
        val store : addr -> word -> unit Machine.t

        (** [add_text mem] maps a memory chunk [mem] as executable and
            readonly segment of machine memory.*)
        val add_text : addr -> Bigstring.t -> unit Machine.t

        (** [add_data] maps a memory chunk [mem] as writable and
            nonexecutable segment of machine memory.  *)
        val add_data : addr -> Bigstring.t -> unit Machine.t


        (** [allocate addr size] allocates a segment of the specified
            [size]. An unitilialized reads from the segment will
            produce values generated by a generator (defaults to a
            [Generator.Random.Seeded.byte]).

            If [init] is provided then the region is initialized.

            An attempt to write to a readonly segment, or an attemp to
            execute non-executable segment will generate a
            segmentation fault. (TODO: provide more fine-granular traps).*)
        val allocate :
          ?readonly:bool ->
          ?executable:bool ->
          ?init:(addr -> word Machine.t) ->
          ?generator:Generator.t ->
          addr -> int -> unit Machine.t


        (** [map mem] maps a memory chunk [mem] to a segment with the
            given permissions. See also {!add_text} and {!add_data}. *)
        val map :
          ?readonly:bool ->
          ?executable:bool ->
          addr -> Bigstring.t -> unit Machine.t


        (** [is_mapped addr] a computation that evaluates to true,
            when the value is mapped, i.e., it is readable.  *)
        val is_mapped : addr -> bool Machine.t


        (** [is_writable addr] is a computation that evaluates to
            [true] if [addr] is writable.  *)
        val is_writable : addr -> bool Machine.t
      end
    end
  end
end
