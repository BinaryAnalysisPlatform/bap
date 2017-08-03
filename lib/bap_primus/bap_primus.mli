open Core_kernel.Std
open Regular.Std
open Bap.Std
open Monads.Std
open Bap_future.Std
open Format

module Std : sig

  module Primus : sig
    (** Machine Exception.

        The exn type is an extensible variant, and components
        usually register their own error constructors. *)
    type exn = ..

    (** [an observation] of a value of type [an].*)
    type 'a observation

    (** [a statement] is used to make an observation of type [a].    *)
    type 'a statement

    type value


    (** Machine exit status.
        A machine may terminate normally, or abnormally with the
        specified exception.
    *)
    type exit_status =
      | Normal
      | Exn of exn

    (** An abstract type that represents an effect produced by a
        Machine run. That type is left abstract, and has no
        operations, as its purpose is to disallow running machine
        directly, withou an instantiation of the [Machine.Main]
        module. *)
    type 'a effect

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



    (** A hierarchical program position.

        The [Level.t] is a cursor-like data structure, that
        describes a program position in the program term hierarchy.*)
    module Pos : sig
      (** uninhabited type  *)
      type nil

      (** the top-most program term.  *)
      type top = program

      (** [(t,p) level] a cursor pointing to a [t term], that is
          nested in the parent cursor [p]. *)
      type ('a,'b) level = {
        me : 'a term;          (** [me] current position *)
        up : 'b;               (** [up] parent cursor *)
      }


      (** the highest level of the hierarchy - a cursor the points
          to the whole program. This is a starting position.  *)
      type level3 = (top,nil) level

      (** a cursor pointing to a function  *)
      type level2 = (sub,level3) level

      (** a level of arguments and basic blocks  *)
      type 'a level1 = ('a,level2) level

      (** a level of the basic terms, e.g., defs, jmps and phi-nodes.  *)
      type 'a level0 = ('a,blk level1) level

      (** a program location  *)
      type t =
        | Top of level3       (** a program *)
        | Sub of level2       (** a subroutine  *)
        | Arg of arg level1   (** subroutine argument *)
        | Blk of blk level1   (** a basic block *)
        | Phi of phi level0   (** a phi-node *)
        | Def of def level0   (** a definition *)
        | Jmp of jmp level0   (** a jump term *)


      (** [tid p] is term identifier of the term enclosing position [p] *)
      val tid : t -> tid

      (** [to_string level] a textual and human readable
          representation of a cursor.  *)
      val to_string : t -> string

      (** [next p cls t] moves the cursor position [p] to the next
          position, that points to the term [t] of the class
          [cls]. Returns an error if there is no valid transition
          from the current program position to the specified program
          term.  *)
      val next : t -> ('p,'t) cls -> 't term -> (t,exn) Monad.Result.result
    end

    type pos = Pos.t [@@deriving sexp_of]

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
        type uuid = (void,void,void) format



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
          (project -> 'a) -> 'a t


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
                                     and type env := project
                                     and type id := id
                                     and module Syntax := Syntax
                                     and type 'a e =
                                           (exit_status * project) m effect

        (** Local state of the machine.  *)
        module Local  : State with type 'a m := 'a t
                               and type 'a t := 'a state


        (** Global state shared across all machine clones.  *)
        module Global : State with type 'a m := 'a t
                               and type 'a t := 'a state

        val raise : exn -> 'a t
        val catch : 'a t -> (exn -> 'a t) -> 'a t

        val project : project t
        val program : program term t
        val arch : arch t
        val args : string array t
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


      module Main(M : S) : sig
        val run :
          ?envp:string array ->
          ?args:string array ->
          project ->
          unit M.t ->
          (exit_status * project) M.m
      end

      (** [add_component comp] registers a machine component in the
          Primus Framework.  *)
      val add_component : component -> unit
    end




    (** type abbreviation for the Machine.state  *)
    type 'a state = 'a Machine.state


    (** The Machine component.  *)
    type component = Machine.component


    module Value : sig
      type id [@@deriving bin_io, compare, sexp]
      module Id : Regular.S with type t = id

      type t = value [@@deriving bin_io, compare, sexp]

      val to_word : t -> word
      val id : t -> id

      module Make(Machine : Machine.S) : sig
        type t = value
        type 'a m = 'a Machine.t
        val to_word : t -> word
        val of_word : word -> t m
        val of_string : string -> t m
        val of_bool : bool -> t m
        val of_int : width:int -> int -> t m
        val of_int32 : ?width:int -> int32 -> t m
        val of_int64 : ?width:int -> int64 -> t m
        val b0 : t m
        val b1 : t m
        val one : int -> t m
        val zero : int -> t m
        val signed : t -> t m
        val is_zero : t -> bool
        val is_one : t -> bool
        val is_positive : t -> bool
        val is_negative : t -> bool
        val is_non_positive : t -> bool
        val is_non_negative : t -> bool
        val bitwidth : t -> int
        val extract : ?hi:int -> ?lo:int -> t -> t m
        val concat : t -> t -> t m
        val succ : t -> t m
        val pred : t -> t m
        val nsucc : t -> int -> t m
        val npred : t -> int -> t m
        val abs : t -> t m
        val neg : t -> t m
        val add : t -> t -> t m
        val sub : t -> t -> t m
        val div : t -> t -> t m
        val modulo : t -> t -> t m
        val lnot : t -> t m
        val logand : t -> t -> t m
        val logor : t -> t -> t m
        val logxor : t -> t -> t m
        val lshift : t -> t -> t m
        val rshift : t -> t -> t m
        val arshift : t -> t -> t m

        module Syntax : sig
          val ( ~-) : t -> t m
          val ( + ) : t -> t -> t m
          val ( - ) : t -> t -> t m
          val ( * ) : t -> t -> t m
          val ( / ) : t -> t -> t m
          val (mod) : t -> t -> t m
          val (lor) : t -> t -> t m
          val (lsl) : t -> t -> t m
          val (lsr) : t -> t -> t m
          val (asr) : t -> t -> t m
          val (lxor) : t -> t -> t m
          val (land) : t -> t -> t m
        end
        include Regular.S with type t := t
      end

      include Regular.S with type t := t

    end

    (** The Interpreter.

        The Interpreter is the core componet of the Primus Machine. It
        provides lots of observations, giving other components an
        ability to track every event that happens during the program
        evaluation. The components can affect the results of
        evaluation in a limited way, by affecting the state of the
        components that are used by the Interpreter, name the
        Environemnt and the Memory. *)
    module Interpreter : sig

      val pc_change : addr observation

      val loading : value observation
      val loaded : (value * value) observation
      val storing : value observation
      val stored : (value * value) observation
      val reading : var observation
      val read : (var * value) observation
      val writing : var observation
      val written : (var * value) observation
      val undefined : value observation
      val const : value observation

      val binop : ((binop * value * value) * value) observation
      val unop : ((unop * value) * value) observation
      val cast : ((cast * int * value) * value) observation
      val extract : ((int * int * value) * value) observation


      (** an identifier of a term that will be executed next.   *)
      val enter_term : tid observation

      (** an identifier of a term that just finished the execution.  *)
      val leave_term : tid observation

      (** new program locatio entered  *)
      val enter_pos : pos observation

      (** a program location left  *)
      val leave_pos : pos observation

      (** a subroutine entered  *)
      val enter_sub : sub term observation

      (** a subroutine argument is entered  *)
      val enter_arg : arg term observation

      (** a basic block is entered  *)
      val enter_blk : blk term observation

      (** a phi-node is entered  *)
      val enter_phi : phi term observation

      (** a definition is entered  *)
      val enter_def : def term observation

      (** a jump term is entered  *)
      val enter_jmp : jmp term observation

      (** a subroutine was left  *)
      val leave_sub : sub term observation

      (** a subroutine argument was left  *)
      val leave_arg : arg term observation

      (** a basic block was left  *)
      val leave_blk : blk term observation

      (** a phi-node was left  *)
      val leave_phi : phi term observation

      (** a definition was left  *)
      val leave_def : def term observation

      (** a jump term was left  *)
      val leave_jmp : jmp term observation

      (** an expression was entered  *)
      val enter_exp : exp observation

      (** an expression was left *)
      val leave_exp : exp observation

      val halting : unit observation

      type exn += Halt

      (** Make(Machine) makes an interpreter that computes in the
          given machine.  *)
      module Make (Machine : Machine.S) : sig
        type 'a m = 'a Machine.t
        val halt : never_returns m
        val pos : pos m
        val sub : sub term -> unit m
        val blk : blk term -> unit m
        val get : var -> value m
        val set : var -> value -> unit m
        val binop : binop -> value -> value -> value m
        val unop : unop -> value -> value m
        val cast : cast -> int -> value -> value m
        val concat : value -> value -> value m
        val extract : hi:int -> lo:int -> value -> value m
        val const : word -> value m

        val load : value -> endian -> size -> value m
        val store : value -> value -> endian -> size -> unit m
      end
    end


    (** Iterator is a sequence of values from some domain.

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
        val next : t -> int Machine.t
      end
    end

    (** Machine Linker.

        The Linker dynamically extends program with the new code.

        The code is represented as a functor that performs a
        computation using a provided machine.*)
    module Linker : sig


      (** A code identifier.

          A program code can be identified by a name, address or by a
          term identifier.  *)
      type name = [
        | `tid of tid
        | `addr of addr
        | `symbol of string
      ] [@@deriving sexp_of]



      (** The Linker error  *)
      type exn += Unbound_name of name


      (** Code representation.

          A code representation is abstract and hides how the code
          itself is represented. It is just a function, that takes a
          machine and performs a computation using this machine.*)
      module type Code = functor (Machine : Machine.S) -> sig
        val exec : unit Machine.t
      end


      (** code representation  *)
      type code = (module Code)

      (** [Make(Machine)] parametrize the [Linker] with the [Machine].

          Note that the Linker, as well as all other Primus Machine
          components, is stateless, i.e., the functor itself doesn't
          contain any non-syntactic values, and thus it is purely
          functional. All the state is stored in the [Machine]
          state. Thus it is absolutely safe, and correct, to create
          multiple instances of components, as they needed. The
          functor instatiation is totaly side-effect free.

      *)
      module Make(Machine : Machine.S) : sig
        type 'a m = 'a Machine.t
        (** [link ~addr ~name ~tid code] links the given [code]
            fragment into the Machine. The code can be invoked by one
            of the provided identifier. If no idetifiers were
            provided, then apparently code will not be ever invoked. If
            an identifier was alread bound to some other code
            fragment, then the old binding will be shadowed by the new
            one.  *)
        val link :
          ?addr:addr ->
          ?name:string ->
          ?tid:tid ->
          code -> unit m


        (** [exec name] executes a code fragment associated with the
            given name. Terminates the computation with the
            [Linker.Unbound_name name] condition, if the [name] is not
            associated with any code fragment.  *)
        val exec : name -> unit m


        (** [is_linked name] computes to [true] if the [name] is
            associated with some code.  *)
        val is_linked : name -> bool m
      end
    end


    (** Evaluation environemnt.

        The Environment binds variables to values.*)
    module Env : sig

      (** a value of the variable will be looked up  *)
      val variable_access : var observation

      (** a variabe was valuated to the provided value.  *)
      val variable_read : (var * value) observation

      (** a variable was set to the specified value.  *)
      val variable_written : (var * value) observation

      (** A variable is undefined, if it was never [add]ed to the
          environment.  *)
      type exn += Undefined_var of var


      (** happens when an undefined variable is accessed.  *)
      val undefined_variable : var observation



      (** [Env = Make(Machine)]  *)
      module Make(Machine : Machine.S) : sig

        (** [get var] returns a value associated with the variable.
            Todo: it looks like that the interface doesn't allow
            anyone to save bottom or memory values in the environemnt,
            thus the [get] operation should not return the
            [Bil.result].*)
        val get : var -> value Machine.t

        (** [set var value] binds a variable [var] to the given [value].  *)
        val set : var -> value -> unit Machine.t


        (** [add var generator] adds a variable [var] to the
            environment. If a variable is read before it was defined
            with the [set] operation, then a value produces by the
            generator will be automatically associated with the
            variable and returned. *)
        val add : var -> Generator.t -> unit Machine.t
      end
    end



    (** Virtual memory.

        The virtual memory is a byte addressable machine memory.*)
    module Memory : sig


      (** occurs when an access (read or write) is perfomed on address
          that is not mapped into the Machine memory.  *)
      val segmentation_fault : addr observation

      module Make(Machine : Machine.S) : sig

        (** [load addr] loads a byte from the given address *)
        val load : addr -> word Machine.t


        (** [store addr x] stores a byte [x] at the given address [addr]  *)
        val store : addr -> word -> unit Machine.t

        (** [add_text mem] maps a memory chunk [mem] as executable and
            readonly segment of machine memory.*)
        val add_text : mem -> unit Machine.t


        (** [add_data] maps a memory chunk [mem] as writable and
            nonexecutable segment of machine memory.  *)
        val add_data : mem -> unit Machine.t


        (** [allocate addr size] allocates a segment of the specified
            [size]. An unitilialized reads from the segment will
            produce values generated by a generator (defaults to a
            [Generator.Random.Seeded.byte]).

            An attempt to write to a readonly segment, or an attemp to
            execute non-executable segment will generate a
            segmentation fault. (TODO: provide more fine-granular traps).*)
        val allocate :
          ?readonly:bool ->
          ?executable:bool ->
          ?generator:Generator.t ->
          addr -> int -> unit Machine.t


        (** [map mem] maps a memory chunk [mem] to a segment with the
            given permissions. See also {!add_text} and {!add_data}. *)
        val map :
          ?readonly:bool ->
          ?executable:bool ->
          mem -> unit Machine.t


        (** [is_mapped addr] a computation that evaluates to true,
            when the value is mapped, i.e., it is readable.  *)
        val is_mapped : addr -> bool Machine.t


        (** [is_writable addr] is a computation that evaluates to
            [true] if [addr] is writable.  *)
        val is_writable : addr -> bool Machine.t
      end
    end


    (** Lisp machine.

        The Lisp Machine is an extensible Lisp Machine embedded into
        the Primus Machine. The Lisp machine is used to provide
        function stubs (summaries), as well as to control the Primus
        Machine using a dialect of Lisp.

        {1 Primus Lisp Language}

        {2 Overview}

        Primus Lisp is a dialect of Lisp, that can be used to interact
        with a native program. Primus Lisp is close to Common Lisp and
        to the Emacs Lisp dialect.

        Primus Lips is a low-level language, that doesn't provide many
        abstractions, as it tries to be as close to the machine
        language as possible. In that sense Primus Lisp can be seen as
        an assembler, except that it can't really assemble binaries,
        as it operates over already existing and assembled
        program. Primus Lisp, however is still quite powerfull, as the
        absence of suitable abstractions is compensated with powerful
        and versatile meta-programming system.

        Primus Lisp is primarily used for the following tasks:
        - writing function summaries (aka stubs);
        - setting up program environment;
        - exploring and observing program behavior.


        A Primus Lisp program is a file, that can contain the
        following entities:
        - feature requests;
        - declarations;
        - constants
        - substitutions;
        - macros;
        - functions;
        - advice

        The entities may be specified in any order, however the above
        order constitutes a good programming practice.

        Each file provides (implements) a feature, that has the same
        name as the name of the file without an extension and
        directories. Thus the namespace of features is flat. A feature
        is usually a function, macro definition, or any other
        definition, or a collection of definition, gathered under the
        same theme. For example, the [getopt] feature implements C
        [getopt] function, and accompanying definitions. The features
        maybe very specific, i.e., providing an implementation for
        only one small function, or they can be a collection of other
        features. For example, the [posix] feature provides an
        implementation of all functions specified in the POSIX
        standard (not all at the time of writing).

        A collection of files is called library.  To use features
        provided by another file, the file should be requested with
        the [(require <name>)] form, where [<name>] is the name of the
        feature. A file with the requested name is searched in the
        library, and loaded, making all its definitions available in
        the lexical scope, that follows the [require] form. If a
        feature is already provided, then nothing
        happens. Dependencies should not contain cycles.

        Declarations specify attributes that are shared by all
        definitions in a file.For example, a declaration

        {v (declare (context (arch armv7)) v}

        makes all definitions visible only in the context of the ARMv7
        architecture.

        Constants and substitutions are primitive abstractions that
        give names to code fragments. Macros are program
        transformations. Functions add parameters to a code, and are
        basic building blocks. Scope of all definitions can be limited
        with the context declarations. Finally, a function can be
        advised with another function using the [advice-add] function.


        {2 Type system}

        A type defines all possible values of an expression. In Primus
        Lisp, expression values can be only scalar, i.e., machine
        words of different widths. The width is always specified in
        the number of bits. A maximum width of a word is equal to the
        width of the architecture machine word, thus a family of types
        is dependent on the context of evaluation. (Note, current
        implementation limits maximum width of the machine word to 64
        bits). We denote a type of expression with a decimal number,
        e.g., [(exp : 16)] means that an expression ranges over all 16
        bit words.

        An expression can have a polymorphic type [t] that denotes a
        powerset of all types for the given architecture. For example,
        for ARMv7, {v t = 32 \/ 31 \/ .. \/ 1 v}. Thus a value of any
        type, is a also a value of type [t].

        Side note -- the type system doesn't include the unit type,
        i.e., the [0] type. An expression [()] evaluates to the [0:1]
        value.

        {2 Functions and expressions}

        Functions are named abstractions of code, where a code is a
        sequence of expressions. Since a value of an expression is
        a machine word, functions are not first-class values in Primus
        Lisp. However, functions and types can be manipulated on the
        meta-programming level.

        A function is defined with the [defun] form, that has the
        following syntax:

        {[
          (defun <name> (<p1> <p2> ... <pM>)
           <e1> <e2> .. <eN>)
        ]}

        A function definition may optionally contain a documentation
        strings and a declaration section. For example,

        {[
          (defun strlen (p)
             "returns a length of the null-terminated string pointed by P"
             (declare (external "strlen"))
             (msg "strlen was called with $p")
             (let ((len 0))
                 (while (not (points-to-null p))
             (incr len p))
            len))
        ]}

        A function can be called (applied) using the function
        application form:

        {v (<function-name> <e1> <e2> ... <eM>) v}

        The first element of the function application form is not an
        expression and must be an identified. The rest arguments are
        expressions, that are evaluated from left to right. All
        arguments are passed by value.

        The body of a function is a sequence of expressions, that is
        evaluated in the lexical order (i.e., from top to down). A
        value of the last expression is the result of the function
        evaluation. An expression is either a function application or
        or a special form. Primus Lisp defines only 5 special forms,
        the rest of the syntax is defined using the macro system.

        {3 Choice}

        The {v (if <cond> <e1> <e2> .. <eM>) v} form is a basic
        control flow structure. If <cond> evaluates to a non-zero word
        then the result of the <if> form is the result of the <e1>
        expression. Otherwise, a sequence of expressions

        {v <e2> .. <eM> v} is evaluated and the result of the [if]
        form is the result of the last expression {v <eM> v}.


        Several derived forms are defined as macros, e.g.,

        {[
          (when <cond> <body>)
          (or <e1> <e2> .. <eM>)
            (and <e1> <e2> .. <eM>)
        ]}

        {3 Loops}

        Iterations can be implemented either using recursion or with
        the [while] special form. Since the interpreter doesn't
        provide the tail-call optimization it is better to use the
        latter (although the interpreter itself is using a constant
        stack size, as it uses the host language heap memory to
        represent the Primus Lisp call stack).

        The {v (while <cond> <e1> <e2> ..<eM>) v} form, will evaluate
        the <cond> expression first, and if it is a non-zero value,
        then the sequence of expressions {v <e1> .. <eM> v} is
        evaluated, and the value of the last expression becomes the
        value of the [while] form. If the value of the <cond>
        expression is not a zero, then this value becomes the value of
        the [while] form.


        {3 Variables}

        The [let] form binds values to names in the lexical scope.

        {v (let ((<v1> <e1>) .. (<vM> <eM>) <s1> .. <sN>) v}

        form binds variables {v <v1>, ... <vM > v} to values of
        expressions {v <e1>, ..., <eM> v} in expressions
        {v <s1>,...,<sN> v}.  The lexical scope of the bound variable
        starts from the next binding expression and ends with the
        scope of the whole let expression. Thus, a variable <v1> is
        bound in expressions {v <e2>,..., <eM> v} as well as in
        {v <s1>,..., <sN> v}.

        The value of the [let] form is the value of the last
        expression {v <sN> v}.


        {3 Sequencing }

        The {v (prog <e1> ... <eM>) v} form combines a sequence of
        expressions into one expression, and is useful in the contexts
        where an expression is required. The expressions are evaluated
        from left to right, and the value of the [prog] form is the
        value of the last expression.

        {3 Messaging }

        The {v (msg <fmt> <e0> <eM>) v} form constructs
        logging/debugging messages using an embedded formatting
        language. The formed message will be sent to the logging
        facility, that was set up during the Primus Lisp library
        initialization.

        The format language interprets all symbols literally, unless
        they start with the dollar sign ($).

        A pair of characters of the form {v $<n> v}, where {v <n> v}
        is a decimal digit, will be substituted with the value of the
        n'th expression (counting from zero).

        A sequence {v $<expr> v} will be substituted with the value of
        expression {v $<expr> v}. Thus the [$] symbol can be seen as
        an anti-quotation, that temporary disables the quoting marks.

        Example,

        {v (msg "hello, $0 $0 world, (+ 7 8) = $(+ 7 8)" "cruel") v}

        will be rendered to a message:

        {v "hello, cruel cruel world, (+ 7 8) = 15" v}


        {2 Metaprogramming}

        Ordinary Primus Lisp expressions are evaluated at the runtime
        in the Primus emulator, and are quite limited as they need to
        be evaluated directly on the CPU model. To mitigate this
        limitation, Primus Lisp provides a powerful metaprogramming
        system. The metaprogram is evaluated when the Primus Lisp
        program is read. A metaprogram generates a program, that will
        be evaluated by the CPU. The metaprogram itself is Turing
        complete, thus any transformation can be applied to a
        program. The Primus Lisp metaprogramming system use
        term-rewriting as a computational model, with Lisp code
        fragments as terms. Primus Lisp provides three facilities for
        metaprogramming:

        - syntactic constants;
        - syntactic substitutions;
        - macro definitions.

        {3 Constants}

        The syntactic constants is the simplest syntactic
        substitution, it just substitutes atoms for atoms. Constants
        are introduced with the [defconstant] form, that has the
        following syntax:

        {[
          (defconstant <name>
           [<docstring>]
             [<declarations>]
           <value>)
        ]}

        For example,

        {v (defconstant main-address 0xDEAD) v}


        During the program parsing, each occurrence of the
        {v <name> v} term will be rewritten with the {v <value> v}
        term, that should be an atom.



        {3 Substitutions}

        The syntactic substitutions is the generalization of syntactic
        constant, and has quite a similar syntax:

        {[
          (defsubst <name> [<declarations>] [:<syntax>] {<value>})
        ]}


        During parsing, every occurrence of the term {v <name> v} (that
        should be an atom), will be rewritten with a sequence of
        values {[ {<value>} ]}.

        Example,

        {v (defsubst ten-digits 0 1 2 3 4 5 6 7 8 9) v}

        A process of applying of the substitutions is called
        "expansion". Since the expansion transforms an atom to a list
        of atoms, it can be applied only inside the macro or function
        application. For example,

        {v (+ ten-digits) v}

        will be expanded to

        {v (+ 0 1 2 3 4 5 6 7 8 9 ) v}

        {4 Special syntax}

        Expansions also provide a support for extensible value
        specification syntax, that enables domain-specific data
        specification languages. Currently, we support only two
        syntaxes: [:ascii] and [:hex].

        In the [:ascii] syntax the values should be atoms, possibly
        delimited with double quotes. Each character of each atom will
        be expanded to its corresponding ASCII code. Strings can
        contain special characters prefixed with a backslash. The
        special character can be one of the well-known ASCII special
        character, e.g., [\n], [\r], etc, or it can be a decimal or a
        hexadecimal code of a character.

        Example, given the following substitution:

        {v (defsubst hello-cruel-world :ascii "hello, cruel world\n\000") v}

        the following application:

        {v (write-block SP hello-cruel-world v}

        will be expanded with

        {[
          (write-block SP
             0x68 0x65 0x6c 0x6c 0x6f 0x2c 0x20 0x63
             0x72 0x75 0x65 0x6c 0x20 0x77 0x6f 0x72
             0x6c 0x64 0x0a 0x00)
        ]}


        In the [:hex] syntax the sequence of atoms is split into
        two-characters subsequences each treated as a hex value. This
        syntax is useful for encoding memory dumps in a format that is
        close to the hexdump (without offsets). E.g., given the
        following substitution rule

        {[
          (defsubt example :hex 68656c 6c 6f2c2063)
        ]}

        an application

        {[
          (write-block SP example)
        ]}

        will be expanded into

        {[
          (write-block SP 0x68 0x65 0x6c 0x6c 0x6f 0x2c 0x20 0x63)
        ]}


        {3 Macro}

        The macros provide the most versatile and powerful way to
        specify arbitrary code transformations. The macro definitions
        introduce abstractions on the meta-programming level. I.e., it
        allows a programmer to write a function that operates on code
        terms, making the code a first class value.


        The macro definition has the following syntax:

        {[

          (defmacro <name> (<p1> ... <pM>)
             [<docstring>] [<declarations>]
           <value>)
        ]}

        A macro definition adds a term rewriting rule, that rewrites
        each occurrence of {v (<name> <t1> .. <tN>) v}, where [N >= M]
        with the {v <value> v} in which occurrences of the parameter
        <pN> is substituted with the term <tN>. If [N] is bigger than
        [M], then the last parameter is bound with the sequence
        {v <tM>...<tN> v}.

        The macro subsystem doesn't provide any specific looping or
        control-flow facilities, however, the macro-overloading
        mechanism along with the recursion make it possible to encode
        arbitrary meta-transformations.

        Other than a standard context-based ad-hoc overloading
        mechanism, the macro application uses arity-based
        resolution. As it was described above, if a number of
        arguments is greater than the number of parameters, then the
        last parameter is bound to the rest of the arguments. When
        several macro definitions matches, then a definition that has
        fewer unmatched arguments is chosen. For example, suppose we
        have the following definitions:

        {[
          (defmacro list-length (x) 1)
            (defmacro list-length (x xs) (+ 1 (list-length xs)))
        ]}


        The the following term

        {v (list-length 1 2 3) v}

        will be normalized (after a series of transformations) with
        the following:

        {v (+ 1 (+ 1 1)) v}


        {[
          1: (list-length 1 2 3) => (+ 1 (list-length 2 3))
               2: (+ 1 (list-length 2 3)) => (+ 1 (+ 1 (list-length 3)))
                    3: (+ 1 (+ 1 (list-length 3))) => (+ 1 (+ 1 1))
        ]}

        In the first step, both definition match. In the first
        definition [x] is bound to [1 2 3], while in the second
        [x] is bound to [1] and [xs] is bound to [2 3]. Since the last
        parameter is bound to fewer arguments, the second definition
        is chosen as the most certain. In the second step the second
        definition is still more concrete. Finally at the last step,
        the second definition doesn't match at all, as it has more
        parameters than arguments.


        A slightly more complex example, is a fold iterator, that
        applies a function to a sequence of arguments  of arbitrary
        length, e.g.,

        {[
          (defmacro fold (f a x) (f a x))
            (defmacro fold (f a x xs) (fold f (f a x) xs))
        ]}


        Using this definition we can define a sum function (although
        it is not needed as the [+] function defined in the Primus Lisp
        standard library, already accepts arbitrary number of
        arguments), as:

        {v (defmacro sum (xs) (fold + 0 xs)) v}

        The {v (sum 1 2 3) v} will be rewritten as follows:

        {[
          1: (sum 1 2 3) => (fold + 0 1 2 3)
               2: (fold + 0 1 2 3) => (fold + (+ 0 1) 2 3)
                    3: (fold + (+ 0 1) 2 3) => (fold + (+ (+ 0 1) 2) 3)
                         4: (fold + (+ (+ 0 1) 2) 3) => (+ (+ (+ 0 1) 2) 3)
        ]}

        A more real example is the [write-block] macro, that takes a
        sequence of bytes, and writes them starting from the given
        address:

        {[
          (defmacro write-block (addr bytes)
             (fold memory-write addr bytes))
        ]}

        The definition uses the [memory-write] primitive, that writes
        a byte at the given address and returns an address of the next
        byte.


        {2 Polymorphism}

        Primus Lisp provides both kinds of polymorphism: parametric
        and ad hoc.

        Expressions in Primus Lisp have types, that are denoted with
        natural numbers starting with one. Each type defines a set of
        values that can be represented with the given number of
        bits. Values with different widths are different, even if they
        represent the same number. Expressions can be polymorphic,
        e.g., function

        {v (defun square (x) ( * x x)) v}

        has type `forall n. n -> n -> n`. Thus it can be applied to
        values of different types, e.g., [(square 4:4)], that will be
        evaluated to the [0:4] value, or [(square 4:8)], that will be
        evaluated to [16:8], etc. The parametric polymorphism doesn't
        require any special annotations or type specifications so we
        will not stop on it anymore.


        The ad hoc polymorphism provides a facilities for overloading
        definitions. That is, the same entity may have multiple
        definitions, and depending on a context, only one definition
        should be chosen. Not only functions can have multiple
        definitions, but also macros, constants, and
        substitutions. Since the latter three entities operate on the
        syntactic level, the syntax of Primus Lisp itself is
        context-dependent.

        {3 Context}

        A context (from the perspective of the type system) is a set
        of type class instances. The context is fixed when a program
        is parsed. A Primus Lisp program may not change the context;
        neither in runtime, nor it the parse time, thus a program is
        parsed and evaluated at the specific context. However, a
        definition may declare that it makes sense only in some
        context. If more than one definition make sense under the
        given context, then the most specific one is chosen. If no
        definition is more specific than another, then an error
        occurs.

        A type class defines a type as a set of features. The subset
        relation induces a subtyping relation over types - a type [t']
        is a subtype of a type [t] if [t' <= t] (i.e., if [t'] is a
        subset of [t]). Each feature is a textual tag, called a
        feature constructor.

        The context declaration limits an associated definition to the
        specified type class(es), and has the following syntax:

        {[
          (declare (context <type-class-name> {<feature-constructor>}))
        ]}

        Let's use the following two definitions for a concrete example,

        {[
          (defmacro get-arg-0 ()
             (declare (context (arch arm gnueabi)))
             R0)

            (defmacro get-arg-0 ()
               (declare (context (arch x86 cdecl)))
               (read-word word-width (+ SP (sizeof word-width))))
        ]}


        We have two definitions of the same macro [get-arg-0], that
        are applicable to different contexts. The first definition,
        is only applicable in the context of the ARM architecture and
        the gnueabi ABI. The second is applicable in the context of
        the x86 architecture and the cdecl ABI. More formally, a
        definition is considered only if its context is a subtype of
        the current type context.

        {2 Advice mechanism}

        Primus Lisp also provides a mechanism for non-intrusive
        extending existing function definitions. An existing
        definition maybe advised with another definition. A piece of
        advice maybe added to a function, and will be called either
        before, or after the function evaluation, e.g.,

        {[
          (defun memory-written (a x) (msg "write $x to $a"))
            (advice-add memory-written :after memory-write)
        ]}

        The general syntax is:

        {v (advice-add <advisor> <when> <advised>) v}

        where the <when> clause is either [:before] or [:after].

        {2 Formal syntax}

        Each entity is an s-expression with the grammar, specified
        below.We use BNF-like syntax with the following conventions.
        Non-terminal symbols are denoted like <this>, square brackets
        denote optional components, and curly brackets denote zero,
        one, or several repetitions of the enclosed component. Ordinary
        parentheses do not bear any notation, and should be read
        literally. Note, since the grammar is not context free, and is
        extensible, the following is an approximation of the language
        grammar. Grammar extension points are defined as '..' in the
        production definition.

        {[
          module ::= {<entity>}
              entity ::= <feature-request>
                     | <declarations>
                     | <constant-definition>
                     | <substitution-definition>
                     | <macro-definition>
                     | <function-definition>
                       | <advising>
                         feature-request ::= (require <ident>)
                           declarations ::= (declare {<attribute>})
                           constant-definition ::=
                         (defconstant <ident> [<docstring>] [<declarations>] <atom>)
                           substitution-definition ::=
                         (defsubst <ident> [<declarations>] [:<syntax>] {<atom>})
                           macro-definition ::=
                         (defmacro <ident> ({<macro-param>})
                            [<docstring>] [<declarations>]
                          <exp>)
                           function-definition ::=
                                   (defun <ident> ({<function-param>})
                                       [<docstring>] [<declarations>]
                                       {<exp>})
                                     advice ::= (advice-add <ident> <method> <ident>)
                                     exp ::= ()
                           | (if <exp> <exp> {<exp>})
                           | (let ({<binding>}) {<exp>})
                           | (while exp {exps})
                           | (prog {exp})
                           | (msg <format>)
                           | (<ident> {exp})
                               binding ::= (<var> <exp>)
                               attribute ::= (<ident> <attribute-value>)
                               macro-param ::= {<ident>}
                               function-param ::= {<var>}
                                         var = <ident> | <ident>:<size>

                                                         attribute-value ::= ?each attribute defines its own syntax?
          external-attribute-value ::= {<ident>}
              context-attribute-value ::= {<context-declaration>}
                                         context-declaration ::= (<ident> {<ident>})
                                         docstring ::= <text>
                                         syntax ::= :hex | :ascii | ..
                                       atom  ::= <word> | <text> | ..
                                       word  ::= ?<ascii-char> | <int> | <int>:<size>
                                         int   ::= {<decimal>} | 0x{<hex>} | 0b{<bin>} | 0o{<oct>}
                                         size  ::= {<decimal>}
        ident ::= ?any atom that is not recognized as a <word>?
        ]}
    *)
    module Lisp : sig



      (**  A lisp primitive  *)
      module Primitive : sig
        type 'a t


        (** [create ~docs name code] creates a lisp primitive, that is
            accessible from lisp as a regular function with the given
            [name]. A function [code] accepts a list of arguments,
            and returns a computation in a Machine monad, that should
            evaluate to a word. *)
        val create : ?docs:string -> string -> (value list -> 'a) -> 'a t
      end


      (** a list of primitives.  *)
      module type Primitives = functor (Machine : Machine.S) ->  sig


        (** a list of primitives defined in the Machine monad.  *)
        val defs : unit -> value Machine.t Primitive.t list
      end


      (** a list of priomitives.  *)
      type primitives = (module Primitives)

      type exn += Runtime_error of string


      (** Make(Machine) creates a Lisp machine embedded into the
          Primus [Machine].  *)
      module Make (Machine : Machine.S) : sig


        (** [failf msg a1 ... am ()] terminates a lisp machine, and
            correspondingly the Primus machine with the
            [Runtime_error].  *)
        val failf : ('a, unit, string, unit -> 'b Machine.t) format4 -> 'a


        (** [link_primitives prims] provides the primitives [prims]   *)
        val link_primitives : primitives -> unit Machine.t
      end


      (** [init ?log ?paths features] initializes the Lisp machine.
          This function should be called by a plugin, that is
          responsible for providing lisp code. In the [bap] framework
          it is called by the [primus-lisp] plugin.  *)
      val init : ?log:formatter -> ?paths:string list -> string list -> unit
    end


    (** Primus error.  *)
    module Exn : sig
      type t = exn = ..


      (** returns a textual representation of an error  *)
      val to_string : t -> string


      (** [add_printer to_string] registers a printer.  *)
      val add_printer : (t -> string option) -> unit
    end
  end
end
