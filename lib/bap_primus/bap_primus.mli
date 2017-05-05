open Core_kernel.Std
open Bap.Std
open Monads.Std
open Format

module Std : sig


  module Primus : sig


    (** Machine error.

        The error type is an extensible variant, and components
        usually register their own error constructors. *)
    type error = ..



    (** [an observation] of a value of type [an].*)
    type 'a observation


    (** [a statement] is used to make an observation of type [a].    *)
    type 'a statement

    type ('a,'e) result = ('a,'e) Monad.Result.result =
      | Ok of 'a
      | Error of 'e



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


      (** [provide ?inspect name] returns a pair of two handlers. The
          first element is used to observe values, the second is used
          to provide values for the observation.

          The [inspect] function should provide a sexp representation
          of an observed value, and is used for introspection and
          pretty-printing (it is not required, and if it is provided, it
          is not necessary to disclose everything *)
      val provide : ?inspect:('a -> Sexp.t) -> string -> 'a observation * 'a statement


      (** [name observation] is a name of the observed attribute.  *)
      val name : 'a observation -> string


      (** [inspect observation value] returns a sexp representation of
          an observed [value] *)
      val inspect : 'a observation -> 'a -> Sexp.t
    end


    (** Evaluation Context.*)
    module Context : sig


      (** A hierarchical program position.


          The [Level.t] is a cursor-like data structure, that
          describes a program position in the program term hierarchy.*)
      module Level : sig


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

        type t =
          | Top of level3       (** a program *)
          | Sub of level2       (** a subroutine  *)
          | Arg of arg level1   (** subroutine argument *)
          | Blk of blk level1   (** a basic block *)
          | Phi of phi level0   (** a phi-node *)
          | Def of def level0   (** a definition *)
          | Jmp of jmp level0   (** a jump term *)



        (** [to_string level] a textual and human readable
        representation of a cursor.  *)
        val to_string : t -> string


        (** [next p cls t] moves the cursor position [p] to the next
            position, that points to the term [t] of the class
            [cls]. Returns an error there is no valid transition from
            the current program position to the specified program term.  *)
        val next : t -> ('p,'t) cls -> 't term -> (t,error) Monad.Result.result
      end


      (** program location.  *)
      type level = Level.t [@@deriving sexp_of]



      (** Primus Interpreter Context.

          Although it is possible to extend the Context class via the
          inheritence it is not recommended, as the Primus framework
          provides more composable way to extend the interpreter
          state. See the {!State} module for more information.


          A value of type [#t] can be accessed in the machine
          operation (an operation in the Machine monad), by the virtue
          of the [get ()] operation. The context can be changed with
          the [put ctxt] operation.

          [new ~envp ~argv ~main proj] creates a fresh new
          context. The [envp] is an array of environemnt
          variables. The representation depends on a system, but
          usually it is an array of [name=value]
          bindings. Correspondingly, the [argv] is an array of the
          program arguments. The [main] argument, if present, defines
          a function that is considered the entry point to a program.

          Note: it is usually not necessary to create a new context
          manually, unless you are implementing a new instatiation of
          a framework.*)
      class t :
        ?envp: string array ->
        ?argv: string array -> ?main:sub term -> project ->
        object('s)
          inherit Biri.context


          (** [argv] an array of command line arguments  *)
          method argv : string array

          (** [envp] an array of process environment variables  *)
          method envp : string array

          (** [project] a static model of a program  *)
          method project : project

          (** [with_project proj] updates the static model of a program  *)
          method with_project : project -> 's


          (** [current] returns a term identifier of a current program
              term.  *)
          method current : tid


          (** [level] returns a current program location.  *)
          method level : level


          (** [with_level level] invoked by the interpreter every time
              a program position changes.  *)
          method with_level : level -> 's
        end
    end


    (** Evaluation context.  *)
    class type context = Context.t




    (** Primus Machine.

        The Machine is the core of Primus Framework.  The Machine
        behavior is extended/changed with Machine Components. A
        component is a functor that takes a machine instance, and
        registers reactions to different events, that can happen
        during the machine evaluation. Events can be obtained from the
        observations made by the core components of the Machine, such
        as the Interpreter, or by other components, if their
        implementors provided any observations.

        A machine is usually instantiated and ran only once. For
        example, the [run] analysis creates a machine parameterized by
        the static model of a binary and runs a machine from the
        specified entry point, until it terminates.

        The user analysis is usually written in a form of a component,
        and is registered with the [register_component] function.*)
    module Machine : sig


      (** [finished] occurs when machine terminates.   *)
      val finished : unit observation


      (** Machine State.

          Any component can have it own state. In fact, components can
          have global state and local state.

          The Primus Machine is a non-deterministic Machine that can
          have multiple states at once. Basically, every time a
          non-deterministic event happens a machine can be forked
          (cloned). The [Global] state is shared across all clones of a
          machine, and can be used as a communication channel between
          the clones. Each clone has its own copy of the local state,
          that doesn't interfere with the state of other clones.
      *)
      module State : sig


        (** [('a,'c) t] is a type of state that holds a value of type
            ['a], and can be constructed from the base context of type
            ['c]. *)
        type ('a,'c) t

        type ('a,'c) state = ('a,'c) t


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
          ('c -> 'a) -> ('a,'c) t


        (** [inspect state value] introspects given [value] of the state.  *)
        val inspect : ('a,'c) t -> 'a -> Sexp.t


        (** [name state] a state name that was given during the construction.  *)
        val name : ('a,'c) t -> string
      end


      type 'a state = ('a,Context.t) State.t


      (** An interface to the state.

          An interface gives an access to operations that query and
          modify machine state. *)
      module type State = sig
        type ('a,'e) m
        type 'a t


        (** [get state] extracts the state.  *)
        val get : 'a t -> ('a,#Context.t) m


        (** [put state x] saves a machine state  *)
        val put : 'a t -> 'a -> (unit,#Context.t) m


        (** [update state ~f] updates a state using function [f]. *)
        val update : 'a t -> f:('a -> 'a) -> (unit,#Context.t) m
      end



      (** The Machine interface.*)
      module type S = sig


        (** the machine  *)
        type ('a,'e) t


        (** the machine computation.  *)
        type 'a m



        (** Observations interface.  *)
        module Observation : sig


          (** [observe obs on_observation] subscribes to the given
              observation [obs]. Every time the observation [obs] is
              made a function [on_observation] is called. The
              function can perform arbitrary computations in the
              machine monad, e.g., make its own computations, or access
              other components via their interfaces.  *)
          val observe : 'a observation -> ('a -> (unit,'e) t) -> (unit,'e) t


          (** [make observation event] make an [observation] of the
              given [event].  *)
          val make : 'a statement -> 'a -> (unit,'e) t
        end


        (** Computation Syntax.*)
        module Syntax : sig
          include Monad.Syntax.S2 with type ('a,'e) t := ('a,'e) t


          (** [event >>> action] is the same as
              [Observation.observe event action] *)
          val (>>>) : 'a observation -> ('a -> (unit,'e) t) -> (unit,'e) t
        end


        include Monad.State.Multi.S2 with type ('a,'e) t := ('a,'e) t
                                      and type 'a m := 'a m
                                      and type ('a,'e) e = 'e -> (('a, error) result * 'e) m
                                      and module Syntax := Syntax



        (** Local state of the machine.  *)
        module Local  : State with type ('a,'e) m := ('a,'e) t
                               and type 'a t := 'a state


        (** Global state shared across all machine clones.  *)
        module Global : State with type ('a,'e) m := ('a,'e) t
                               and type 'a t := 'a state

        include Monad.Fail.S2 with type ('a,'e) t := ('a,'e) t
                               and type 'a error = error
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
        val init : unit -> (unit,#Context.t) Machine.t
      end


      (** The Machine component.  *)
      type component = (module Component)



      (** [Make(Monad)] a monad transformer that wraps the Machine
          into an arbitrary [Monad].  *)
      module Make(M : Monad.S) : S with type 'a m = 'a M.t



      (** [Main(Machine)] instantiates the [Machine] and provides a
          function, that runs the Machine *)
      module Main(M : S) : sig
        val run : ('a,#Context.t as 'e) M.t -> 'e -> (('a,error) result * 'e) M.m
      end


      (** [add_component comp] registers a machine component in the
          Primus Framework.  *)
      val add_component : component -> unit
    end




    (** type abbreviation for the Machine.state  *)
    type 'a state = 'a Machine.state


    (** The Machine component.  *)
    type component = Machine.component



    (** The Interpreter.

        The Interpreter is the core componet of the Primus Machine. It
        provides lots of observations, giving other components an
        ability to track every event that happens during the program
        evaluation. The components can affect the results of
        evaluation in a limited way, by affecting the state of the
        components that are used by the Interpreter, name the
        Environemnt and the Memory. *)
    module Interpreter : sig


      (** an identifier of a term that will be executed next.   *)
      val enter_term : tid observation

      (** an identifier of a term that just finished the execution.  *)
      val leave_term : tid observation

      (** new program locatio entered  *)
      val enter_level : Context.level observation

      (** a program location left  *)
      val leave_level : Context.level observation

      (** the top-level term entered  *)
      val enter_top : program term observation

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

      (** an identifier of a term that will be executed next.   *)
      val enter_term : tid observation

      (** an identifier of a term that just finished the execution.  *)
      val leave_term : tid observation

      (** new program locatio left  *)
      val enter_level : Context.level observation

      (** a program location left  *)
      val leave_level : Context.level observation

      (** the top-level term left  *)
      val enter_top : program term observation

      (** a subroutine is left  *)
      val enter_sub : sub term observation

      (** a subroutine argument is left  *)
      val enter_arg : arg term observation

      (** a basic block is left  *)
      val enter_blk : blk term observation

      (** a phi-node is left  *)
      val enter_phi : phi term observation

      (** a definition is left  *)
      val enter_def : def term observation

      (** a jump term is left  *)
      val leave_jmp : jmp term observation


      (** a value of the variable will be looked up  *)
      val variable_access : var observation

      (** a variabe was valuated to the provided value.  *)
      val variable_read : (var * Bil.result) observation

      (** a variable was set to the specified value.  *)
      val variable_written : (var * Bil.result) observation


      (** an address will be read  *)
      val address_access : addr observation


      (** a byte from the given address was read  *)
      val address_read : (addr * word) observation


      (** a byte was written to the given address  *)
      val address_written : (addr * word) observation


      (** Make(Machine) makes an interpreter that computes in the
          given machine.  *)
      module Make (Machine : Machine.S) : sig
        module Biri : Biri.S
          with type ('a,'e) state = ('a,'e) Machine.t
        class ['a] t : object
          inherit ['a] Biri.t
          constraint 'a = #context
        end
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


        (** [lcg ~min ~max seed] a linear congruential generator,
        that produces a sequence of pseudorandom values that lies in
        the range between [min] and [max] (all inclusive). *)
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
        val next : t -> (int,#Context.t) Machine.t
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
      type error += Unbound_name of name


      (** Code representation.

          A code representation is abstract and hides how the code
          itself is represented. It is just a function, that takes a
          machine and performs a computation using this machine.*)
      module type Code = functor (Machine : Machine.S) -> sig
        val exec : (#Context.t as 'a) Biri.Make(Machine).t -> (unit,'a) Machine.t
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
        type ('a,'e) m = ('a,'e) Machine.t
        module Biri : Biri.S
          with type ('a,'e) state = ('a,'e) Machine.t



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
          code -> (unit,#Context.t) m


        (** [exec name] executes a code fragment associated with the
            given name. Terminates the computation with the
            [Linker.Unbound_name name] condition, if the [name] is not
            associated with any code fragment.  *)
        val exec : name -> (#Context.t as 'a) #Biri.t -> (unit,'a) m


        (** [is_linked name] computes to [true] if the [name] is
            associated with some code.  *)
        val is_linked : name -> (bool,#Context.t) m
      end
    end


    (** Evaluation environemnt.

        The Environment binds variables to values.*)
    module Env : sig



      (** A variable is undefined, if it was never [add]ed to the
          environment.  *)
      type error += Undefined_var of var


      (** happens when an undefined variable is accessed.  *)
      val undefined_variable : var observation



      (** [Env = Make(Machine)]  *)
      module Make(Machine : Machine.S) : sig
        type ('a,'e) m = ('a,'e) Machine.t



        (** [get var] returns a value associated with the variable.
            Todo: it looks like that the interface doesn't allow
            anyone to save bottom or memory values in the environemnt,
            thus the [get] operation should not return the
            [Bil.result].*)
        val get : var -> (Bil.result,#Context.t) m


        (** [set var value] binds a variable [var] to the given [value].  *)
        val set : var -> word -> (unit,#Context.t) m


        (** [add var generator] adds a variable [var] to the
            environment. If a variable is read before it was defined
            with the [set] operation, then a value produces by the
            generator will be automatically associated with the
            variable and returned. *)
        val add : var -> Generator.t -> (unit,#Context.t) m
      end
    end



    (** Virtual memory.

        The virtual memory is a byte addressable machine memory.*)
    module Memory : sig


      (** occurs when an access (read or write) is perfomed on address
          that is not mapped into the Machine memory.  *)
      val segmentation_fault : addr observation


      module Make(Machine : Machine.S) : sig
        type ('a,'e) m = ('a,'e) Machine.t


        (** [load addr] loads a byte from the given address *)
        val load : addr -> (word,#Context.t) m


        (** [save addr x] stores a byte [x] at the given address [addr]  *)
        val save : addr -> word -> (unit,#Context.t) m



        (** [add_text mem] maps a memory chunk [mem] as executable and
            readonly segment of machine memory.*)
        val add_text : mem -> (unit,#Context.t) m


        (** [add_data] maps a memory chunk [mem] as writable and
        nonexecutable segment of machine memory.  *)
        val add_data : mem -> (unit,#Context.t) m


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
          addr -> int -> (unit,#Context.t) m


        (** [map mem] maps a memory chunk [mem] to a segment with the
            given permissions. See also {!add_text} and {!add_data}. *)
        val map :
          ?readonly:bool ->
          ?executable:bool ->
          mem -> (unit,#Context.t) m
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

        Side note -- the type system doesn't contain the unit type, i.e.,
        the [0] type. An expression [()] evaluates to the [0:1] value.

        {2 Functions and expressions}

        Functions are named abstractions of code, where a code is a
        sequence of expressions. Since a value of an expression is
        a machine word, functions are not first-class values in Primus
        Lisp. However, functions and types can be manipulated on the
        meta-programming level.

        A function is defined with the [defun] form, that has the
        following syntax:

        {v
           (defun <name> (<p1> <p2> ... <pM>)
              <e1> <e2> .. <eN>)
        v}

        A function definition may optionally contain a documentation
        strings and a declaration section. For example,

        {v
         (defun strlen (p)
           "returns a length of the null-terminated string pointed by P"
           (declare (external "strlen"))
           (msg "strlen was called with $p")
           (let ((len 0))
             (while (not (points-to-null p))
               (incr len p))
             len))
        v}

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

        The {v (if <cond> <then> <else>) v} form is a basic control
        flow structure. If <cond> evaluates to a word that doesn't
        contain ones (e.g., 0:1, 00:2, ...) then the result of the
        <if> form is the result of the <else> expression. Otherwise it
        is the result of the <then> expression.

        Several derived forms are defined as macros, e.g.,
        {v

         (when <cond> <body>)
         (or <e1> <e2> .. <eM>)
         (and <e1> <e2> .. <eM>)
       v}

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

        {v
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
  (defsubst <ident> [<declarations>] [<syntax>] {<atom>})
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
      | (if <exp> <exp> <exp>)
      | (let ({<binding>}) <exp>)
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
        v}






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
        val create : ?docs:string -> string -> (word list -> 'a) -> 'a t
      end


      (** a list of primitives.  *)
      module type Primitives = functor (Machine : Machine.S) ->  sig


        (** a list of primitives defined in the Machine monad.  *)
        val defs : unit -> (Word.t,#Context.t) Machine.t Primitive.t list
      end


      (** a list of priomitives.  *)
      type primitives = (module Primitives)

      type error += Runtime_error of string


      (** Make(Machine) creates a Lisp machine embedded into the
          Primus [Machine].  *)
      module Make (Machine : Machine.S) : sig


        (** [failf msg a1 ... am ()] terminates a lisp machine, and
            correspondingly the Primus machine with the
            [Runtime_error].  *)
        val failf : ('a, unit, string, unit -> ('b, 'c) Machine.t) format4 -> 'a


        (** [link_primitives prims] provides the primitives [prims]   *)
        val link_primitives : primitives -> (unit, #Context.t) Machine.t
      end


      (** [init ?log ?paths features] initializes the Lisp machine.
          This function should be called by a plugin, that is
          responsible for providing lisp code. In the [bap] framework
          it is called by the [primus-lisp] plugin.  *)
      val init : ?log:formatter -> ?paths:string list -> string list -> unit
    end


    (** Primus error.  *)
    module Error : sig
      type t = error = ..


      (** returns a textual representation of an error  *)
      val to_string : t -> string


      (** [add_printer to_string] registers a printer.  *)
      val add_printer : (t -> string option) -> unit
    end
  end
end
