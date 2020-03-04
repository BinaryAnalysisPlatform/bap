open Core_kernel
open Bap_knowledge
open Regular.Std
open Bap.Std
open Monads.Std
open Bap_future.Std
open Bap_strings.Std

[@@@warning "-D"]

module Std : sig
  (** Primus - The Microexecution Framework.

      Primus is a microexecution framework that can be used to implement
      CPU and full system emulators, symbolic executers, static
      fuzzers, policy checkers, tracers, quickcheck-like test suites,
      etc.

      Primus is an extensible non-deterministic interpreter of BAP IR.
      Primus provides a set of extension points through which it is
      possible to track what it interpreter is doing, examine its
      state, and even change the semantics of operatons (to a limited
      extent). These extension points are called "observations" in
      Primus parlance. We user a simple publish/subscriber
      architecture, and subscriber's code is run in Primus monad that
      permits arbitrary mutation of the interpreter state.

      Primus implements a non-deterministic compuation model (here
      non-deterministic is used in a sense of the non-deterministic
      Turning Machine, when on each executing step machine can have
      more than one outcome). Two non-deterministic operations are
      provided: [fork] that clones current computations into two
      identical computations and [switch] that switches between
      computations. Other than these two operators, non-determinism is
      not observable as every thread of execution (called `machine' in
      our parlance) sees a totally deterministic word.

      Primus is built from components. The core components are:
      - Env - provides mapping from variables to values
      - Memory - provides mapping from memory locations to values;
      - Linker - provides mapping from labels to code;
      - Lisp - enables Lisp-like DSL.


      A new component could be added to Primus to extend its
      behavior. A component's [init] function is evaluated when
      Primus starts and it usually registers handlers for
      observations.

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

  module Primus : sig
    (** Machine Exception.

        The exn type is an extensible variant, and components
        usually register their own error constructors. *)
    type exn = ..

    (** [an observation] of a value of type [an].*)
    type 'a observation

    (** [a statement] is used to make an observation of type [a].    *)
    type 'a statement

    (** a cancelable subscription to an observation.
        @since 2.1.0 *)
    type subscription


    (** a system definition  *)
    type system

    (** a result of computation  *)
    type value [@@deriving bin_io, compare, sexp]

    (** Machine exit status.
        A machine may terminate normally, or abnormally with the
        specified exception. *)
    type exit_status =
      | Normal
      | Exn of exn

    (** An abstract type that represents an effect produced by a
        Machine run. That type is left abstract, and has no
        operations, as its purpose is to disallow running machine
        directly, without an instantiation of the [Machine.Main]
        module. *)
    type 'a effect

    (** value generator  *)
    type generator

    (** Machine Observation.

        The Primus Framework is built on top of the Machine
        observations. The Machine components make their own
        observations, based on observation made by other components.

        A value of type ['a observation] is a first-class
        representation of an event of type ['a]. While machine
        components are functors, the values of type observation should
        not depenend on the type of the functor.*)
    module Observation : sig

      (** An observation provider.
          A provider facilitates introspection of the Primus Machine,
          for the sake of debugging and dumping the effects. The
          provider should not (and can't be) used for affecting the
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
          providers. The data stream is expressed as a stream of s-expressions.
      *)
      module Provider : sig
        type t = provider

        (** unique name of a provider *)
        val name : t -> string

        (** a total number of observers that subscribed to this provider  *)
        val observers : t -> int

        (** a stream of occurrences of this observation  *)
        val triggers : t -> unit stream

        (** a data stream from this observation *)
        val data : t -> Sexp.t stream
      end
    end

    (** A hierarchical program position.

        The [Pos.t] is a cursor-like data structure, that
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


      (** [get a p] get a value of the attribute [a] associated with
          the given position [p]. Example, [Pos.get address p] returns
          a machine address of the position [p]. *)
      val get : 'a tag -> t -> 'a option

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

      (** Machine identifier type.   *)
      type id = Monad.State.Multi.id

      (** [init] event occurs just after all components have been
          initialized, and before the execution starts*)
      val init : unit observation

      (** The [finished] event occurs when the machine terminates.   *)
      val finished : unit observation

      (** [exn_raised exn] occurs every time an abnormal control flow
          is initiated *)
      val exn_raised : exn observation


      (** [kill id] occurs when the machine [id] is killed.

          When this observation is made the machine enters the
          restricted mode with non-determinism and observations
          disabled.  *)
      val kill : id observation


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


          (** [subscribe obs handler] creates a cancelable
              subscription to the observation [obs].

              Returns a subscription handler that could be used to
              cancel the subscription.

              @since 2.1.0 *)
          val subscribe : 'a observation -> ('a -> unit t) -> subscription t

          (** [cancel sub] cancels the given subscription.

              An observation that was registered under this
              subscription won't be called anymore.

              @since 2.1.0  *)
          val cancel : subscription -> unit t


          (** [watch prov data] watches for the data provider.

              This function is the same as [observe] except that it
              uses the provider to access the observation and gets the
              observation in the sexp-serialized form. *)
          val watch : Observation.provider -> (Sexp.t -> unit t) -> unit t

          (** [make observation event] make an [observation] of the
              given [event].  *)
          val make : 'a statement -> 'a -> unit t

          (** [post observation k] makes observation if necessary.

              The continuation [k] is a function that is called only
              when the given statement has subscribers.

              Use this function to skip creating an observation if
              nobody is interested in it. This is useful, when the
              observation has some cost to construct, so when there no
              subscribers no machine cycles will be lost.

              The function [k] receives a [provide] function that could
              be used to provide observation once it is ready, e.g.,
              {[

                Observation.post big_thing ~f:(fun provide ->
                    some_costly_function1 >>= fun x ->
                    some_costly_function2 >>= fun y ->
                    some_costly_function3 >>= fun z ->
                    provide (x,y,z))
              ]}

              Note: even for observations that are tuples this
              function is efficient as sometimes the compiler
              can optimize the closure creation or the closure
              itself might be smaller than the created observation.

              @since 2.1.0
          *)
          val post : 'a statement -> f:(('a -> unit t) -> unit t) -> unit t
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
                                           ?boot:unit t ->
                                           ?init:unit t ->
                                           (exit_status * project) m effect

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


        (** [project] is a computation that results with the project
            data structure. Note, that Machine is a State monad
            with the [env] type equal to [project], thus [project] is
            a shortcut to [get ()].

            You can use [put project] to update the project data structure.*)
        val project : project t


        (** [program] program representation  *)
        val program : program term t


        (** [arch] code architecture  *)
        val arch : arch t


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


      (** The Legacy Main System.

          This module together with the {!add_component} function
          builds the legacy main system ([primus:legacy-main]).  The
          [add_component] function adds a component to this system.

          The built system could be obtained with the
          {!legacy_main_system} function and run as usual via the
          {!System} module. The old [Main(M).run] interface is still
          provided for backward compatibility.

          This interface is deprecated and is provided for backward
          compatibility. Use the {!System} interface to define and
          run Primus systems.
      *)
      module Main(M : S) : sig

        (** [run ?envp ?args proj] returns a computation that will
            run a program represented with the [proj] data structure.

            The [envp] and [args] parameters are constants, and can be
            accessible during the computation using [Machine.envp] and
            [Machine.argp].

            The computation evaluates to a pair [(result,project)]
            where result is a result of computation and project can be
            modified by the [primus] components, e.g., annotated with
            attributes, etc. *)
        val run :
          ?envp:string array ->
          ?args:string array ->
          project ->
          unit M.t ->
          (exit_status * project) M.m
      end
      [@@@deprecated ["[since 2020-03] use Primus.System instead"]]


      (** {3 The Deprecated Legacy Main system interface}  *)

      (** [add_component comp] registers the machine component [comp] in the
          Primus Framework.
          The component's [init] function will be run every time the
          Machine compuation is run. After all components are
          initialized, the [init] observation is made.

          The components shall not access the interpreter in their
          [init] function. Instead, they should subscribe to
          observations and/or initialize the machine state via
          Linker/Memory/Env components.

          See also a more general [register_component] function.
      *)
      val add_component : component -> unit
      [@@deprecated "[since 2020-03] use Components.register* instead"]


      (** returns the [primus:legacy-main] system that is composed of
          all components added via the {!add_component} function.
      *)
      val legacy_main_system : unit -> system
      [@@deprecated "[since 2020-03] use the System interface"]

    end

    (** A runnable instance of Primus Machine.

        A collection of components defines a runnable instance of Primus
        Machine. Systems could be defined programmatically, using this
        interface, or in system description files.

        The system definition holds enough information to initialize
        and run the system.

        {2 System's timeline}

        A system consequently passes through the following main phases of its
        life:
        - initialization (booting);
        - post-init;
        - running;
        - post-running;
        - stopped.

        {3 The initialization phase}

        During the initialization phase, the [init] method of all
        components is run. The observations are blocked in this phase
        and other components might be unitialized in this phase
        (components are initialized in an unspecified order).
        Components should subscribe to other observations and register
        their Lisp primitives in this phase.

        Components should minimize the side-effects on the Primus
        machine and do not use Interpreter, Linker, and/or any
        observable operations. In this phase the Primus Machine
        operates in a deterministic mode and fork/switch operators are
        disabled.

        Once this phase is complete, the [init] observation is posted
        and the system enters the post-init phase.


        {3 The post-init phase}

        The post-init phase starts after the [init] observation is
        posted. During this phase observation and non-determinism are
        enabled. This phase is used by the components that would like
        to change the initial state of the Machine (i.e., initialize
        variables, possibly non-deterministically, link code, etc).

        Components that need this kind of initialization shall
        subscribe to the [init] observation and perform the necessary
        post-initialization in the handler.

        This stage is used to prepare the Machine for the execution. Once
        it is finished the [start] observation is posted.

        {3 The running phase}

        The [start] observation designates the start of the execution
        and the code that is attached to this observation denotes the
        main function of the system. It is possible that there is more
        than one component attach their behavior to the [start] event,
        in that case all the components will be run in an unspecified
        order.

        It is not strictly required that a system should have
        components that are executed in the running phase, as when a
        system is run it is possible to provide the code that is run,
        during the start phase (as well as the code that is run
        during, the post-init phase), see {!System.run} below.

        {3 The post-running phase}

        After the code attached to the start phase terminates, either
        normally or via the Primus exception, the [fini] observation
        is posted and the system enters the post-running phase. This
        is a non-deterministic phase and components of the system
        might resume running by switching the computation to another
        fork, therefore, the system can enter this phase multiple
        times (but exit only once).

        Once the post-running phase is finally finished, the machine
        enters the final [stopped] phases.

        {3 The stopped phase}

        This is the final phase and, like the initial phase,
        observations and non-determinism are disabled. This phase
        could be used to summarize the information that was obtained
        during the system run.

        When the system enters the stopped state it is no longer
        possible to restart it and all computations that are run
        during this phase will not be observable.

        {2 Non-determinism and machine stopping}

        Since Primus Machine is non-deterministic, for the given
        system we can observe more than one finalizations of
        computations. Usually, schedulers use the [fini] observation
        to kill the finished machine and switch to another machine.

        When a machine is killed the [killed] observation is posted
        that could be used to summarize the machine. After the
        [killed] observation is posted, the machine (not the system)
        enters the machine stopped phase in which observations and
        non-determinism are blocked (it is only possible to update
        the project data structure or record the information in the
        knowledge base).

        @since 2.1.0
    *)
    module System : sig
      type t = system

      (** designates some component *)
      type component_specification

      (** designates a system  *)
      type system_specification

      (** [define name] defnines a new system.

          The system designator is built from [package] and [name] and
          could be used to reference this system from the user
          interface. The designator is not required to be unique and
          is not registered anywhere in the library.

          @param desc a human-readable description of the system, its
          task and purposes

          @param components a list of components that comprise the
          system.
      *)
      val define :
        ?desc:string ->
        ?depends_on:system_specification list ->
        ?components:component_specification list ->
        ?package:string -> string -> t


      (** [add_component ?package system name] adds a component to the
          system.

          Adds the component designated by the given package and name
          to the [system]. *)
      val add_component : ?package:string -> t -> string -> t

      (** [run system project state] runs the analysis defined by [system].

          Initializes all components and triggers the [init]
          observation after that. Once the observation is processed
          runs the [fini] event.

          The components that comprise the [system] must schedule
          their evaluation on one of the observations.

          The [project] and [state] are passed as the initial static
          representation of the program and the initial knowledge. The
          result of the analysis is either a failure that indicates a
          conflicting knowledge or a success that includes an updated
          program representation, computation exit status, and possibly
          extend knowledge.

          See the [Job.run] function if you want to run Primus
          instances in a batch mode.

          @param envp an array of environment variables that are passed
          to the program
          @param args an array of program parameters, with the first
          element of array being the program name.
      *)
      val run :
        ?envp:string array ->
        ?args:string array ->
        ?init:unit Machine.Make(Knowledge).t ->
        ?start:unit Machine.Make(Knowledge).t ->
        system -> project -> Knowledge.state ->
        (exit_status * project * Knowledge.state, Knowledge.conflict) result


      (** [init ()] is posted when all components finished their
          initializations. It is not posted if components failed to
          initialize the system.  *)
      val init : unit observation


      (** [start sys] occurs after the system [sys] starts   *)
      val start : string observation


      (** [fini ()] is posted when all computations are
          finished. This observation is posted only if [init] was posted,
          i.e., if the system wasn't initialized then neither [init]
          nor [fini] will happen. *)
      val fini : unit observation

      val stop : string observation


      (** [name system] is the system designator.  *)
      val name : system -> Knowledge.Name.t

      (** prints the system definition.  *)
      val pp : Format.formatter -> system -> unit

      (** {3 Component specification language}  *)

      (** [component ?package name] specifies the component with the
          given designator.

          @param package defaults to [user].
      *)
      val component : ?package:string -> string -> component_specification


      (** [depends_on ?package name] specifies a dependency on a
          system with the given designator.
      *)
      val depends_on : ?package:string -> string -> system_specification

      (** {3 Parsing system descriptions from files}

          A system can be described in a system description file that
          has the following format (closely follows Common Lisp's asdf
          format)

          {v
             systems ::= <system-definition> ...
             system-definition ::= (defsystem <ident> <option> ...)
             option ::=
               | :description <string>
               | :components (<component> ...)
             component ::= <ident>
          v}
      *)


      (** a parsing error description  *)
      type parse_error

      (** [from_file name] parses a list of system descriptions from
          the file with the given [name].   *)
      val from_file : string -> (system list,parse_error) result

      (** prints the parse error  *)
      val pp_parse_error : Format.formatter -> parse_error -> unit

      (** {3 Interface to generic systems}

          Generic systems are not specialized to the Knowledge monad
          and could be run on any instance of the Primus monad.

          Unlike the specialized [run] function the generic [run]
          function is a functor parameterized by a monad and returns a
          value wrapped into that monad.
      *)
      module Generic(Machine : Machine.S) : sig


        (** [run system project] runs the [system] on the specified [project].

            @param envp an array of environment variables that are passed
            to the program;

            @param args an array of program parameters, with the first
            element of array being the program name

            @param init is a computation that will be run just after
            the system is initialized but before the [init]
            observation is posted.
        *)
        val run :
          ?envp:string array ->
          ?args:string array ->
          ?init:unit Machine.t ->
          ?start:unit Machine.t ->
          t -> project -> (exit_status * project) Machine.m
      end

      module Repository : sig
        val add : system -> unit
        val get : ?package:string -> string -> system
      end
    end


    (** A task to run a Primus system.

        A is a system together with input parameters that is run via
        the {!Jobs} module.x

        @since 2.1.0
    *)
    module Job : sig

      (** an abstract type for jobs  *)
      type t

      (** the job name, doesn't have to be unique a bears any sense  *)
      val name : t -> string

      (** desc describes what the job is doing  *)
      val desc : t -> string

      (** an array of environment variables  *)
      val envp : t -> string array

      (** an array of execve parameters   *)
      val args : t -> string array

      (** the system that this job runs  *)
      val system : t -> system
    end

    (** A facility to register and run multiple instances of Primus.

        This interface allows only the analysis (specialized) systems.

        @since 2.1.0
    *)
    module Jobs : sig

      (** an action to take after each job.  *)
      type action = Stop | Continue

      (** the final result of running the job queue.  *)
      type result


      (** [enqueue system] creates a new job and enqueues it for future run.

          @param name a short name of the job for logging (defaults to unnamed)
          @param desc a short description of the job task (defaults to [""])
          @param envp the array of environment variables
          @param argv the array of commandline arguments
          @param init runs after machine boots
          @param start runs after machine is initialized
      *)
      val enqueue :
        ?name:string ->
        ?desc:string ->
        ?envp:string array ->
        ?args:string array ->
        ?init:unit Machine.Make(Knowledge).t ->
        ?start:unit Machine.Make(Knowledge).t ->
        system -> unit


      (** [pending ()] is the number of jobs still waiting to be run.  *)
      val pending : unit -> int


      (** [run project state] runs until there are no more jobs queued
          or until explicitly stopped.

          The [project] and [state] values are used as the initial
          static program representation and knowledge base.

          Every time a job is finised either [on_success] or
          [on_conflict] is called. These callbacks shall return either
          [Stop] or [Continue]. If either returns [Stop] then [run]
          also stops even if there are more jobs in the queue (the
          jobs are not cleared, so [pending ()] might be non-zero).

          Both of the callbacks return [Continue] by default.

          @param on_conflict is called as [on_conflict sys conflict]
          when the system [sys] failes to converge and stops with the
          knowledge [conflict]. The returned value should indicate
          what to do next.

          @param on_success is called as [on_success sys status kb]
          when the system [sys] terminates without knowledge conflicts
          with the exit [status] and the knowledge stored in [kb]. The
          returned value prescribes what to do next.

          Note that the job queue could be extended during analysis,
          the [run] function will execute all jobs in the FIFO order,
          unless explicitly stopped.
      *)
      val run :
        ?on_failure:(Job.t -> Knowledge.conflict -> action) ->
        ?on_success:(Job.t -> exit_status -> Knowledge.state -> action) ->
        project -> Knowledge.state -> result

      (** [knowledge result] is the knowledge obtained from running
          the jobs.  *)
      val knowledge : result -> Knowledge.state


      (** [project result] is the final static representation of program.  *)
      val project : result -> project


      (** [failures result] is the list of failed jobs.

          Each failed is a pair of the job and and the description of
          the conflict that prevented system from convering.

          The failures are specified in the order in which they happened.
      *)
      val failures : result -> (Job.t * Knowledge.conflict) list


      (** [finished result] is the final list of jobs that were run
          in the order in which they were run.  *)
      val finished : result -> Job.t list

    end

    (** The Machine component.  *)
    type component = Machine.component


    (** A Primus Machine parameterized with the Knowledge monad.

        This is an instance of the Primus machine that is
        parameterized by the Knowledge monad that gives accecss to the
        knowledge base directly from the Primus computation.

        The knowledge base should be used by analyses to store their
        results as well as a communication media between different analyses.

        New analyses are added in the form of machine components
        using the [Components.register] function.

        @since 2.1.0
    *)
    module Analysis : sig
      open Knowledge
      include Machine.S with type 'a m = 'a Knowledge.t
                         and type 'a t = 'a Machine.Make(Knowledge).t


      (** {3 Common knowledge operations lifted into the Primus monad}

          Note, that any Knowledge computation could be used in the
          Analysis monad using the [lift] function, e.g.,

          [lift@@Knowledge.Object.create Theory.Program.cls]

          The functions below are lifted covenience.
      *)

      (** [collect p x] is lifted {!Knowledge.collect}.  *)
      val collect : ('a,'p) slot -> 'a obj -> 'p t


      (** [resove p x] is lifted {!Knowledge.resolve}.  *)
      val resolve : ('a,'p opinions) slot -> 'a obj -> 'p t


      (** [provide p x v] is lifted {!Knowledge.provide}.  *)
      val provide : ('a,'p) slot -> 'a obj -> 'p -> unit t


      (** [suggest a p x v] is lifted {!Knowledge.suggest}.  *)
      val suggest : agent -> ('a,'p opinions) slot -> 'a obj -> 'p -> unit t

    end


    (** A registry of machine components.

        We distinguish between two kinds of machine components:
        - analyses (aka specialized components)
        - generics (aka general components).

        Analyses are specialized components that can have access to
        the knowledge base and are much easier to write, contrary to
        generics that are represented as functors.

        Generic components are applicable to any instantiation of the
        Primus monad, while analyses are only applicable to the Primus
        monad parameterized by the Knowledge Monad. If you are not
        sure which to use, then use analyses.

        When a specialized instance of the Primus monad is run (via
        [System.run] any specialized component overrides a generic
        component with the same name).

        @since 2.1.0
    *)
    module Components : sig


      (** [register name analysis] registers an analysis under given name.

          Fails, if there is already a component with the same name.
      *)
      val register :
        ?desc:string ->
        ?package:string ->
        string -> unit Analysis.t ->
        unit

      (** [register_generic name comp] registers a generic component.   *)
      val register_generic :
        ?desc:string ->
        ?package:string ->
        string -> component ->
        unit
    end

    (** type abbreviation for the Machine.state  *)
    type 'a state = 'a Machine.state

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
            with new identifier. Only setting and reading a variable
            preserves value identifiers. Each new constaint or
            arithmetic, or memory expression produces a value with a
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

            A value can have a symbolic representation that is useful
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

    (** Machine time.

        Each machine has its own clock that is incremented on each
        operation. When machine is forked, the derived machine
        inherits the clock value from the parent machine.

        @since 2.1.0
    *)
    module Time : sig
      type t [@@deriving sexp_of]

      (** [clocks t] is the time [t] expressed in clocks from the start of machine.

          @since 2.1.0
      *)
      val clocks : t -> int

      (** [of_clocks clk] represents a time duration equal to the
          specified number of clocks.  *)
      val of_clocks : int -> t

      (** a string representation of time  *)
      val to_string : t -> string

      (** time printer  *)
      val pp : Format.formatter -> t -> unit
      include Base.Comparable.S with type t := t
    end


    (** The Interpreter.

        The Interpreter is the core component of the Primus Machine. It
        provides lots of observations, giving other components an
        ability to track every event that happens during the program
        evaluation. The components can affect the results of
        evaluation in a limited way, by affecting the state of the
        components that are used by the Interpreter, name the
        Environment and the Memory.

        Note: we the [observation (x,y,z)] notation in the
        documentation to denote an observation of a value represented
        with the [(x,y,z)] tuple, that in fact corresponds to
        [observation >>> fun (x,y,z)] -> ... *)
    module Interpreter : sig

      (** [clock] occurs every time the machine clock changes its value.  *)
      val clock : Time.t observation

      (** [pc_change x] happens every time a code at address [x] is executed.  *)
      val pc_change : addr observation


      (** [loading x] happens before a value from the address [x] is loaded
          by the interpreter from the memory.  *)
      val loading : value observation


      (** [loaded (addr,value)] happens after the [value] is loaded
          from the address [addr].  *)
      val loaded : (value * value) observation


      (** [storing x] happens before a value is stored at address [x]  *)
      val storing : value observation


      (** [stored (addr,value)] happens after the [value] is stored at
          the address [addr] *)
      val stored : (value * value) observation


      (** [reading x] happens before the variable [x] is read from the
          environment. *)
      val reading : var observation

      (** [read (var,x)] happens after a variable [var] is evaluated
          to the value [x] *)
      val read : (var * value) observation

      (** [writing v] happens before a value is written to the variable [v]  *)
      val writing : var observation

      (** [written (v,x)] happens after [x] is assigned to [v]  *)
      val written : (var * value) observation


      (** [jumping (cond,dest)] happens just before a jump to [dest]
          is taken under the specified condition [cond].
          Note: [cond] is always [true]

          @since 1.5.0 *)
      val jumping : (value * value) observation

      (** [eval_cond v] occurs every time the [cond] part of a jump is
          evaluated.

          @since 1.5.0 *)
      val eval_cond : value observation

      (** [undefined x] happens when a computation produces an
          undefined value [x].  *)
      val undefined : value observation

      (** [const x] happens when a constant [x] is created *)
      val const : value observation

      (** [binop ((op,x,y),r)] happens after the binary operation [op]
          is applied to values [x] and [y] and evaluates to [r] *)
      val binop : ((binop * value * value) * value) observation

      (** [unop ((op,x),r)] happens after the unary operation [op] is
          applied to [x] and results [r] *)
      val unop : ((unop * value) * value) observation

      (** [cast ((t,x),r)] happens after [x] is casted to [r] using
          the casting type [t] *)
      val cast : ((cast * int * value) * value) observation

      (** [extract ((hi,lo,x),r)] happens after [r] is extracted from [x] *)
      val extract : ((int * int * value) * value) observation

      (** [extract ((x,y),z)] happens after [x] is concatenated with [y]
          and produces [z] as a result.*)
      val concat : ((value * value) * value) observation

      (** [ite ((cond, yes, no), res)] happens after the ite expression
          that corresponds to ite([cond], [yes], [no]) is evaluated
          to [res]. *)
      val ite : ((value * value * value) * value) observation

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

      (** occurs on [halt] operation  *)
      val halting : unit observation

      (** [interrupt n] occurs on the machine interrupt [n] (aka CPU
          exception) *)
      val interrupt : int observation

      (** [division_by_zero] occurs just before the division by zero trap
          is signaled.

          See the [binop] operation and [division_by_zero_handler] for more
          information.
          @since 1.5  *)
      val division_by_zero : unit observation

      (** [pagefault x] occurs just before the pagefault trap is signaled.

          See [load] and [store] operations, and [pagefault_handler] for
          more information.
          @since 1.5 *)
      val pagefault : addr observation

      (** [segfault x] occurs when an invalid memory operation is performed
          on the address [x]. See the [load] and [store] operations for more.
          @since 1.5  *)
      val segfault : addr observation

      (** [cfi_violation x] occurs when the CFI is not preserved.
          The control flow integrity (CFI) is violated when a call
          doesn't return to an expected place. This might be an
          indicator of malicious code or an improper control flow
          graph.

          After the observation is made the [cfi_violation] trap is
          signaled, which could be handled via the
          [cfi_violation_handler].
          @since 1.7  *)
      val segfault : addr observation

      (** is raised when a computation is halted *)
      type exn += Halt

      (** is raised by a machine that attempts to divide by zero  *)
      type exn += Division_by_zero

      (** is raised when a memory operation has failed. *)
      type exn += Segmentation_fault of addr


      (** [pagefault_hanlder] is a trap handler that is invoked when the [Pagefault]
          exception is raised by the machine memory component. If the handler is
          provided via the Linker, then it is invoked, otherwise a segmentation
          fault is raised. If the handler returns normally then the faulty operation is
          repeated.

          Note, page faults are usually handled together with the [pagefault]
          observation.

          @since 1.5
      *)
      val pagefault_handler : string

      (** [division_by_zero_hanlder] is a trap handler for
          the Division_by_zero exception. If it is linked into the machine,
          then it will be invoked when the division by zero trap is signaled.
          If it returns normally, then the result of the faulty operation is
          undefined.

          @since 1.5
      *)
      val division_by_zero_handler : string



      (** [division_by_zero] is the name of a trap handler for the
          [Cfi_violation] exception. If it is linked into the machine,
          then it will be invoked when the cfi-violation trap is signaled.
          If it returns normally, then the result of the faulty operation is
          undefined. *)
      val cfi_violation_handler : string

      (** Make(Machine) makes an interpreter that computes in the
          given [Machine].  *)
      module Make (Machine : Machine.S) : sig
        type 'a m = 'a Machine.t


        (** [time] returns the value of the machine clock.

            @since 2.1.0
        *)
        val time : Time.t m

        (** [halt] halts the machine by raise the [Halt] exception.  *)
        val halt : never_returns m

        (** [interrupt n] interrupts the computation with cpuexn [n]  *)
        val interrupt : int -> unit m

        (** [pc] current value of a program counter.*)
        val pc : addr m

        (** [pos m] current program position.  *)
        val pos : pos m

        (** [sub x] computes the subroutine [x].  *)
        val sub : sub term -> unit m

        (** [blk x] interprets the block [x].  *)
        val blk : blk term -> unit m

        (** [exp x] returns a value of [x]. *)
        val exp : exp -> value m

        (** [get var] reads [var]  *)
        val get : var -> value m

        (** [set var x] sets [var] to [x]  *)
        val set : var -> value -> unit m

        (** [binop op x y] computes a binary operation [op] on [x] and [y].

            If [binop op x y] will involve the division by zero, then the
            division by zero trap is signaled. If the
            [division_by_zero_handler] is provided, (i.e., is linked) then
            it will be invoked. If it returns normally, then the result of
            the [binop op x y] is undefined. Otherwise, the [Division_by_zero]
            machine exception is raised. *)
        val binop : binop -> value -> value -> value m

        (** [unop op x] computes an unary operation [op] on [x]  *)
        val unop : unop -> value -> value m

        (** [cast t n x] casts [n] bits of [x] using a casting type [t]  *)
        val cast : cast -> int -> value -> value m

        (** [concat x y] computes a concatenation of [x] and [y]  *)
        val concat : value -> value -> value m

        (** [extract ~hi ~lo x] extracts bits from [lo] to [hi] from
            [x].  *)
        val extract : hi:int -> lo:int -> value -> value m

        (** [const x] computes the constant expression [x]  *)
        val const : word -> value m

        (** [load a d s] computes a load operation, that loads a word
            of size [s] using an order specified by the endianness [d]
            from address [a].

            If the address [a] is not mapped, then a pagefault trap
            is signaled. If the [pagefault_hanlder] is provided, then
            it is invoked and the load operation repeats. Note, the
            handler either shall not return or ensure that the
            second attempt would be successful. If no handler is linked,
            then the segmentation fault machine exception is raised. *)
        val load : value -> endian -> size -> value m

        (** [store a x d s] computes a store operation, that stores at
            the address [a] the word [x] of size [s], using an
            ordering specified by the endianness [d].

            If [a] is not mapped or not writable then the pagefault
            trap is invoked. If the handler is provided, then it is
            invoked and the operation is repeated. Otherwise the
            [Segmentation_fault] machine exception is raised.  *)
        val store : value -> value -> endian -> size -> unit m
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
      ] [@@deriving bin_io, compare, sexp]


      (** Call tracing.

          Linker doesn't operate in terms of functions or subroutines,
          but rather in terms of executable chunks of code. It is
          convenient, though, to track called functions, i.e., there
          are names and arguments (data-flow). Since a code in Primus
          is an uniterpreted computation it is the responsibility of
          the code provider to make corresponding observations, when a
          subroutine is entered or left.

          By default, the code is provided by the BIR Interpreter and
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

        (** occurs when an externally linked primus stub is called.
            Arguments values are specified in the same order as in
            the [call] observation *)
        val lisp_call : (string * value list) observation

        (** occurs just before a lisp call from an external procedure
            returns. Arguments values are specified in the same order as in
            the [return] observation *)
        val lisp_call_return : (string * value list) observation

        (** {3 Notification interface}

            Use [Machine.Observation.make] function, where [Machine]
            is a module implementing [Machine.S] interface, to provide
            observations.
        *)

        (** the statement that makes [call] observations. *)
        val call_entered : (string * value list) statement

        (** the statement that makes [return] observations  *)
        val call_returned : (string * value list) statement

        (** the statement that makes [lisp-call] observations  *)
        val lisp_call_entered : (string * value list) statement

        (** the statement that makes [lisp-call-return] observations  *)
        val lisp_call_returned : (string * value list) statement

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
            an identifier was already bound to some other code
            fragment, then the old binding will be substituted by the new
            one.  *)
        val link :
          ?addr:addr ->
          ?name:string ->
          ?tid:tid ->
          code -> unit m

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


        (** [resolve_tid name] returns the term identifier associated
            with the given [name].

            @since 1.5.0
        *)
        val resolve_tid : name -> tid option m


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


      (** Infinite iterators produces infinite sequences.  *)
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
              regular generator.

              Caveats:

              The [init] function can use only one of the two
              generator constructors to create a generator:
              - [Random.lcg]
              - [Random.byte]
          *)
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

    (** Evaluation environment.

        The Environment binds variables to values.*)
    module Env : sig

      (** A variable is undefined, if it was never [add]ed to the
          environment.  *)
      type exn += Undefined_var of var


      (** [Env = Make(Machine)]  *)
      module Make(Machine : Machine.S) : sig

        (** [get var] returns a value associated with the variable.
            Todo: it looks like that the interface doesn't allow
            anyone to save bottom or memory values in the environment,
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


        (** [all] is a sequence of all variables defined in the
            environment. Note, the word _defined_ doesn't mean
            initialized.   *)
        val all : var seq Machine.t
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
        val add_text : mem -> unit Machine.t

        (** [add_data] maps a memory chunk [mem] as writable and
            nonexecutable segment of machine memory.  *)
        val add_data : mem -> unit Machine.t


        (** [allocate addr size] allocates a segment of the specified
            [size]. An unitilialized reads from the segment will
            produce values generated by a generator (defaults to a
            [Generator.Random.Seeded.byte]).

            If [init] is provided then the region is initialized.

            An attempt to write to a readonly segment, or an attempt to
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

        Primus Lips is a low-level language that doesn't provide many
        abstractions, as it tries to be as close to the machine
        language as possible. In that sense Primus Lisp can be seen as
        an assembler, except that it can't really assemble binaries,
        as it operates over already existing and assembled
        program. Primus Lisp, however is still quite powerful, as the
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
        - methods;
        - macros;
        - functions;

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

        A collection of files is called a library.  To use features
        provided by another file, the file should be requested with
        the [(require <ident>)] form, where [<ident>] is the name of
        the feature. A file with the requested name is searched in the
        library, and loaded, making all its definitions available in
        the lexical scope, that follows the [require] form. If a
        feature is already provided, then nothing
        happens. Dependencies should not contain cycles.

        Top-level declarations specify attributes that are shared by
        all definitions in a file. For example, a declaration

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

        Primus Lips has a gradual type system.  A type defines all
        possible values of an expression. In Primus Lisp, expression
        values can be only scalar, i.e., machine words of different
        widths. The width is always specified in the number of
        bits. We denote a type of an expression with a decimal number,
        e.g., [(exp : 16)] means that an expression ranges over all
        16-bit-wide words.

        An expression can have a polymorphic type [any] that means
        that there are no static guarantees about the term
        type. Branching expressions in Primus Lisp are relaxed from
        typing (so the type of the [if] form depends on the
        condition). In other words, the type of a branching expression
        is always [any].

        {2 Functions and expressions}

        Functions are named abstractions of code, where a code is a
        sequence of expressions. Since a value of an expression is
        a machine word, functions are not first-class values in Primus
        Lisp. However, functions and types can be manipulated on the
        meta-programming level.

        A function is defined with the [defun] form, that has the
        following syntax:

        {[
          (defun <name> (<arg> ...) <exp> ...)
        ]}

        A list of arguments (that can be empty) defines function
        arity. Functions in Primus Lisp has fixed arity, unlike
        macros.

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

        {v (<name> <exp> ...) v}

        The first element of the function application form is not an
        expression and must be an identified. The rest arguments are
        expressions, that are evaluated from left to right. All
        arguments are passed by value.

        The body of a function is a sequence of expressions, that is
        evaluated in the lexical order (i.e., from left to right). A
        value of the last expression is the result of the function
        evaluation. An expression is either a function application or
        or a special form. Primus Lisp defines only 5 special forms,
        the rest of the syntax is defined using the macro system.

        {3 Conditionals}

        The {b (if <test-expr> <then-expr> <else-expr> ...)} form is a basic
        control flow structure. If {b <test-expr>} evaluates to a non-zero word
        then the result of the {b <if>} form is the result of evaluation
        of the {b <then-expr>}, otherwise a sequence of {b <else-expr>} ...
        is evaluated and the result of the form evaluation
        would be a result of the last expression in a form

        For example,

        {v
        (if (< 4 3)
            (msg "shouldn't happen")
          (msg "that's right")
          (- 4 3))
        v}

        Note that the the {b <else-expr> } sequence maybe empty.

        Several derived forms are defined as macros, e.g.,

        {v
          (when <cond> <expr> ...)
          (or <expr> ...)
          (and <expr> ...)
        v}

        {3 Loops}

        Iterations can be implemented either using recursion or with
        the [while] special form. Since the interpreter doesn't
        provide the tail-call optimization it is better to use the
        latter (although the interpreter itself is using a constant
        stack size, as it uses the host language heap memory to
        represent the Primus Lisp call stack).

        The {b (while <cond> <expr> ...) } form, will evaluate the
        <cond> expression first, and if it is a non-zero value, then
        the sequence of expressions {b <expr> ... } is evaluated, and
        the value of the last expression becomes the value of the
        [while] form. If the value of the {b <cond> } expression is a
        false value, then this value becomes the value of the [while]
        form.


        {3 Variables}

        The [let] form binds values to names in the lexical scope.

        {v
         (let (<binding> ...)  <body-expr> ...)
         binding ::= (<var> <expr>)
        v}

        Evaluates each {b <binding>} in order binding the {b <var>}
        identifier to a result of {b <expr>}. The newly created
        binding is available in consequent bindings and in the
        <body-expr>, but is not visible outside of the scope of the
        let-form.

        Example,
        {v
        (let ((x 4)
              (y (+ x 2)))
          (+ x 3))
        v}

        The value of the [let] form is the value of the last
        expression {b <sN> }.


        {3 Sequencing }

        The {b (prog <expr> ...) } form combines a sequence of
        expressions into one expression, and is useful in the contexts
        where an expression is required. The expressions are evaluated
        from left to right, and the value of the [prog] form is the
        value of the last expression.

        {3 Messaging }

        The {b (msg <fmt> <expr> ...) } form constructs
        logging/debugging messages using an embedded formatting
        language. The formed message will be sent to the logging
        facility, that was set up during the Primus Lisp library
        initialization.

        The format language interprets all symbols literally, unless
        they start with the dollar sign ($).

        A pair of characters of the form {b $<n> }, where {b <n> }
        is a decimal digit, will be substituted with the value of the
        n'th expression (counting from zero).

        Example,

        {v (msg "hello, $0 $0 world, (+ 7 8) = $1" "cruel" (+ 7 8)) v}

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

        {v
         (defconstant <name> <atom>)
         (defconstant <name> <docstring> <atom>)
         (defconstant <name> <declarations> <atom>)
         (defconstant <name> <docstring> <declarations> <atom>)
        v}

        For example,

        {v (defconstant main-address 0xDEAD) v}


        During the program parsing, each occurrence of the
        {b <name> } term will be rewritten with the {b <value> }
        term, that should be an atom.

        {3 Substitutions}

        The syntactic substitution is a generalization of syntactic
        constant, and has quite a similar syntax:

        {v
          | (defsubst <name> <value> ...)
          | (defsubst <name> <declarations> <value> ...)
          | (defsubst <name> :<syntax> <value> ...)
          | (defsubst <name> <declarations> :<syntax> <value> ...)
        v}


        During parsing, every occurrence of the term {b <name> } (that
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

        {v
          (defmacro <name> (<param> ...) <value>)
          (defmacro <name> (<param> ...) <docstring> <value>)
          (defmacro <name> (<param> ...) <declarations> <value>)
          (defmacro <name> (<param> ...) <docstring> <declarations> <value>)
        v}

        A macro definition adds a term rewriting rule, that rewrites
        each occurrence of {b (<name> <arg> ...) }, where the number
        of arguments [N] is greater or equal then the number of
        parameters [M], with the {b <value> } in which occurrences of the
        [i]th parameter is substituted with the term [i]th argument. If [N] is
        bigger than [M], then the last parameter is bound with the
        sequence of arguments  {b <argM>...<argN> }.

        The macro subsystem doesn't provide any specific looping or
        control-flow facilities, however, the macro-overloading
        mechanism along with the recursion make it possible to encode
        arbitrary meta-transformations.

        Other than a standard context-based ad-hoc overloading
        mechanism, the macro application uses the arity-based
        resolution. As it was described above, if a number of
        arguments is greater than the number of parameters, then the
        last parameter is bound to the rest of the arguments. When
        several macro definitions matches, then a definition that has
        fewer unmatched arguments is chosen. For example, suppose we
        have the following definitions:

        {v
          (defmacro list-length (x) 1)
          (defmacro list-length (x xs) (+ 1 (list-length xs)))
        v}


        The the following term

        {v (list-length 1 2 3) v}

        will be normalized (after a series of transformations) with
        the following:

        {v (+ 1 (+ 1 1)) v}


        {v
          1: (list-length 1 2 3) => (+ 1 (list-length 2 3))
          2: (+ 1 (list-length 2 3)) => (+ 1 (+ 1 (list-length 3)))
          3: (+ 1 (+ 1 (list-length 3))) => (+ 1 (+ 1 1))
        v}

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

        {v
          (defmacro fold (f a x) (f a x))
          (defmacro fold (f a x xs) (fold f (f a x) xs))
        v}


        Using this definition we can define a sum function (although
        it is not needed as the [+] function defined in the Primus Lisp
        standard library, already accepts arbitrary number of
        arguments), as:

        {v (defmacro sum (xs) (fold + 0 xs)) v}

        The {b (sum 1 2 3) } will be rewritten as follows:

        {v
          1: (sum 1 2 3) => (fold + 0 1 2 3)
          2: (fold + 0 1 2 3) => (fold + (+ 0 1) 2 3)
          3: (fold + (+ 0 1) 2 3) => (fold + (+ (+ 0 1) 2) 3)
          4: (fold + (+ (+ 0 1) 2) 3) => (+ (+ (+ 0 1) 2) 3)
        v}

        A more real example is the [write-block] macro, that takes a
        sequence of bytes, and writes them starting from the given
        address:

        {v
          (defmacro write-block (addr bytes)
             (fold memory-write addr bytes))
        v}

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

        {v
          (declare (context (<type-class> <feature> ...) ...))
        v}

        Let's use the following two definitions for a concrete example,

        {v
          (defmacro get-arg-0 ()
             (declare (context (arch arm gnueabi)))
             R0)

          (defmacro get-arg-0 ()
             (declare (context (arch x86 cdecl)))
             (read-word word-width (+ SP (sizeof word-width))))
        v}


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
        advice maybe added to a function that will be called either
        before or after the evaluation of an advised function, e.g.,

        {v
          (defun memory-written (a x)
            (declare (advice :before memory-write))
            (msg "write $x to $a"))
        v}

        This definition not only defines a new function called
        [memory-written], but also proclaims it as advice function to
        the [memory-write] function that should before it is called.

        If an advisor is attached before the advised function, then
        it the advisor will be called with the same arguments as the
        advised function. The return value of the advisor is
        ignored. The advisor function will be called as a normal Lisp
        function, with all expected overloading and name resolving. So
        it is possible to provide context specific advice. If there
        are several advice to the same function, then they will be
        called in the unspecified order.

        An advisor that is attached after the advised function will be
        called with one extra argument - the result of evaluation of
        the advised function. The value returned by the advisor will
        override the result of the advised function. If there are
        several advisors attached after the same function, then they
        will be called in the unspecified order.


        {2 Signaling Mechanims}

        The Primus Observation system is reflected onto the Primus
        Lisp Machine Signals. Every time a reflected observation
        occurs the Lisp Machine receives a signal that is dispatched
        to handlers. A handler can be declared defined with the
        [defmethod] form, e.g.,

        {v
        (defmethod call (name arg)
          (when (= name 'malloc)
            (msg "malloc($0) was called" arg)))
        v}

        The [defmethod] form follows the general definition template,
        i.e., it can contain a docstring and declaration section, and
        selection and resolution rules are applicable to
        methods. Methods of the same signal are invoked in an
        unspecified order.

        {2 Formal syntax}

        Each entity is an s-expression with the grammar, specified
        below.We use BNF-like syntax with the following conventions.
        Metavariables are denoted like [<this>]. The [<this> ...]
        stands of any number of [<this>] (possibly zero). Ordinary
        parentheses do not bear any notation, and should be read
        literally. Note, since the grammar is not context free, and is
        extensible, the following is an approximation of the language
        grammar. Grammar extension points are defined with the
        '?extensible?'comment in a production definition.

        {v
module ::= <entity> ...

entity ::=
  | <feature-request>
  | <declarations>
  | <constant-definition>
  | <substitution-definition>
  | <parameter-definition>
  | <macro-definition>
  | <function-definition>
  | <method-definition>

feature-request ::= (require <ident>)

declarations ::= (declare <attribute> ...)

constant-definition ::=
  | (defconstant <ident> <atom>)
  | (defconstant <ident> <atom> <docstring>)
  | (defconstant <ident> <atom> <declarations>)
  | (defconstant <ident> <atom> <declarations> <docstring>)

parameter-definition ::=
  | (defparameter <ident> <atom>)
  | (defparameter <ident> <atom> <docstring>)
  | (defparameter <ident> <atom> <declarations> <docstring>)

substitution-definition ::=
  | (defsubst <ident> <atom> ...)
  | (defsubst <ident> <declarations> <atom> ...)
  | (defsubst <ident> :<syntax> <atom> ...)
  | (defsubst <ident> <declarations> :<syntax> <atom> ...)

macro-definition ::=
  | (defmacro <ident> (<ident> ...) <exp>)
  | (defmacro <ident> (<ident> ...) <docstring> <exp>)
  | (defmacro <ident> (<ident> ...) <declarations> <exp>)
  | (defmacro <ident> (<ident> ...) <docstring> <declarations> <exp>)

function-definition ::=
  | (defun <ident> (<var> ...) <exp> ...)
  | (defun <ident> (<var> ...) <docstring> <exp> ...)
  | (defun <ident> (<var> ...) <declarations> <exp> ...)
  | (defun <ident> (<var> ...) <docstring> <declarations> <exp> ...)

method-definition ::=
  | (defmethod <ident> (<var> ...) <exp> ...)
  | (defmethod <ident> (<var> ...) <docstring> <exp> ...)
  | (defmethod <ident> (<var> ...) <declarations> <exp> ...)
  | (defmethod <ident> (<var> ...) <docstring> <declarations> <exp> ...)


exp ::=
  | ()
  | <var>
  | <word>
  | <sym>
  | (if <exp> <exp> <exp> ...)
  | (let (<binding> ...) <exp> ...)
  | (set <var> <exp>)
  | (while <exp> <exp> <exp> ...)
  | (prog <exp> ...)
  | (msg <format> <exp> ...)
  | (<ident> <exp> ...)

binding ::= (<var> <exp>)

var ::= <ident> | <ident>:<size>

attribute ::=
  | (external <ident> ...)
  | (context (<ident> <ident> ...) ...)
  | (<ident> ?ident-specific-format?)

docstring ::= <text>

syntax ::= :hex | :ascii | ...

atom  ::= <word> | <text>

word  ::= ?ascii-char? | <int> | <int>:<size>

sym   ::= '<atom>

int   ::= ?decimal-octal-hex-or-bin format?

size  ::= ?decimal?

ident ::= ?any atom that is not recognized as a <word>?
        v}
    *)
    module Lisp : sig

      (** an abstract type representing a lisp program  *)
      type program


      (** an abstract type that represents messages send with the
          [msg] form. *)
      type message


      (** Primus Lisp program loader  *)
      module Load : sig

        (** abstract error type *)
        type error

        (** [program ?paths proj features] loads a program that
            implements a set of [features]. For each feature its
            implementation file, that must have the same basename as
            the name of feature, is looked up in the list of
            directories, specified by the [path] parameter (defaults
            to the current folder). The first implementation that is
            found, will be used, thus the order of the paths matters.

            Returns an abstract representation of a program, that can
            be linked into the Lisp machine, or an error if the program
            is not well-formed.
        *)
        val program : ?paths:string list -> project -> string list ->
          (program,error) result


        (** [pp_error ppf err] outputs error information into the
            pretty-printing formatter [ppf].  *)
        val pp_error : Format.formatter -> error -> unit


        (** [pp_program ppf program] dumps program definitions into the formatter [ppf]   *)
        val pp_program : Format.formatter -> program -> unit
      end

      module Doc : sig

        (** Abstract Element of a document.

            A documentation element is something that can be printed.
            We keep it abstract, as we plan to extend it in the future.
        *)
        module type Element = sig
          type t
          val pp : Format.formatter -> t -> unit
        end

        module Category : Element
        module Name     : Element
        module Descr    : Element



        (** Documentation index.

            Documentation index has the following ogranization:

            {[
              Category 1:
                - Element1 Name, Element1 Description;
              - Element2 Name, Element2 Description;
              ...
                Category2:
                  - ...
            ]}

            All entries are sorted in alphabetic order.

        *)
        type index = (Category.t * (Name.t * Descr.t) list) list

        module Make(Machine : Machine.S) : sig
          val generate_index : index Machine.t
        end
      end


      (** Lisp Type System.

          Primus Lisp is equipped with the gradual type system that
          features type inference.
      *)
      module Type : sig


        (** A type of an expression  *)
        type t


        (** Typing environemnt is a mapping from expressions to types.  *)
        type env

        (** Definition signature  *)
        type signature

        (** An abstract type error *)
        type error

        (** a type specifier for function parameters

            Don't use this directly, this type is uses in the [Spec]
            eDSL. Use the Spec module directly.

        *)
        type parameters = [
          | `All of t
          | `Gen of t list * t
          | `Tuple of t list
        ]


        (** Type Specifier DSL.

            A language to build type signatures for Primus Lisp
            primitives.

            The signature specifier consists of two parts: the
            parameter list specifier, and the return value type
            specifier. They are separated with the [@->] operator:

            {[params @-> return]}

            The list of parameters can be specified as a tuple, a
            variable number of arguments of the same type, or a
            tuple followed by a variable number of arguments of the
            same type. Special shortcuts for 1-tuple and 0-tuple are
            provided.

            The return value type could be [any], [bool], [byte],
            [word n], [sym], [int], or a type variable bound in the
            parameters list.

            Examples:

            {[
              one int @-> byte;
              tuple [int; byte] @-> int;
              all a @-> bool;
              tuple [sym; int] // all byte @-> bool;
            ]}

        *)
        module Spec : sig


          (** [any] top type which is inhabitated by all Primus values  *)
          val any : t


          (** [var x] type variable [x]. All variables with the same
              name in the scope of a definition are unified.  *)
          val var : string -> t

          (** [sym] symbol type.  *)
          val sym : t


          (** a machine integer - a word that has the same width as
              [Arch.addr_size] *)
          val int : t


          (** [bool] a one bit word  *)
          val bool : t

          (** [byte] an eight bit word  *)
          val byte : t

          (** [word n] an [n] bit word *)
          val word : int -> t

          (** [a] shortcut for [var "a"]  *)
          val a : t

          (** [b] shortcut for [var "b"]  *)
          val b : t

          (** [c] shortcut for [var "c"]  *)
          val c : t

          (** [d] shortcut for [var "d"]  *)
          val d : t


          (** [tuple [args]] specifies that a function accepts
              a tuple of arguments of specified types.*)
          val tuple : t list -> [`Tuple of t list]

          (** [all t] specifies that a function accepts a variable
              number of arguments all having type [t].  *)
          val all : t -> [`All of t]

          (** [one t] specifies that a function accepts one argument
              of type [t] *)
          val one : t -> [`Tuple of t list]


          (** [unit] specifies that a function doesn't have any parameters  *)
          val unit : [`Tuple of t list]


          (** [params // rest] specifies that a function is variadic,
              but have some number of mandatory arguments, i.e., it
              accepts a tuple of parameters specified by the [params]
              type specifier and a variadic list of arguments
              specified by the [rest] type specifier. *)
          val (//) : [`Tuple of t list] -> [`All of t] -> parameters


          (** [params @-> t] constructs a signature from the
              parameter list specifier [params] and the return type
              specifier [t]  *)
          val (@->) : [< parameters] -> t -> signature
        end


        (** [error p] occurs when the typechecker detects an error [p].
            @since 2.1.0
        *)
        val error : error observation


        (** [errors env] is a list of type errors.
            [@since 2.1.0] *)
        val errors : env -> error list

        val check : Var.t seq -> program -> error list
        [@@deprecated "[since 2020-02] use [Make(Machine).types] [errors] instead"]


        (** [pp_error ppf err] prints a description of the type error
            [err] into the formatter [ppf] *)
        val pp_error : Format.formatter -> error -> unit
      end


      (** Lisp Machine Message interface.

          Lisp machine messages are sent with the [msg] primitive. The
          messages are abstract, but they could be printed.
      *)
      module Message : sig
        type t = message


        (** [pp ppf msg] prints the message into the specified
            formatter [ppf]. *)
        val pp : Format.formatter -> t -> unit
      end

      (** Machine independent closure.

          A closure is an anonymous function, that performs some
          computation in the Machine Monad. Closures are used to
          extend the Lisp Machine with arbitrary primitive operations
          implemented in OCaml. *)
      module type Closure = functor (Machine : Machine.S) -> sig

        (** [run args] performs the computation.  *)
        val run : value list -> value Machine.t
      end


      (** [(lisp-primitive <name> <arg1> ... <argM> <rval>)] is posted
          when the Lisp primitive with the given <name> is called with
          the list of arguments [(<arg1> .... <argM>)] and evaluates
          to the [<rval>] value.

          @since 2.1.0
      *)
      val primitive : (string * value list) observation

      (** a closure packed as an OCaml value *)
      type closure = (module Closure)

      (* dedocumented due to deprecation *)
      module Primitive : sig
        type 'a t
        val create : ?docs:string -> string -> (value list -> 'a) -> 'a t
      end [@@deprecated "[since 2018-03] use [Closure]"]

      (* undocumented since it is deprecated *)
      module type Primitives = functor (Machine : Machine.S) ->  sig
        val defs : unit -> value Machine.t Primitive.t list [@@warning "-D"]
      end [@@deprecated "[since 2018-03] use [Closure]"]

      (** a list of primitives.  *)
      type primitives = (module Primitives)
      [@@deprecated "[since 2018-03] use [closure]"]

      type exn += Runtime_error of string

      (** [message] observation occurs every time a message is sent
          from the Primus Machine.  *)
      val message : message observation

      (** Make(Machine) creates a Lisp machine embedded into the
          Primus [Machine].  *)
      module Make (Machine : Machine.S) : sig

        (** [link_program p] links the program [p] into the Lisp
            Machine. Previous program, if any, is discarded. *)
        val link_program : program -> unit Machine.t

        (** [program] is the current Machine program.  *)
        val program : program Machine.t

        (** [types] returns Primus Lisp typing environment.  *)
        val types : Type.env Machine.t

        (** [define ?docs name code] defines a lisp primitive with
            the given [name] and an optional documentation string
            [doscs].

            Example:

            {[
              open Bap_primus.Std

              type Primus.exn += Bad_abs_call

              module Abs(Machine : Primus.Machine.S) = struct
                let run = function
                  | [x] -> Value.abs x
                  | _ -> Machine.raise Bad_abs_call
              end

              ...

              module Library(Machine : Primus.Machine.S) = struct
                module Lisp = Primus.Lisp.Make(Machine)
                let init () = Machine.sequence [
                    Lisp.define "abs" (module Abs);
                    ...;
                  ]
              end
            ]}
        *)
        val define : ?types:Type.signature ->
          ?docs:string -> string -> closure -> unit Machine.t


        (** [signal ?params ?docs obs proj] defines a new signal.

            Primus Observations are reflected onto Primus Lisp
            signals. Each reflection is defined via the [signal]
            operator that establishes a mapping between an observation
            and a signal.

            After the signal is defined, every time the observation
            [obs] is made, the signal [(signal args)] will be sent,
            where [signal = Observation.name obs] and [args] is a
            mapping from the observation value to a list of values.

            The signal will match with the observation name. Though
            the same observation may produce signals with different
            arities.

            @param params optional type specification

            @param doc optional documentation string
        *)
        val signal :
          ?params:[< Type.parameters] ->
          ?doc:string ->
          'a observation ->
          ('a -> value list Machine.t) -> unit Machine.t

        (** [failf msg a1 ... am ()] terminates a lisp machine, and
            correspondingly the Primus machine with the
            [Runtime_error].  *)
        val failf : ('a, unit, string, unit -> 'b Machine.t) format4 -> 'a

        (** [eval_fun name args] calls a lisp function with the given
            [name], that is the most specific to the current context
            and is applicable to the specified list of arguments.

            @since 1.5 *)
        val eval_fun : string -> value list -> value Machine.t

        (** [eval_method name args] invokes all methods with the given
            [name] that are applicable in the current context to the
            specified list of arguments.

            @since 1.5 *)
        val eval_method : string -> value list -> unit Machine.t

        (** [link_primitives prims] provides the primitives [prims]   *)
        val link_primitives : primitives -> unit Machine.t
        [@@deprecated "[since 2018-03] use link_primitive instead"]
      end

      (* it's a no-op now. *)
      val init : ?log:Format.formatter -> ?paths:string list -> string list -> unit
      [@@deprecated "[since 2018-03] use the Machine interface instead"]
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
