open Core_kernel.Std
open Regular.Std
open Bap.Std
open Monads.Std
open Bap_future.Std

module Std : sig

  (** Primus - The Microexecution Framework.


      Primus is a microexecution framework that can be used to build
      CPU and full system emulators, symbolic executers, static
      fuzzers, policy checkers, tracers, quickcheck-like test suits,
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



      (** Primus Entry Point.  *)
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
        Environemnt and the Memory.

        Note: we the [observation (x,y,z)] notation in the
        documentation to denote an observation of a value represented
        with the [(x,y,z)] tuple, that in fact corresponds to
        [observation >>> fun (x,y,z)] -> ... *)
    module Interpreter : sig


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

      (** [written (v,x)] happens after [x] is assinged to [v]  *)
      val written : (var * value) observation

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
          given [Machine].  *)
      module Make (Machine : Machine.S) : sig
        type 'a m = 'a Machine.t


        (** [halt] halts the machine by raise the [Halt] exception.  *)
        val halt : never_returns m


        (** [pc] current value of a program counter.*)
        val pc : addr m

        (** [pos m] current program position.  *)
        val pos : pos m

        (** [sub x] computes the subroutine [x].  *)
        val sub : sub term -> unit m

        (** [blk x] interprets the block [x].  *)
        val blk : blk term -> unit m

        (** [get var] reads [var]  *)
        val get : var -> value m

        (** [set var x] sets [var] to [x]  *)
        val set : var -> value -> unit m

        (** [binop x y] computes a binary operation [op] on [x] and [y]  *)
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
            of size [s] using an order specified by the endianness [d] from
            address [a]. *)
        val load : value -> endian -> size -> value m

        (** [store a x d s] computes a store operation, that stores at
            the address [a] the word [x] of size [s], using an
            ordering specified by the endianness [d]. *)
        val store : value -> value -> endian -> size -> unit m
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



      (** The Linker error  *)
      type exn += Unbound_name of name


      (** occurs before a piece of code is executed *)
      val exec : name observation

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

      (** A variable is undefined, if it was never [add]ed to the
          environment.  *)
      type exn += Undefined_var of var


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


        (** [all] is a sequence of all variables defined in the
            environment. Note, the word _defined_ doesn't mean
            initialized.   *)
        val all : var seq Machine.t
      end
    end



    (** Virtual memory.

        The virtual memory is a byte addressable machine memory.*)
    module Memory : sig


      (** [Make(Machine)] lifts the memory interface into the
          [Machine] monad.  *)
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

        Primus Lips is a low-level language that doesn't provide many
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
        for ARMv7, {b t = 32 \/ 31 \/ .. \/ 1 }. Thus a value of any
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

        A sequence {b $<expr> } will be substituted with the value of
        expression {b $<expr> }. Thus the [$] symbol can be seen as
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
          (defun memory-written (a x) (msg "write $x to $a"))
          (advice-add memory-written :before memory-write)
        v}

        The general syntax is:

        {v (advice-add <advisor> <when> <advised>) v}

        where the <when> clause is either [:before] or [:after].

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

        The following example will demostrate how to implement fat
        pointers using the advice mechanism:

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
  | <macro-definition>
  | <function-definition>
  | <advising>

feature-request ::= (require <ident>)

declarations ::= (declare <attribute> ...)

constant-definition ::=
  | (defconstant <ident> <atom>)
  | (defconstant <ident> <docstring> <atom>)
  | (defconstant <ident> <declarations> <atom>)
  | (defconstant <ident> <docstring> <declarations> <atom>)

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

advice ::=
  | (advice-add <ident> <method> <ident>)

exp ::=
  | ()
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

syntax ::= :hex | :ascii | ?extensible?

atom  ::= <word> | <text> |

word  ::= ?ascii-char? | <int> | <int>:<size>

int   ::= ?decimal-octal-hex-or-bin format?

size  ::= ?decimal?

ident ::= ?any atom that is not recognized as a <word>?
        v}
    *)
    module Lisp : sig

      (** an abstract type representing a lisp program  *)
      type program


      (** Primus Lisp program loader  *)
      module Load : sig
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

      module Type : sig
        type t
        type signature
        type error

        type parameters = [
          | `All of t
          | `Gen of t list * t
          | `Tuple of t list
        ]

        module Spec : sig
          val any : t
          val var : string -> t
          val sym : t
          val int : t
          val bool : t
          val byte : t
          val word : int -> t
          val a : t
          val b : t
          val c : t
          val d : t

          val tuple : t list -> [`Tuple of t list]
          val all : t -> [`All of t]
          val one : t -> [`Tuple of t list]
          val unit : [`Tuple of t list]
          val (//) : [`Tuple of t list] -> [`All of t] -> parameters
          val (@->) : [< parameters] -> t -> signature
        end

        val check : Var.t seq -> program -> error list
        val pp_error : Format.formatter -> error -> unit
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

      (** a closure packed as an OCaml value *)
      type closure = (module Closure)

      (* undocumented since it is deprecated *)
      module Primitive : sig
        type 'a t
        val create : ?docs:string -> string -> (value list -> 'a) -> 'a t
      end

      (* undocumented since it is deprecated *)
      module type Primitives = functor (Machine : Machine.S) ->  sig
        val defs : unit -> value Machine.t Primitive.t list
      end

      (** a list of priomitives.  *)
      type primitives = (module Primitives)

      type exn += Runtime_error of string


      (** [message] occurs every time the Lisp Machine produces a
          message using the [msg] primitive.   *)
      val message : string observation


      (** Make(Machine) creates a Lisp machine embedded into the
          Primus [Machine].  *)
      module Make (Machine : Machine.S) : sig


        (** [link_program p] links the program [p] into the Lisp
            Machine. Previous program, if any, is discarded. *)
        val link_program : program -> unit Machine.t


        (** [progra] is the current Machine program.  *)
        val program : program Machine.t

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


        (** [failf msg a1 ... am ()] terminates a lisp machine, and
            correspondingly the Primus machine with the
            [Runtime_error].  *)
        val failf : ('a, unit, string, unit -> 'b Machine.t) format4 -> 'a


        (** [link_primitives prims] provides the primitives [prims]   *)
        val link_primitives : primitives -> unit Machine.t
        [@@deprecated "[since 2017-12] use link_primitive instead"]
      end

      (* it's a no-op now. *)
      val init : ?log:Format.formatter -> ?paths:string list -> string list -> unit
      [@@deprecated "[since 2017-12] use the Machine interface instead"]
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
