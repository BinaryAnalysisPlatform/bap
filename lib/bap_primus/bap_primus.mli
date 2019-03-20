open Core_kernel
open Monads.Std
open Bap_knowledge

(** Primus - A non-deterministic interpreter.


*)

module Primus : sig
  open Knowledge
  type 'a machine

  (** The Machine Exception.

      The exn type is an extensible variant, and components
      usually register their own error constructors. *)
  type exn = ..

  (** [an observation] of a value of type [a].*)
  type 'a observation

  (** Machine exit status.
        A machine may terminate normally, or abnormally with the
        specified exception. *)
  type exit_status =
    | Normal
    | Exn of exn



  (** the machine computation  *)
  type 'a t = 'a machine

  type 'a state

  type project


  (** Machine identifier type.   *)
  type id = Monad.State.Multi.id

  (** [raise exn] raises the machine exception [exn], intiating
        an abonormal control flow *)
  val raise : exn -> 'a t


  (** [catch x f] creates a computation that is equal to [x] if
        it terminates normally, and to [f e] if [x] terminates
        abnormally with the exception [e]. *)
  val catch : 'a t -> (exn -> 'a t) -> 'a t

  val collect : ('a,'p) slot -> 'a obj -> 'p t
  val provide : ('a,'p) slot -> 'a obj -> 'p -> unit t
  val project : project obj t

  val die : id -> unit t

  val conflict : conflict -> 'a t


  (** [decide fact] make the fact determined in the current machine.

      This is the [pure] function w.r.t. to the non-determinism, also
      known as lift, since it lifts the inner knowledge monad into the
      outer machine monad.
  *)
  val decide : 'a knowledge -> 'a t


  (** [run comp project] runs the Primus system. *)
  val run : unit t -> project obj -> unit knowledge



  (** Computation State *)
  module State : sig
    (** ['a t] is a type of state that holds a value of type
            ['a], and can be constructed from the base context of type
            ['c]. *)
    type 'a t = 'a state
    type 'a state = 'a t



    (** [declare ~inspect ~uuid ~name make] declares a state with
            the given [uuid] and [name]. The name is not required to be
            unique, while [uuid] is obviously required to be unique.

            See [uuid] type description for the uuid representation. A
            new [uuid] can be obtained in the Linux system is provided
            by the [uuidgen] command.*)
    val declare :
      ?inspect:('a -> Info.t) ->
      ?name:string ->
      (project obj -> 'a Knowledge.t) -> 'a t

    (** [inspect state value] introspects given [value] of the state.  *)
    val inspect : 'a t -> 'a -> Info.t

    (** [name state] a state name that was given during the construction.  *)
    val name : 'a t -> string
  end



  (** An interface to the state.

          An interface gives an access to operations that query and
          modify machine state. *)
  module type State = sig
    type 'a t

    (** [get state] extracts the state.  *)
    val get : 'a t -> 'a machine

    (** [put state x] saves a machine state  *)
    val put : 'a t -> 'a -> unit machine

    (** [update state ~f] updates a state using function [f]. *)
    val update : 'a t -> f:('a -> 'a) -> unit machine
  end

  (** Observations interface.

      An observation is a named event, that can occur during the
      program execution. Observations could be provided (usually
      by components that are implementing a paricular primitive),
      and observed (i.e., a component could be notified every time
      an observation is made). In other word, the Observation module
      provides a publish/subscribe service.

      The observation system uses the continutation passing style to
      enable polymorphic event system which doesn't rely on boxed
      types, such as tuples and records, to deliver observation
      (events) to subscribers.

      Each observation is parametrized by a type of a function which
      is used to provide the observation. For a concrete example,
      let's take the [sum] observation, which occurs every time
      a sum of two values is computed. This observation will have
      three arguments (for simplicity let's assume that they have type
      [int]) and (this is true for all observation types) will have
      the return type [ok machine], so the final type of the [sum] is
      [int -> int -> int -> ok machine].

      {3 Providing Observations}

      Observations are provided using the [Observation.provide]
      function, which takes a function, that will be called with one
      parameter, which is a function on itself and has type ['f]. We
      call this function [observe], since it is actually the observer
      which is being notified. Here is an example, using our [sum]
      observation:

      {[
        Observation.make sum ~f:(fun observe ->
            observe 1 2 3)
      ]}


      {3 Monitoring Observations}

      It is possible to register a function, which will be called
      every time an observation is made via the [provide] function.
      The monitor has a little bit more complicated type, as beyond
      the actual payload (arguments of the observation), it takes a
      [ctrl] instance, which should be used to return from the
      observation, via [Observation.continue] or [Observation.stop]
      functions.

  *)
  module Observation : sig
    type 'f t = 'f observation
    type info = Info.t
    type ctrl
    type void

    val declare :
      ?inspect:((info -> unit machine) -> 'f) ->
      ?package:string -> name:string ->
      'f observation


    (** [provide obs f] provides the observation of [obs].

        The function [f] takes one argument a function,
        which accepts


    *)
    val provide : 'f observation -> f:('f -> void machine) -> unit machine
    val monitor : 'f observation -> f:(ctrl -> 'f) -> unit machine
    val inspect : 'f observation -> f:(info -> unit machine) -> unit machine

    val continue : ctrl -> unit machine
    val stop : ctrl -> unit machine
  end

  (** [exn_raised exn] occurs every time an abnormal control flow
        is initiated *)
  val exn_raised : exn observation


  (** Computation Syntax.*)
  module Syntax : sig
    include Monad.Syntax.S with type 'a t := 'a t

    (** [x-->p] is [collect p x] *)
    val (-->) : 'a obj -> ('a,'p) slot -> 'p t


    (** [p <-- f] is [promise p f]  *)
    val (<--) : ('a,'p) slot -> ('a obj -> 'p t) -> unit


    (** [event >>> action] is the same as
              [Observation.observe event action] *)
    val (>>>) : 'a observation -> ('a -> unit t) -> unit t
  end



  include Monad.State.Multi.S with type 'a t := 'a t
                               and type id := id
                               and module Syntax := Syntax

  (** Local state of the machine.  *)
  module Local  : State with type 'a t := 'a state


  (** Global state shared across all machine clones.  *)
  module Global : State with type 'a t := 'a state
end
