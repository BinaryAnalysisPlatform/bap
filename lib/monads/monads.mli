open Core_kernel

(** Monad Transformer Library.

    {2:abstract Abstract}

    A monad is an abstraction that can be used to parametrize code
    with the way how computations are sequenced. A monad can also be
    seen as design pattern, that sometimes makes your code more
    readable (and, more often, less readable).  A monad transformer of
    a monad [M] is a functor that takes another monad [M'] and
    enriches [M] with the behavior of [M']. In other terms a
    transformer is a monad composition operator.  This library
    provides the monad abstraction and implementations of commonly
    known monads. Each implementation provides the transformer as
    well.

    {2 Table of Contents}
    - {{!abstract}Abstract}
    - {{!intro}Introduction}
    - {{!conv}Conventions}
    - {{!Std.Monoid}Monoid} a set with an associative operation
    - {{!Std.Monad}Monad} a basic monad interface
    {ul
      {- Monad Interfaces
        {ul
          {- {{!Std.Monad.S}Monad.S} the unary monad}
          {- {{!Std.Monad.S2}Monad.S2} the binary monad}
          {- {{!Std.Monad.Minimal}Monad.Minimal} the minimal interface}
          {- {{!Std.Monad.Minimal}Monad.Minimal2} the minimal interface}
          {- {{!Std.Monad.Core}Monad.Core} the core interface}
          {- {{!Std.Monad.Core}Monad.Core2} the core interface}
          {- {{!Std.Monad.Plus}Monad.Plus} a monad over a monoid interface}
          {- {{!Std.Monad.Fail}Monad.Fail} a failure monad interface}
          {- {{!Std.Monad.Choice}Monad.Choice} a choice monad interface}
          {- {{!Std.Monad.Trans}Monad.Trans} a monad transformer interface}
        }
      }
      {- {{!Std.Monad.Collection}Monad.Collection} a container of monads}
      {- {{!Std.Monad.Syntax}Monad.Syntax} monad operators}
      {- Monad Constructors
        {ul
          {- {{!Std.Monad.Make}Make}}
          {- {{!Std.Monad.Make2}Make2}}
          {- {{!Std.Monad.Core}Core}}
          {- {{!Std.Monad.Core2}Core2}}
          {- {{!Std.Monad.Minimal}Minimal}}
          {- {{!Std.Monad.Minimal2}Minimal2}}
        }
      }
      {- {{!Std.Monad.Ident}Monad.Ident} the do-nothing monad}
      {- {{!Std.Monad.Option}Monad.Option} a non-total monad}
      {- {{!Std.Monad.Result}Monad.Result} a non-total monad}
      {- {{!Std.Monad.Result.Error}Monad.Result.Error} a non-total monad}
      {- {{!Std.Monad.Result.Exception}Monad.Result.Exception} a non-total monad}
      {- {{!Std.Monad.List}Monad.List} a non-deterministic monad}
      {- {{!Std.Monad.Seq}Monad.Seq} a non-deterministic monad}
      {- {{!Std.Monad.Writer}Monad.Writer} a computation with a writable state}
      {- {{!Std.Monad.Reader}Monad.Reader} a computation with a readable state}
      {- {{!Std.Monad.State}Monad.State} a computation with a state}
      {- {{!Std.Monad.State}Monad.State.Multi} a computation with a non-deterministic state}
      {- {{!Std.Monad.Cont}Monad.Cont} a call/cc monad}
    }

    {2:intro Introduction}

    In this section we will provide a small introduction into the
    Monad Concept. If you feel yourself comfortable with the idea of
    Monad you may skip to the next section.

    Monad by itself is a concept or an abstraction. Abstractions come
    into play when there is a need to write a generic implementation
    of some algorithm. There are different mechanisms for
    parametrization an algorithm, but all of them require defining
    some kind of abstraction, that are usually denoted in a
    programming language with a type or a module type. In simple
    cases, an algorithm is parametrized by scalar, e.g.,

    {v
      algorithm a b ::=
         x := f a;
         y := f b;
         return (x + y);
    v}

    In a more general case, we may parametrize an algorithm with
    transformations, i.e., with functions:

    {v
      algorithm ((_ + _), (f _)) a b ::=
         x := f a;
         y := f b;
         return (x + y);
    v}

    However, we have one more generalization opportunity. The
    semicolon is a sequencing operator that has semantics that is
    usually defined by a programming language, and, typically, in
    regular deterministic languages [x := f a; y := f b] means: first
    compute [f a] and assign the result to [x], then compute [f b] and
    assign the result to [y]. However, what if we would like to
    parametrize our algorithm with a behavior of the semicolon
    and operators:

    {v
      algorithm ((return _), (_ := _ ; _)) ((_ + _), (f _)) a b ::=
         x := f a;
         y := f b;
         return (x + y);
    v}

    A pair of operators [(return _)] and [(_ := _ ; _)] form the monad
    signature. Since a host language no longer defines semantics of
    the assignment and semicolon, the monad actually operates with
    computations rather than with values. I.e., it is the monad
    implementation that now defines how computations produce values,
    the order of evaluation, etc. The [return x] lifts a value into
    the computation, i.e., it constructs a trivial computation from a
    constant.  The [v := y; z] operator, also called [bind], gives the
    general semantics of a program, i.e., how the result of computation
    [y] is propagated to the computation [z] (if propagated), it also
    defines the semantics of the semicolon, i.e., whether [z] is
    performed after [y], etc. In general, the semantics may be
    arbitrary, but let's show few examples.

    1. Partiality: a computation may diverge into a bottom value,
       i.e., if [y] diverges, then [z] is not called and the bottom
       value becomes the result of the whole
       computation. {{!Std.Monad.Option}Monad.Option} and
       {{!Std.Monad.Result}Monad.Result} provide a notion of partial
       computation with different representations of the bottom value.

    2. Nondeterminism: a computation may produce more than one value,
       in that case [v] will be bound several times, and [z] will be
       called for each possible value of
       [v]. {{!Std.Monad.List}Monad.List} and
       {{!Std.Monad.Seq}Monad.Seq} provide implementations of the
       nondeterministic monad with different representations of a
       sequence of values.

    3. Side-effects: [x] may produce an effect that changes the
       computation environment. We can subdivide effectful computation
       into more precise categories:
         - effect only -- computations do depend on the effects produced
           by other computations, see {{!Std.Monad.Writer}Monad.Writer};
         - coeffect only -- computations can't produce effects, though
           they depend on the computation environment,
           see {{!Std.Monad.Reader}Monad.Reader};
         - full effect -- computations may change the environment and
           may depend on effects produced by other computations, see
           see {{!Std.Monad.State}Monad.State}.
       The effect itself may also be non-deterministic, e.g., [z] is
       computed for each possible effect produced by [y], see
       {{!Std.Monad.State.Multi}Monad.State.Multi}

    4. Continuation: [x] defines a continuation of [z], i.e., akin to
       the effect notion, in which a program state is passed from one
       computation to another, the continuation notion reifes the
       control flow of a computation and passes it to the consequent
       continuation as a state. See {{!Std.Monad.Cont}Monad.Cont}.

    {3 The OCaml representation of the monad signature}

    The monad signature [(return _), (_ := _ ; _)] is represented with
    the following OCaml signature:

    {[
      module type Monad = sig
        type 'a t
        val return : 'a -> 'a t
        val bind : 'a t -> ('a -> 'b t) -> 'b t
      end
    ]}

    We also use [>>=] operator as an alias to the [bind]
    function. Thus [v := x; z] is represented in OCaml as
    [x >>= fun v -> z]. We use functors to parametrize algorithms with
    signatures, e.g.,

    {[

      module Algorithm(M : Monad) = struct
        open R open M

        let run a b =
          f a >>= fun x ->
          f b >>= fun y ->
          return (x + y)
      end
    ]}

    Basically, whenever you see [c1 >>= fun v -> c2] you should
    understand it as [v := c1; c2] with a parametrized semicolon, and
    when you see [c1 >>= fun () -> c2] you should understand it as [c1;
    c2].  Once you will develop a habit of using monadic semicolon it
    will become much easier to you to understand the monadic
    code. Alternatively, you may try one of the syntax preprocessors
    that will introduce the so called do-notation, with the actual
    semicolon being overloaded.

    {2:conv Conventions}

    To use the library add [open Monads.Std] to your program. It will
    bring [Monoid] and [Monad] modules to your scope. A conventional
    way of writing a computation in a monad [M], is to open its syntax
    with [open M.Syntax].

    Given that monad is a concept that goes beyond OCaml language,
    i.e., it is more a design pattern rather than just a module type
    signature, we follow few conventions to make it easier to work
    with different monads.

    First of all we have two monad signatures, [S] and [S2]. The [S]
    signature defines monad interface for an unary type constructor ['a
    t] and [S2] defines the monad interface for a type parametrized
    with two type parameters, i.e., [('a,'b) t]. Correspondingly,
    functors named [Make] generate modules of type [S] and modules
    named [Make2] produce modules of type [S2].

    Every monad [M] provides two transformers [M.Make] and [M.Make2]
    that transforms [M] into another monad. The [M] itself provides an
    implementation of [M.S] or [M.S2] (depending on a particular kind
    of monad).

    If a monad type is parametrized by two parameters, then the first
    parameter holds the type of a value, and the second type holds the
    type of an extra information (usually the type of the context).

    Each monad transformer creates a module that has functions [lift]
    and [run]. The [lift] function lifts original computation into the
    transformed one, and [run] will run the computation.
*)
module Std : sig

  (** A monoid set.

      A monoid is a set closed under an associative binary operation
      [plus] and a [zero] element that is neutral to the [plus]
      operation.

      Good examples are numeric types with addition and zero, or
      multiplication and one, see
      {{!Std.Monoid.Int.Sum}Monoid.Int.Sum},
      {{!Std.Monoid.Int.Product}Monoid.Int.Product},
      {{!Std.Monoid.Float.Sum}Monoid.Float.Sum},
      {{!Std.Monoid.Float.Product}Monoid.Float.Product}.

      Containers are also usually form a monoid with an empty
      container being a neutral element, and the  being
      the closure, e.g., {{!Std.Monoid.List}Monoid.List},
      {{!Std.Monoid.Stack}Monoid.Stack},
      {{!Std.Monoid.String}Monoid.String}, {{!Std.Monoid.Set}Monoid.Set}.

  *)
  module Monoid : sig

    (** The minimal monoid interface  *)
    module type Base = sig
      type t

      (** [zero] an element that is neutral to [plus]  *)
      val zero : t

      (** [plus x y] an associative operation.  *)
      val plus : t -> t -> t
    end

    (** The monoid interface *)
    module type S = sig
      include Base

      (** [concat xs] reduces [xs] to using [plus] *)
      val concat : t list -> t

      (** [x @@ y] is [plus x y]  *)
      val (@@) : t -> t -> t
    end

    (** Make(Base) derives a monoid from its minimal definition  *)
    module Make(M : Base) : S with type t := M.t

    (** a trivial monoid  *)
    module Unit : S with type t = unit

    (** Concretizes ['a list] to [T.t list]  *)
    module TList(T : T) : sig type t = T.t list end

    (** A {{!Std.Monoid.List}list monoid} that accumulates data in a
        reversed order. *)
    module Stack : sig

      (** Make(T) constructs a monoid that accumulates elements of
          type [T.t] *)
      module Make (T : T) : S with type t := TList(T).t
    end

    (** A monoid that accumulates data in a list.  *)
    module List : sig

      (** Make(T) constructs a monoid that accumulates elements of
          type [T.t] *)
      module Make (T : T) : S with type t := TList(T).t
    end

    (** A monoid on strings. *)
    module String : S with type t = string

    (** A set monoind.  *)
    module Set : sig

      (** Derives a set monoind from the set [S].  *)
      module Make(S : Set.S) : S with type t := S.t
    end

    (** Provides monoids in the Z domain.

        By default an addition monoid is provided.  *)
    module Int : sig

      include S with type t = int

      (** A monoind over addition  *)
      module Sum : S with type t = int

      (** A monoid over product *)
      module Product : S with type t = int
    end

    (** Provides monoids in the R domain.

        By default an addition monoid is provided.*)
    module Float : sig
      include S with type t = float

      (** A monoind over addition  *)
      module Sum : S with type t = float

      (** A monoid over product *)
      module Product : S with type t = float
    end
  end

  (** The Monad module.   *)
  module Monad : sig

    (** A parametric {{!Std.Monoid}monoid} *)
    module Plus : sig

      (** a monoid over an unary polymorphic type.  *)
      module type S = sig
        type 'a t

        (** [zero ()] constructs a zero element  *)
        val zero : unit -> 'a t

        (** [plus x y] an associative operation.  *)
        val plus : 'a t -> 'a t -> 'a t
      end

      (** a monoid over a binary polymorphic type.  *)
      module type S2 = sig
        type ('a,'e) t

        (** [zero ()] constructs a zero element  *)
        val zero : unit -> ('a,'e) t

        (** [plus x y] an associative operation.  *)
        val plus : ('a,'e) t -> ('a,'e) t -> ('a,'e) t
      end
    end

    (** A fail monad interface.

        Fail monads are used to represent partial computation. There
        are several Fail monads with different flavors and kinds. This
        module provides a base abstraction for them, namely the [fail]
        and [catch] functions.

        Implemented by:
        - {{!Std.Monad.Result.Make}Monad.Result.Make}
        - {{!Std.Monad.Result.Make2}Monad.Result.Make2}
        - {{!Std.Monad.Result.Error}Monad.Result.Error}
        - {{!Std.Monad.Result.Error.Make}Monad.Result.Error.Make}
        - {{!Std.Monad.Result.Exception}Monad.Result.Exception}
        - {{!Std.Monad.Result.Exception.Make}Monad.Result.Exception.Make}
    *)
    module Fail : sig

      (** The unary fail monad interface.

          Implemented by:
            - {{!Std.Monad.Result.Make}Monad.Result.Make}
            - {{!Std.Monad.Result.Error}Monad.Result.Error}
            - {{!Std.Monad.Result.Error.Make}Monad.Result.Error.Make}
            - {{!Std.Monad.Result.Exception}Monad.Result.Exception}
            - {{!Std.Monad.Result.Exception.Make}Monad.Result.Exception.Make}
      *)
      module type S = sig

        (** a type of error  *)
        type 'a error

        (** a monad type  *)
        type 'a t

        (** [fail err] diverges the computation, possibly providing an
            extra information in a value of type [_ error].  *)
        val fail : _ error -> 'a t

        (** [catch m f] if [m] diverges with some bottom value [err],
            the [f err] is a result of the whole computation, otherwise
            returns [m].  *)
        val catch : 'a t -> (_ error -> 'a t) -> 'a t
      end

      (** The binary fail monad interface.

          Implemented by:
          - {{!Std.Monad.Result.Make2}Monad.Result.Make2}
      *)
      module type S2 = sig

        (** a type of error  *)
        type 'a error

        (** a monad type, where ['a] is a value type, and ['e] is a type
            parameter of the [error] type. *)
        type ('a,'e) t

        (** [fail err] diverges the computation, possibly providing an
            extra information in a value of type ['e error].  *)
        val fail : 'e error -> ('a,'e) t

        (** [catch m f] if [m] diverges with some bottom value [err],
            the [f err] is a result of the whole computation, otherwise
            returns [m].  *)
        val catch : ('a,'e) t -> ('e error -> ('a,'e) t) -> ('a,'e) t
      end
    end

    (** A choice monad interface.

        A choice monad is a monad with an exceptional control flow,
        akin to the [Fail] monad, except that it may not hold an
        additional error information. In exchange several control flow
        operators are provided. Basically, a choice computation may
        have zero or one result.

        Example:

        {[
          let sqrt x =
            guard (x > 0.) >>| fun () ->
            sqrt x
        ]}

        Implemented by:

        - {{!Std.Monad.Option}Monad.Option}
        - {{!Std.Monad.Option.Make}Monad.Option.Make}
        - {{!Std.Monad.Option.Make2}Monad.Option.Make2}
        - {{!Std.Monad.List}Monad.List}
        - {{!Std.Monad.List.Make}Monad.List.Make}
        - {{!Std.Monad.List.Make2}Monad.List.Make2}
        - {{!Std.Monad.Seq}Monad.Seq}
        - {{!Std.Monad.Seq.Make}Monad.Seq.Make}
        - {{!Std.Monad.Seq.Make2}Monad.Seq.Make2}
    *)
    module Choice : sig

      (** The minimal choice interface for unary monad.  *)
      module type Basic = sig
        type 'a t

        (** [pure x] creates a computation that results in [x].  *)
        val pure : 'a -> 'a t

        (** [zero ()] creates a computation that has no result.  *)
        val zero : unit -> 'a t
      end

      (** The unary choice monad interface  *)
      module type S = sig
        type 'a t
        include Basic with type 'a t := 'a t

        (** [accept x] accepts [x] as a result of computation. (Same
            as [pure x].  *)
        val accept : 'a -> 'a t

        (** [reject ()] rejects the rest of computation sequence, and
            terminate the computation with the [zero] result (Same as
            [zero ()] *)
        val reject : unit -> 'a t

        (** [guard cond] ensures [cond] is [true] in the rest of
            computation. Otherwise the rest of the computation is
            rejected.  *)
        val guard : bool -> unit t

        (** [on cond x] computes [x] only iff [cond] is [true]   *)
        val on : bool -> unit t -> unit t

        (** [unless cond x] computes [x] unless [cond] is [true].  *)
        val unless : bool -> unit t -> unit t
      end

      (** The minimal choice interface for binary monad.  *)
      module type Basic2 = sig
        type ('a,'e) t

        (** [pure x] creates a computation that results in [x].  *)
        val pure : 'a -> ('a,'e) t

        (** [zero ()] creates a computation that has no result.  *)
        val zero : unit -> ('a,'e) t
      end

      (** The binary choice monad interface  *)
      module type S2 = sig
        type ('a,'e) t
        include Basic2 with type ('a,'e) t := ('a,'e) t

        (** [accept x] accepts [x] as a result of computation. (Same
            as [pure x].  *)
        val accept : 'a -> ('a,'e) t

        (** [reject ()] rejects the rest of computation sequence, and
            terminate the computation with the [zero] result (Same as
            [zero ()] *)
        val reject : unit -> ('a,'e) t

        (** [guard cond] ensures [cond] is [true] in the rest of
            computation. Otherwise the rest of the computation is
            rejected.  *)
        val guard : bool -> (unit,'e) t

        (** [on cond x] computes [x] only iff [cond] is [true]   *)
        val on : bool -> (unit,'e) t -> (unit,'e) t

        (** [unless cond x] computes [x] unless [cond] is [true].  *)
        val unless : bool -> (unit,'e) t -> (unit,'e) t
      end

      module Make(M : Basic) : S with type 'a t := 'a M.t
      module Make2(M : Basic2) : S2 with type ('a,'e) t := ('a,'e) M.t
    end

    (** Monad Transformer Interface.

        A monad transformer is a monad composition operator. There is
        not notion of a generic transformer, that takes two arbitrary
        monads and compose them, but every single monad should provide
        its own transformer. This module provides three interfaces of the
        transfomers:

        - {{!Std.Monad.Trans.S}Monad.Trans.S} composes two unary monad;
        - {{!Std.Monad.Trans.S1}Monad.Trans.S} composes unary and binary monads;
        - {{!Std.Monad.Trans.S2}Monad.Trans.S} composes two binary monad;

        A composition of two monads denoted as [module M =
        Outer.Make(Inner)] can is a new monad [M] that has properties
        of both [Outer] and [Inner] (you visualize this composition as
        a russian doll). All transfomers provide a [lift] function
        that lifts computations in the inner monad [_ m] to the
        resultint monad [_ t]. The [run] function will actually run
        the computation and result in a value of type [_ e] that each
        monad transfomer defines differently.
    *)
    module Trans : sig

      (** Unary monad transformer.  *)
      module type S = sig
        type 'a t
        type 'a m
        type 'a e

        (** lifts inner monad into the resulting monad  *)
        val lift : 'a m -> 'a t

        (** runs the computation  *)
        val run : 'a t -> 'a e
      end

      (** Unary to binary monad transfomer.  *)
      module type S1 = sig
        type ('a,'e) t
        type 'a m
        type ('a,'e) e

        (** lifts inner monad into the resulting monad  *)
        val lift : 'a m -> ('a,'e) t

        (** runs the computation  *)
        val run : ('a,'e) t -> ('a,'e) e
      end

      (** Binary to binary monad transfomer  *)
      module type S2 = sig
        type ('a,'e) t
        type ('a,'e) m
        type ('a,'e) e

        (** lifts inner monad into the resulting monad  *)
        val lift : ('a,'e) m -> ('a,'e) t

        (** runs the computation  *)
        val run : ('a,'e) t -> ('a,'e) e
      end
    end

    (** Basic monad interface.  *)
    module type Basic = sig
      type 'a t

      (** [bind m f] passes the result of computation [m] to function [f] *)
      val bind : 'a t -> ('a -> 'b t) -> 'b t

      (** [return x] creates a trivial computation that results in [x]  *)
      val return : 'a -> 'a t

      (** map function can be derived from bind or provided explicitly  *)
      val map : [ `Define_using_bind
                | `Custom of ('a t -> f:('a -> 'b) -> 'b t)
                ]
    end

    (** Basic binary monad interface.  *)
    module type Basic2 = sig

      (** (a,e) t is a type of monad, where [a] is a type of
          computation values, and [e] is a type of some extra
          information attached to the computation, usually a type of
          environment. *)
      type ('a, 'e) t

      (** [bind m f] passes the result of computation [m] to function [f] *)
      val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

      (** map function can be derived from bind or provided explicitly  *)
      val map : [ `Define_using_bind
                | `Custom of (('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t)
                ]

      (** [return x] creates a trivial computation that results in [x]  *)
      val return : 'a -> ('a, _) t

    end

    (** Container interface in the Kleisli category.

        The module provides two interfaces for collections
        (containters) where functions return monadic computations
        instead of values.

        As usual, two interfaces are provided:
        -  {{!Std.Monad.Collection.S}Monad.Collection.S} for unary monad
        -  {{!Std.Monad.Collection.S2}Monad.Collection.S2} for binary monad

        The container interfaces can be derived for any collection
        that implements a base interface. In our case we have two
        versions of the base interfaces,
        {{!Std.Monad.Collection.Eager}Eager} and
        {{!Std.Monad.Collection.Delay}Delay}. The interfaces differ
        only in the type of the [fold] function. The [Eager] interface
        provides a regular [fold] function, while the [Delay.fold] is
        a more general variant of [fold] that is parametrized by a
        continuation.

        If it is not possible to provide the generic version of [fold],
        then all iterators will be implemented using continuations,
        (i.e., will use a constant stack size). However, the iterators
        that maybe short circuited (i.e., a computation may be
        determined without consulting all the elements), e.g., [find],
        [exists], etc, will still still fold through all the
        elements. This is especially problematic with infinite
        sequences. So if it is possible the delayed version of [fold]
        should be provided, especially in the case of infinite
        sequences.

    *)
    module Collection : sig

      (** Base signature for collections  *)
      module type Basic = sig

        (** [x t] a container that hold values of type [x]  *)
        type 'a t

        (** [return x] creates a singleton container holding [x]  *)
        val return : 'a -> 'a t

        include Plus.S   with type 'a t := 'a t
      end

      (** A container with an eager (regular) fold.  *)
      module type Eager = sig
        include Basic

        (** fold implementation  *)
        val fold : 'a t -> init:'s -> f:('s -> 'a -> 's) -> 's
      end

      (** A container with a generic fold  *)
      module type Delay = sig
        include Basic

        (** [fold xs ~init:s ~f] a delayed fold implementation. It is
            the same as a regular [fold] except that function [f]
            accepts an extra continuation argument, and the [fold]
            expression returns a continuation.*)
        val fold : 'a t -> init:'s -> f:('s -> 'a -> ('s -> 'r) -> 'r) -> (('s -> 'r) -> 'r)
      end

      (** A container interface for a binary monad.  *)
      module type S2 = sig

        (** the monad  *)
        type ('a,'e) m

        (** type of the container  *)
        type 'a t

        (** [all cs] performs all computations in [cs] and returns a
            list of results in the same order. The order of
            evaluation is unspecified.  *)
        val all : ('a,'e) m t -> ('a t, 'e) m

        (** [all_ignore cs] performs all computations in [cs] in an
            unspecified order, and discards the results. *)
        val all_ignore : ('a,'e) m t -> (unit,'e) m

        (** [sequence cs] performs all computations in [cs] from left
            to right.  *)
        val sequence : (unit,'e) m t -> (unit,'e) m

        (** [map xs ~f] returns a container where [n]'th element is a
            result of computation [f x_n], where [x_n] is the [n]'th
            element of the input container. It is unspecified, in
            which order the computations are evaluated, and whether
            all computations are performed.  *)
        val map : 'a t -> f:('a -> ('b,'e) m) -> ('b t,'e) m

        (** [iter xs ~f] performs [f x] for each [x] in [xs] in the
            left to right order.  *)
        val iter : 'a t -> f:('a -> (unit,'e) m) -> (unit,'e) m

        (** [fold xs ~init:s0 ~f] folds [f] over [xs] in the given monad.

            Effectively computes a chain:

            {v
              f s0 x0 >>= fun s1 ->
              f s1 x1 >>= fun s2 ->
              ...
              f sN xN
            v}

            Except that the computation uses a constant stack size. *)
        val fold : 'a t -> init:'b -> f:('b -> 'a -> ('b,'e) m) -> ('b,'e) m

        (** [fold_left] is a synonym for [fold].  *)
        val fold_left  : 'a t -> init:'b -> f:('b -> 'a -> ('b,'e) m) -> ('b,'e) m

        (** [fold_right xs ~f ~init:s0] folds [f] over [xs] from right to
            left in the given monad.

            Effectively computes a chain:

            {v
              f x_N s0     >>= fun s1 ->
              f x_(N-1) s1 >>= fun s2 ->
              ...
              f x0 s_N
            v}

            Except that the computation uses a constant stack size.*)
        val fold_right : 'a t -> f:('a -> 'b -> ('b,'e) m) -> init:'b -> ('b,'e) m

        (** [reduce xs ~f] same as [fold] except that the initial
            state is obtained from the first element of the
            container, i.e., computes a sequence

            {v
              f x0 x1 >>= fun s1 ->
              f s1 x2 >>= fun s2 ->
              ...
              f sN xN
            v} *)
        val reduce : 'a t -> f:('a -> 'a -> ('a,'e) m) -> ('a option,'e) m

        (** [exists xs ~f] returns a computation that results in
            [true] iff there exists an element [x] in [xs] such that
            [f x] evaluates to [true] *)
        val exists : 'a t -> f:('a -> (bool,'e) m) -> (bool,'e) m

        (** [for_all xs ~f] returns a computation that results in
            [true] iff for all [x] in [xs] [f x] evaluates to [true].  *)
        val for_all : 'a t -> f:('a -> (bool,'e) m) -> (bool,'e) m

        (** [count xs ~f] returns a computation that results to a
            number of elements of [xs] for which [f] evaluates to
            [true]. The order of application of [f] is unspecified. *)
        val count : 'a t -> f:('a -> (bool,'e) m) -> (int,'e) m

        (** [map_reduce (module Monoid) xs ~f] a composition of [map]
            and [reduce]. Effectively the same as [map xs ~f] and then
            reduce in [Monoid] except that no intermediate
            collections are created.*)
        val map_reduce : (module Monoid.S with type t = 'a) -> 'b t -> f:('b -> ('a,'e) m) -> ('a,'e) m

        (** [find xs ~f] returns the first element [x] of [xs] for
            which [f x] evaluates to [true].  *)
        val find : 'a t -> f:('a -> (bool,'e) m) -> ('a option,'e) m

        (** [find_map xs ~f] returns the first computation [f x] for
            [x] in [xs] which will result in non [None].  *)
        val find_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b option,'e) m

        (** [filter xs ~f] returns a computation that contains all
            the elements of [xs] for which [f] evaluated to [true]. The
            order of the elements is the same. *)
        val filter : 'a t -> f:('a -> (bool,'e) m) -> ('a t,'e) m

        (** [filter_map xs ~f] is a partial mapping from [xs] to a
            collection [ys], such that all the elements of [xs] for which
            [f] returned [Some] value are mapped, while the rest are
            omitted.  *)
        val filter_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b t,'e) m
      end

      (** A container interface for an unary monad  *)
      module type S = sig

        (** the monad  *)
        type 'a m

        (** type of the container  *)
        type 'a t

        (** [all cs] performs all computations in [cs] and returns a
            list of results in the same order. The order of
            evaluation is unspecified.  *)
        val all : 'a m t -> 'a t m

        (** [all_ignore cs] performs all computations in [cs] in an
            unspecified order, and discards the results. *)
        val all_ignore : 'a m t -> unit m

        (** [sequence cs] performs all computations in [cs] from left
            to right.  *)
        val sequence : unit m t -> unit m

        (** [map xs ~f] returns a container where [n]'th element is a
            result of computation [f x_n], where [x_n] is the [n]'th
            element of the input container. It is unspecified, in
            which order the computations are evaluated, and whether
            all computations are performed.  *)
        val map : 'a t -> f:('a -> 'b m) -> 'b t m

        (** [iter xs ~f] performs [f x] for each [x] in [xs] in the
                    left to right order.  *)
        val iter : 'a t -> f:('a -> unit m) -> unit m

        (** [fold xs ~init:s0 ~f] folds [f] over [xs] in the given monad.

             Effectively computes a chain:

            {v
              f s0 x0 >>= fun s1 ->
              f s1 x1 >>= fun s2 ->
              ...
              f sN xN
            v}

             Except that the computation uses a constant stack size. *)
        val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b m) -> 'b m

        (** [fold_left] is a synonym for [fold].  *)
        val fold_left : 'a t -> init:'b -> f:('b -> 'a -> 'b m) -> 'b m

        (** [fold_right xs ~f ~init:s0] folds [f] over [xs] from right to
            left in the given monad.

            Effectively computes a chain:

            {v
              f x_N s0     >>= fun s1 ->
              f x_(N-1) s1 >>= fun s2 ->
              ...
              f x0 s_N
            v}

            Except that the computation uses a constant stack size.*)
        val fold_right : 'a t -> f:('a -> 'b -> 'b m) -> init:'b -> 'b m

        (** [reduce xs ~f] same as [fold] except that the initial
            state is obtained from the first element of the
            container, i.e., computes a sequence

            {v
              f x0 x1 >>= fun s1 ->
              f s1 x2 >>= fun s2 ->
              ...
              f sN xN
            v} *)
        val reduce : 'a t -> f:('a -> 'a -> 'a m) -> 'a option m

        (** [exists xs ~f] returns a computation that results in
            [true] iff there exists an element [x] in [xs] such that
            [f x] evaluates to [true] *)
        val exists : 'a t -> f:('a -> bool m) -> bool m

        (** [for_all xs ~f] returns a computation that results in
            [true] iff for all [x] in [xs] [f x] evaluates to [true].  *)
        val for_all : 'a t -> f:('a -> bool m) -> bool m

        (** [count xs ~f] returns a computation that results to a
            number of elements of [xs] for which [f] evaluates to
            [true]. The order of application of [f] is unspecified. *)
        val count : 'a t -> f:('a -> bool m) -> int m

        (** [map_reduce (module Monoid) xs ~f] a composition of [map]
            and [reduce]. Effectively the same as [map xs ~f] and then
            reduce in [Monoid] except that no intermediate
            collections are created.*)
        val map_reduce : (module Monoid.S with type t = 'a) -> 'b t -> f:('b -> 'a m) -> 'a m

        (** [find xs ~f] returns the first element [x] of [xs] for
            which [f x] evaluates to [true].  *)
        val find : 'a t -> f:('a -> bool m) -> 'a option m

        (** [find_map xs ~f] returns the first computation [f x] for
            [x] in [xs] which will result in non [None].  *)
        val find_map : 'a t -> f:('a -> 'b option m) -> 'b option m

        (** [filter xs ~f] returns a computation that contains all
            the elements of [xs] for which [f] evaluated to [true]. The
            order of the elements is the same. *)
        val filter : 'a t -> f:('a -> bool m) -> 'a t m

        (** [filter_map xs ~f] is a partial mapping from [xs] to a
            collection [ys], such that all the elements of [xs] for which
            [f] returned [Some] value are mapped, while the rest are
            omitted.  *)
        val filter_map : 'a t -> f:('a -> 'b option m) -> 'b t m
      end
    end

    (** Describes monadic operators.   *)
    module Syntax : sig

      (** Operators for an unary monad.  *)
      module type S = sig
        type 'a t

        (** [m >>= f] is [bind m f]  *)
        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

        (** [m >>= f] is [map m ~f]  *)
        val (>>|) : 'a t -> ('a -> 'b) -> 'b t

        (** [f >=> g] is [fun x -> f x >>= g] *)
        val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)

        (** [!!x] is [return x]  *)
        val (!!)  : 'a -> 'a t

        (** [!$f] is [Lift.unary f]  *)
        val (!$)   : ('a -> 'b) -> ('a t -> 'b t)

        (** [!$$f] is [Lift.binary f]  *)
        val (!$$)  : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)

        (** [!$$$f] is [Lift.ternary f]  *)
        val (!$$$) : ('a -> 'b -> 'c -> 'd) -> ('a t -> 'b t -> 'c t -> 'd t)

        (** [!$$$$f] is [Lift.quaternary f]  *)
        val (!$$$$) : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t)

        (** [!$$$$$f] is [Lift.quinary f]  *)
        val (!$$$$$) : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t)
      end

      module type S2 = sig
        type ('a,'e) t

        (** [m >>= f] is [bind m f]  *)
        val (>>=) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t

        (** [m >>= f] is [map m ~f]  *)
        val (>>|) : ('a,'e) t -> ('a -> 'b) -> ('b,'e) t

        (** [f >=> g] is [fun x -> f x >>= g] *)
        val (>=>) : ('a -> ('b,'e) t) -> ('b -> ('c,'e) t) -> ('a -> ('c,'e) t)

        (** [!!x] is [return x]  *)
        val (!!) : 'a -> ('a,'e) t

        (** [!$f] is [Lift.unary f]  *)
        val (!$)   : ('a -> 'b) -> (('a,'e) t -> ('b,'e) t)

        (** [!$$f] is [Lift.binary f]  *)
        val (!$$)  : ('a -> 'b -> 'c) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t)

        (** [!$$$f] is [Lift.ternary f]  *)
        val (!$$$) : ('a -> 'b -> 'c -> 'd) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t -> ('d,'e) t)

        (** [!$$$$f] is [Lift.quaternary f]  *)
        val (!$$$$) : ('a -> 'b -> 'c -> 'd -> 'e) -> (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t)

        (** [!$$$$$f] is [Lift.quinary f]  *)
        val (!$$$$$) : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
          (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t -> ('f,'s) t)
      end
    end

    (** An unary monad interface.  *)
    module type S = sig
      type 'a t

      (** [void m] computes [m] and discrards the result.  *)
      val void : 'a t -> unit t

      (** [sequence xs] computes a sequence of computations [xs] in
          the left to right order.  *)
      val sequence : unit t list -> unit t

      (** [forever xs] creates a computationt that never returns.  *)
      val forever : 'a t -> 'b t

      (** Various function combinators lifted into the Kleisli category.  *)
      module Fn : sig

        (** [id x] a monadic identity function  *)
        val id : 'a -> 'a t

        (** [ignore m] computes [m] and discards the result.  *)
        val ignore : 'a t -> unit t

        (** [nothing] is a computation that does nothing.   *)
        val nothing : unit -> unit t

        (** [non f] returns a negation of the function [f].  *)
        val non : ('a -> bool t) -> ('a -> bool t)

        (** [apply_n_times ~n f] creates a chaing of computation of
            size [n] made from applications of the same function to its
            own result.  *)
        val apply_n_times : n:int -> ('a -> 'a t) -> 'a -> 'a t

        (** [compose f g] creates a composition [f.g] of two
            function.  *)
        val compose : ('b -> 'c t) -> ('a -> 'b t) -> ('a -> 'c t)
      end

      (** The pair interface lifted into the monad.  *)
      module Pair : sig

        (** [fst (x,y)] computes [x]  *)
        val fst: ('a * 'b) t -> 'a t

        (** [snd (x,y)] computes [y]  *)
        val snd: ('a * 'b) t -> 'b t
      end

      (** The triple interface lifted into a monad.   *)
      module Triple : sig

        (** [fst (x,y,z)] computes [x]  *)
        val fst : ('a * 'b * 'c) t -> 'a t

        (** [snd (x,y,z] computes [y]  *)
        val snd : ('a * 'b * 'c) t -> 'b t

        (** [trd (x,y,z)] computes [z]  *)
        val trd : ('a * 'b * 'c) t -> 'c t
      end

      (** Lifts functions into the monad.

          A function that operates on values can be mapped to a
          function that operates on computations. We provide several
          liftes for common arities.*)
      module Lift : sig

        (** [nullary x] lifts [x] (a synonym to [return])  *)
        val nullary : 'a -> 'a t

        (** [unary f] lifts [f] *)
        val unary   : ('a -> 'b) -> ('a t -> 'b t)

        (** [binary f] lifts [f] *)
        val binary  : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)

        (** [ternary f] lifts [f] *)
        val ternary : ('a -> 'b -> 'c -> 'd) -> ('a t -> 'b t -> 'c t -> 'd t)

        (** [quaternary f] lifts [f] *)
        val quaternary : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t)

        (** [quinary f] lifts [f] *)
        val quinary : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t)
      end

      (** Interacting between monads and language exceptions  *)
      module Exn : sig

        (** [expect ?finally ~f ~catch] evaluates [f ()], if an
            exception [e] is raised during the evaluation (or call)
            then the result of whole computation will be [catch e].
            If an optional argument [?finally] is passed the
            [finally] is called after [f ()] computation or after
            [catch e] if an exception was raised.  *)
        val expect : ?finally:(unit -> unit t) -> f:(unit -> 'a t) -> catch:(exn -> 'a t) -> 'a t
      end

      (** Lifts collection interface into the monad.

          This module provides two functors that take a basic
          collection interface and provide the
          {{!Std.Monad.Collection.S}Monad.Collection.S} interface.

          See {{!Std.Monad.Collection}Monad.Collection} for more
          information. *)
      module Collection : sig
        module type S = Collection.S with type 'a m := 'a t

        (** [Eager(C)] derives
            {{!Std.Monad.Collection.S}Monad.Collection.S} for container
            [C] *)
        module Eager(T : Collection.Eager) : S with type 'a t := 'a T.t

        (** [Delay(C)] derives
            {{!Std.Monad.Collection.S}Monad.Collection.S} for container
            [C] *)
        module Delay(T : Collection.Delay) : S with type 'a t := 'a T.t
      end

      (** The {{!Std.Monad.Collection.S}Monad.Collection.S} interface
            for lists *)
      module List : Collection.S with type 'a t := 'a list

      (** The {{!Std.Monad.Collection.S}Monad.Collection.S} interface
            for sequences *)
      module Seq : Collection.S with type 'a t := 'a Sequence.t

      include Syntax.S with type 'a t := 'a t
      include Monad.S with type 'a t := 'a t

      (** Monadic operators, see
          {{!Std.Monad.Syntax.S}Monad.Syntax.S} for more.  *)
      module Syntax : Syntax.S with type 'a t := 'a t
    end

    module type S2 = sig
      type ('a,'e) t

      (** [void m] computes [m] and discrards the result.  *)
      val void : ('a,'e) t -> (unit,'e) t

      (** [sequence xs] computes a sequence of computations [xs] in
          the left to right order.  *)
      val sequence : (unit,'e) t list -> (unit,'e) t

      (** [forever xs] creates a computationt that never returns.  *)
      val forever : ('a,'e) t -> ('b,'e) t

      (** Various function combinators lifted into the Kleisli category.  *)
      module Fn : sig

        (** [id x] a monadic identity function  *)
        val id : 'a -> ('a,'e) t

        (** [ignore m] computes [m] and discards the result.  *)
        val ignore : ('a,'e) t -> (unit,'e) t

        (** [nothing] is a computation that does nothing.   *)
        val nothing : unit -> (unit,'e) t

        (** [non f] returns a negation of the function [f].  *)
        val non : ('a -> (bool,'e) t) -> 'a -> (bool,'e) t

        (** [apply_n_times ~n f] creates a chaing of computation of
            size [n] made from applications of the same function to its
            own result.  *)
        val apply_n_times : n:int -> ('a -> ('a,'e) t) -> 'a -> ('a,'e) t

        (** [compose f g] creates a composition [f.g] of two
            function.  *)
        val compose : ('b -> ('c,'e) t) -> ('a -> ('b,'e) t) -> ('a -> ('c,'e) t)
      end

      (** The pair interface lifted into the monad.  *)
      module Pair : sig

        (** [fst (x,y)] computes [x]  *)
        val fst: (('a * 'b),'e) t -> ('a,'e) t

        (** [snd (x,y)] computes [y]  *)
        val snd: (('a * 'b),'e) t -> ('b,'e) t
      end

      (** The triple interface lifted into a monad.   *)
      module Triple : sig

        (** [fst (x,y,z)] computes [x]  *)
        val fst : (('a * 'b * 'c),'e) t -> ('a,'e) t

        (** [snd (x,y,z] computes [y]  *)
        val snd : (('a * 'b * 'c),'e) t -> ('b,'e) t

        (** [trd (x,y,z)] computes [z]  *)
        val trd : (('a * 'b * 'c),'e) t -> ('c,'e) t
      end

      (** Lifts functions into the monad.

          A function that operates on values can be mapped to a
          function that operates on computations. We provide several
          liftes for common arities.*)
      module Lift : sig

        (** [nullary x] lifts [x] (a synonym to [return])  *)
        val nullary : 'a -> ('a,'e) t

        (** [unary f] lifts [f] *)
        val unary   : ('a -> 'b) -> (('a,'e) t -> ('b,'e) t)

        (** [binary f] lifts [f] *)
        val binary  : ('a -> 'b -> 'c) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t)

        (** [ternary f] lifts [f] *)
        val ternary : ('a -> 'b -> 'c -> 'd) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t -> ('d,'e) t)

        (** [quaternary f] lifts [f] *)
        val quaternary : ('a -> 'b -> 'c -> 'd -> 'e) -> (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t)

        (** [quinary f] lifts [f] *)
        val quinary : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
          (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t -> ('f,'s) t)
      end

      (** Interacting between monads and language exceptions  *)
      module Exn : sig

        (** [expect ?finally ~f ~catch] evaluates [f ()], if an
            exception [e] is raised during the evaluation (or call)
            then the result of whole computation will be [catch e].
            If an optional argument [?finally] is passed the
            [finally] is called after [f ()] computation or after
            [catch e] if an exception was raised.  *)
        val expect :
          ?finally:(unit -> (unit,'s) t) ->
          f:(unit -> ('a,'s) t) ->
          catch:(exn -> ('a,'s) t) -> ('a,'s) t
      end

      (** Lifts collection interface into the monad.

          This module provides two functors that take a basic
          collection interface and provide the
          {{!Std.Monad.Collection.S2}Monad.Collection.S2} interface.

          See {{!Std.Monad.Collection}Monad.Collection} for more
          information. *)
      module Collection : sig
        module type S = Collection.S2 with type ('a,'e) m := ('a,'e) t

        (** [Eager(C)] derives
            {{!Std.Monad.Collection.S2}Monad.Collection.S2} for container
            [C] *)
        module Eager(T : Collection.Eager) : S with type 'a t := 'a T.t

        (** [Delay(C)] derives
            {{!Std.Monad.Collection.S2}Monad.Collection.S2} for container
            [C] *)
        module Delay(T : Collection.Delay) : S with type 'a t := 'a T.t
      end

      (** The {{!Std.Monad.Collection.S2}Monad.Collection.S2} interface
            for lists *)
      module List : Collection.S with type 'a t := 'a list

      (** The {{!Std.Monad.Collection.S2}Monad.Collection.S2} interface
            for sequences *)
      module Seq : Collection.S with type 'a t := 'a Sequence.t

      include Syntax.S2 with type ('a,'e) t := ('a,'e) t
      include Monad.S2 with type ('a,'e) t := ('a,'e) t

      (** Monadic operators, see
          {{!Std.Monad.Syntax.S2}Monad.Syntax.S2} for more.  *)
      module Syntax : Syntax.S2 with type ('a,'e) t := ('a,'e) t
    end

    (** Reexports Core's [Monad.S] as [Monads.Core] *)
    module type Core = Monad.S

    (** Reexports Core's [Monad.S2] as [Monads.Core2] *)
    module type Core2 = Monad.S2

    (** The Minimal monad interface.  *)
    module type Minimal = sig
      type 'a t
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
    end

    (** The Minimal monad interface.  *)
    module type Minimal2 = sig
      type ('a,'e) t
      val return : 'a -> ('a,'e) t
      val bind : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
    end

    (** [Make(M)] derives {{!Std.Monad.S}Monad.S} from the
        {{!Std.Monad.Basic}Basic} implementation *)
    module Make(M : Basic) : S with type 'a t := 'a M.t

    (** [Make2(M)] derives {{!Std.Monad.S2}Monad.S2} from the
        {{!Std.Monad.Basic2}Basic} implementation *)
    module Make2(M : Basic2) : S2 with type ('a,'s) t := ('a,'s) M.t

    (** [Core(M)] derives {{!Std.Monad.S}Monad.S} from the
        {{!Std.Monad.Core}Core} implementation *)
    module Core(M : Core) : S with type 'a t = 'a M.t

    (** [Core2(M)] derives {{!Std.Monad.S2}Monad.S2} from the
        {{!Std.Monad.Core2}Core} implementation *)
    module Core2(M : Core2) : S2 with type ('a,'e) t = ('a,'e) M.t

    (** [Minimal(M)] derives {{!Std.Monad.S}Monad.S} from the
        {{!Std.Monad.Minimal}Minimal} implementation *)
    module Minimal(M : Minimal) : S with type 'a t = 'a M.t

    (** [Minimal2(M)] derives {{!Std.Monad.S2}Monad.S2} from the
        {{!Std.Monad.Minimal2}Minimal} implementation *)
    module Minimal2(M : Minimal2) : S2 with type ('a,'e) t = ('a,'e) M.t

    module type Monad = S
    module type Monad2 = S2

    (** The identity monad.

        The identity monad represents a computation that has the same
        semantics as a host language computation. The provided
        implementation is not derived via some functor but rather
        manually written to help optimizer to inline the identity
        monad and actually to provide code that is as efficient as the
        same code written without a monad.
    *)
    module Ident : Monad with type 'a t = 'a

    (** The option aka Maybe monad.

        This monad can be used to denote partial computations or
        a limited form of non-deterministic computations where an
        absence of result, i.e., the bottom value, can be considered
        as a possible outcome. *)
    module Option : sig

      (** The unary option monad.  *)
      module type S = sig
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Choice.S  with type 'a t := 'a t
        include Plus.S    with type 'a t := 'a t
        include Fail.S    with type 'a t := 'a t
                           and type 'a error = unit
      end

      (** The binary option monad  *)
      module type S2 = sig
        include Trans.S2
        include Monad2   with type ('a,'e) t := ('a,'e) t
        include Choice.S2  with type ('a,'e) t := ('a,'e) t
        include Plus.S2    with type ('a,'e) t := ('a,'e) t
        include Fail.S2    with type ('a,'e) t := ('a,'e) t
                            and type 'a error = unit
      end

      include S with type 'a t = 'a option
                 and type 'a m = 'a
                 and type 'a e = 'a option

      module T1(M : T1) : sig
        type 'a t = 'a option M.t
        type 'a m = 'a M.t
        type 'a e = 'a t
      end

      module T2(M : T2) : sig
        type ('a,'e) t = ('a option, 'e) M.t
        type ('a,'e) m = ('a,'e) M.t
        type ('a,'e) e = ('a,'e) t
      end

      (** [Make(M)] composes the option monad with the monad [M].   *)
      module Make(M : Monad) :
        S with type 'a m := 'a T1(M).m
           and type 'a t := 'a T1(M).t
           and type 'a e := 'a T1(M).e

      (** [Make2(M)] composes the option monad with the monad [M].   *)
      module Make2(M : Monad2) :
        S2 with type ('a,'e) t := ('a,'e) T2(M).t
            and type ('a,'e) m := ('a,'e) T2(M).m
            and type ('a,'e) e := ('a,'e) T2(M).e
    end

    (** The Result Monad.

        The result monad is a generalization of the Option monad,
        where the bottom type can bear an arbitrary value of type
        ['e]. The type ['e] is fixed and can not be changed during the
        computation.

        {{!Std.Monad.Result.Error}Monad.Result.Error} is a
        specialization of the Result monad where ['e] is fixed to
        [Error.t].

        {{!Std.Monad.Result.Exception}Monad.Result.Exception} is
        another specialization that fixes ['e] to the [exn] type.*)
    module Result : sig

      type ('a,'e) result = ('a,'e) Result.t =
        | Ok of 'a
        | Error of 'e

      module type S = sig
        type err
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Fail.S  with type 'a t := 'a t with type 'a error = err
      end

      module type S2 = sig
        include Trans.S1
        include Monad2 with type ('a,'e) t := ('a,'e) t
        include Fail.S2  with type ('a,'e) t := ('a,'e) t with type 'a error = 'a
      end

      module T1(T : T)(M : Monad) : sig
        type error = T.t
        type 'a m = 'a M.t
        type 'a t = ('a,error) result m
        type 'a e = ('a,error) result m
      end

      module T2(M : Monad) : sig
        type 'a m = 'a M.t
        type ('a,'e) t = ('a,'e) result m
        type ('a,'e) e = ('a,'e) result m
      end

      (** [Make(E)(M)] concretized the type of error to [E.t] and
          composes the Result monad with the monad [M]. *)
      module Make(T : T)(M : Monad) : S
        with type 'a t := 'a T1(T)(M).t
         and type 'a m := 'a T1(T)(M).m
         and type 'a e := 'a T1(T)(M).e
         and type err := T.t

      (** [Make2(M)] composes the result monad with the monad [M].  *)
      module Make2(M : Monad) : S2
        with type ('a,'e) t := ('a,'e) T2(M).t
         and type 'a m     := 'a     T2(M).m
         and type ('a,'e) e := ('a,'e) T2(M).e

      (** The Error monad.

          The error monad is a concretization of the result monad with
          the error type fixed to [Error.t], aka [Or_error Monad]*)
      module Error : sig
        module type S = sig
          include S

          (** [failf "<fmt>" <args> ()] constructs an error message
              using the specified format description and returns a
              computation that will result in the constructed
              error. *)
          val failf : ('a, Format.formatter, unit, unit -> 'b t) format4 -> 'a
        end

        module T(M : Monad) : sig
          type 'a m = 'a M.t
          type 'a t = 'a Or_error.t m
          type 'a e = 'a Or_error.t m
        end

        (** [Make(M)] wraps [M] into the [Error] monad.  *)
        module Make(M : Monad) : S
          with type 'a t := 'a T(M).t
           and type 'a m := 'a T(M).m
           and type 'a e := 'a T(M).e
           and type err := Error.t

        include S with type 'a t = 'a Or_error.t
                   and type 'a m = 'a
                   and type 'a e = 'a Or_error.t
                   and type err := Error.t
      end

      (** The Exception monad.

          The exception monad is a result monad that uses [exn] as
          type of values associated with an exceptional control flow.  *)
      module Exception : sig
        include S
          with type 'a t = ('a,exn) Result.t
           and type 'a e = ('a,exn) Result.t
           and type 'a m = 'a
           and type err := exn

        module T(M : Monad) : sig
          type 'a m = 'a M.t
          type 'a t = ('a,exn) Result.t m
          type 'a e = ('a,exn) Result.t m
        end
        module Make(M : Monad) : S
          with type 'a t := 'a T(M).t
           and type 'a m := 'a T(M).m
           and type 'a e := 'a T(M).e
           and type err := exn
      end
    end

    (** The List monad.

        The list monad denotes a non-deterministic computation, i.e.,
        a computation that can have more than one result or no results
        at all. *)
    module List : sig
      module type S = sig
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Choice.S  with type 'a t := 'a t
        include Plus.S    with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2   with type ('a,'e) t := ('a,'e) t
        include Choice.S2  with type ('a,'e) t := ('a,'e) t
        include Plus.S2    with type ('a,'e) t := ('a,'e) t
      end

      include S with type 'a t = 'a list
                 and type 'a m = 'a
                 and type 'a e = 'a list
      module T1(M : T1) : sig
        type 'a t = 'a list M.t
        type 'a m = 'a M.t
        type 'a e = 'a t
      end

      (** [Make(M)] composes the List monad with [M]  *)
      module Make(M: Monad)
        : S with type 'a m := 'a T1(M).m
             and type 'a t := 'a T1(M).t
             and type 'a e := 'a T1(M).e

      module T2(M : T2) : sig
        type ('a,'e) t = ('a list, 'e) M.t
        type ('a,'e) m = ('a,'e) M.t
        type ('a,'e) e = ('a,'e) t
      end

      (** [Make2(M)] composes the List monad with [M]  *)
      module Make2(M : Monad2)
        : S2 with type ('a,'e) m := ('a,'e) T2(M).m
              and type ('a,'e) t := ('a,'e) T2(M).t
              and type ('a,'e) e := ('a,'e) T2(M).e
    end

    (** The Sequence monad.

        The sequence monad is the same as the list monad, except the
        sequences are used to represent multiple results.*)
    module Seq : sig
      module type S = sig
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Choice.S  with type 'a t := 'a t
        include Plus.S    with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2   with type ('a,'e) t := ('a,'e) t
        include Choice.S2  with type ('a,'e) t := ('a,'e) t
        include Plus.S2    with type ('a,'e) t := ('a,'e) t
      end

      include S with type 'a t = 'a Sequence.t
                 and type 'a m = 'a
                 and type 'a e = 'a Sequence.t
      module T1(M : T1) : sig
        type 'a t = 'a Sequence.t M.t
        type 'a m = 'a M.t
        type 'a e = 'a t
      end

      (** [Make(M)] composes the Seq monad with [M]  *)
      module Make(M: Monad)
        : S with type 'a m := 'a T1(M).m
             and type 'a t := 'a T1(M).t
             and type 'a e := 'a T1(M).e

      module T2(M : T2) : sig
        type ('a,'e) t = ('a Sequence.t, 'e) M.t
        type ('a,'e) m = ('a,'e) M.t
        type ('a,'e) e = ('a,'e) t
      end

      (** [Make2(M)] composes the Seq monad with [M]  *)
      module Make2(M : Monad2)
        : S2 with type ('a,'e) m := ('a,'e) T2(M).m
              and type ('a,'e) t := ('a,'e) T2(M).t
              and type ('a,'e) e := ('a,'e) T2(M).e
    end

    (** The Writer monad.

        The writer monad denotes a simple effectful computations. The
        environment is represented with some type that should form the
        monoid. Effects are accumulated using the [Monoid.plus]
        operation. The Writer monad is also known a the Logger monad.
    *)
    module Writer : sig
      module type S = sig

        (** type representing the system state (environment)  *)
        type state
        include Trans.S

        (** [write s] add [s] to the current state  *)
        val write : state -> unit t

        (** [read m] reads the current state of computation [m]  *)
        val read : 'a t -> state t

        (** [listen m] reads both the computation result and the
            current state.  *)
        val listen : 'a t -> ('a * state) t

        (** [exec m] executes computation [m] and reads the state  *)
        val exec : unit t -> state m
        include Monad with type 'a t := 'a t
      end

      type ('a,'b) writer

      module T1(T : Monoid.S)(M : Monad) : sig
        type state = T.t
        type 'a m = 'a M.t
        type 'a t = ('a,state) writer m
        type 'a e = ('a * state) m
      end

      (** Make(Sum)(M) constructs a reader that uses [Sum.t] type as a
          state accumulator and composes the Writer with the monad
          [M]. *)
      module Make(T : Monoid.S)(M : Monad)
        : S with type 'a m := 'a T1(T)(M).m
             and type 'a t := 'a T1(T)(M).t
             and type 'a e := 'a T1(T)(M).e
             and type state := T1(T)(M).state
    end

    (** The Reader monad.

        The reader monad denotes a very limited form of effectful
        computation. In fact only a coeffect is allowed, i.e., a
        computation may depend on the state of the world, but can't
        change the world itself. The reader monad can be seen as a way
        of threading several functions with some common parameter
        without pollutiong the namespace with global variables.  Also
        known as the configuration monad, since a common usage would
        be to use the Reader monad to pass command line arguments and
        program configuration.  *)
    module Reader : sig

      (** The reader monad interface with the environment type fixed
          on the module level.  *)
      module type S = sig
        include Trans.S
        type env

        (** [read ()] reads the environment  *)
        val read : unit -> env t
        include Monad with type 'a t := 'a t
      end

      (** The reader monad interface with the environment type left
          variable.

          Note, although the type of the environment is variable it
          still cannot be changed during the computation.*)
      module type S2 = sig
        include Trans.S1

        (** [read ()] reads the environment.  *)
        val read : unit -> ('e,'e) t

        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

      (** an abstract type of reader computations  *)
      type ('a,'b) reader

      include S2 with type ('a,'e) t = ('a,'e) reader
                  and type 'a m = 'a
                  and type ('a,'e) e = 'e -> 'a

      module T1(T : T)(M : Monad) : sig
        type env = T.t
        type 'a m = 'a M.t
        type 'a t = ('a m, env) reader
        type 'a e = env -> 'a m
      end

      module T2(M : Monad) : sig
        type 'a m = 'a M.t
        type ('a,'e) t = ('a m,'e) reader
        type ('a,'e) e = 'e -> 'a m
      end

      (** [Make(Env)(M)] concretizes the environment type to [Env.t]
          and composes the Reader monad with [M]. *)
      module Make(T : T)(M : Monad): S
        with type 'a t := 'a T1(T)(M).t
         and type 'a m := 'a T1(T)(M).m
         and type 'a e := 'a T1(T)(M).e
         and type env := T.t

      (** [Make2(Env)(M)] composes the Reader monad with [M]. *)
      module Make2(M : Monad) : S2
        with type ('a,'e) t := ('a,'e) T2(M).t
         and type 'a m     := 'a     T2(M).m
         and type ('a,'e) e := ('a,'e) T2(M).e
    end

    (** The State Monad.

        The state monad denotes a generic effectful
        computation. Unlike [Writer] and [Reader] the [State] monad
        can perform arbitrary transformations on state. However, the
        same as with[Reader], the State monad comes in two flavors -
        with a type of state fixed at module level, or with a type of
        state left to be a type variable. In both cases the type of
        state remains constant during the computation. There is no
        difference in implementation, these are just two different
        interfaces.

  *)
    module State : sig

      (** an abstract storage  *)
      type ('a,'e) storage

      (** an abstract type of stateful computations. The type variable
          ['a] denotes types of values and the type variable ['e] denotes a
          type of the state (aka environment, aka world).  *)
      type ('a,'e) state

      (** The State Monad interface with a fixed environment.   *)
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t

        (** the environment type.  *)
        type env

        (** [put s] changes the current state to [s]  *)
        val put : env -> unit t

        (** [get s] gets the current state  *)
        val get : unit -> env t

        (** [gets p] projects the current state with the function [p]  *)
        val gets : (env -> 'r) -> 'r t

        (** [update f] updates the current state with the function [f]  *)
        val update : (env -> env) -> unit t
      end

      module type S2 = sig
        include Trans.S1
        include Monad2 with type ('a,'s) t := ('a,'s) t

        (** [put s] changes the current state to [s]  *)
        val put : 's -> (unit,'s) t

        (** [get s] gets the current state  *)
        val get : unit -> ('s,'s) t

        (** [gets p] projects the current state with the function [p]  *)
        val gets : ('s -> 'r) -> ('r,'s) t

        (** [update f] updates the current state with the function [f]  *)
        val update : ('s -> 's) -> (unit,'s) t
      end

      (** The Multi State monad.

          The multi state monad is a generalization of a state monad
          where a computation can have simultaneously more than one
          state. It can be used to denote non-deterministic effects
          and concurrent computations.

          The multi state monad introduces two new operators: [fork]
          and [switch]. The [fork] operator clones (replicates)
          current state of computation (environment), the [switch]
          operator chooses between different available states.

          States form a hierarchy, with a state in which [fork] is
          called being a parent of a newly forked state. The initial
          state, called the global state is an ancestor of all
          states.

      *)
      module Multi : sig
        type status = [`Current | `Live | `Dead]

        type 'a contexts

        type id
        module Id : sig
          include Identifiable with type t = id
          val pp : Format.formatter -> id -> unit
        end

        module type S = sig
          include Trans.S
          type id

          module Id : Identifiable.S with type t = id

          (** the identifier of the global (initial) state.  *)
          val global : id

          (** [fork ()] forks the current state.  *)
          val fork : unit -> unit t

          (** [switch id] switches to the state with the given [id] if
              such state is alive, otherwise switches to the closest
              alive ancestor of the state with the given [id] *)
          val switch : id -> unit t

          (** [parent ()] returns an identifier of the closest alive parent.  *)
          val parent : unit -> id t

          (** [ancestor ids] returns an identifier of the closest
              common ancestor of states with the given identifiers. *)
          val ancestor : id list -> id t

          (** [current id] returns an identifier of current state.  *)
          val current : unit -> id t

          (** [kill id] kills a state with the specified [id]. If [id]
              corresponds to the current state, then switches to the
              closest ancestor.  If [id = global] then do nothing.*)
          val kill : id -> unit t

          (** [forks xs] returns a sequence of all alive states  *)
          val forks : unit -> id Sequence.t t

          (** [status id] returns a status of a state with the given [id]  *)
          val status : id -> status t

          include S with type 'a t := 'a t
                     and type 'a e := 'a e
                     and type 'a m := 'a m
        end

        module type S2 = sig
          include Trans.S1
          type id

          module Id : Identifiable.S with type t = id

          (** the identifier of the global (initial) state.  *)
          val global : id

          (** [fork ()] forks the current state.  *)
          val fork : unit -> (unit,'e) t

          (** [switch id] switches to the state with the given [id] if
              such state is alive, otherwise switches to the closest
              alive ancestor of the state with the given [id] *)
          val switch : id -> (unit,'e) t

          (** [parent ()] returns an identifier of the closest alive parent.  *)
          val parent : unit -> (id,'e) t

          (** [ancestor ids] returns an identifier of the closest
              common ancestor of states with the given identifiers. *)
          val ancestor : id list -> (id,'e) t

          (** [current id] returns an identifier of current state.  *)
          val current : unit -> (id,'e) t

          (** [kill id] kills a state with the specified [id]. If [id]
              corresponds to the current state, then switches to the
              closest ancestor.  If [id = global] then do nothing.*)
          val kill : id -> (unit,'e) t

          (** [forks xs] returns a sequence of all alive states  *)
          val forks : unit -> (id Sequence.t,'e) t

          (** [status id] returns a status of a state with the given [id]  *)
          val status : id -> (status,'e) t

          include S2 with type ('a,'e) t := ('a,'e) t
                      and type ('a,'e) e := ('a,'e) e
                      and type 'a m := 'a m
        end

        module T1(T : T)(M : Monad) : sig
          type env = T.t
          type 'a m = 'a M.t
          type 'a t = (('a,env contexts) storage m, env contexts) state
          type 'a e = env -> ('a * env) m
        end

        module T2(M : Monad) : sig
          type 'a m = 'a M.t
          type ('a,'e) t = (('a,'e contexts) storage m, 'e contexts) state
          type ('a,'e) e = 'e -> ('a * 'e) m
        end

        module Make(T : T)(M : Monad): S
          with type 'a t := 'a T1(T)(M).t
           and type 'a m := 'a T1(T)(M).m
           and type 'a e := 'a T1(T)(M).e
           and type env := T.t
           and type id := id
           and module Id := Id

        module Make2(M : Monad) : S2
          with type ('a,'e) t := ('a,'e) T2(M).t
           and type 'a m     := 'a     T2(M).m
           and type ('a,'e) e := ('a,'e) T2(M).e
           and type id := id
           and module Id := Id
      end

      include S2 with type ('a,'e) t = (('a,'e) storage, 'e) state
                  and type 'a m = 'a
                  and type ('a,'e) e = 'e -> ('a * 'e)

      (** [eval m] is the same as [run m >>| fst], i.e., it runs the
          computation and returns the computed value.  *)
      val eval : ('a,'e) t -> 'e -> 'a

      (** [exec m] is the same as [run m >>| snd], i.e., it runs the
          computation and returns the final state.  *)
      val exec : ('a,'e) t -> 'e -> 'e

      module T1(T : T)(M : Monad) : sig
        type env = T.t
        type 'a m = 'a M.t
        type 'a t = (('a,env) storage m, env) state
        type 'a e = env -> ('a * env) m
      end

      module T2(M : Monad) : sig
        type 'a m = 'a M.t
        type ('a,'e) t = (('a,'e) storage m, 'e) state
        type ('a,'e) e = 'e -> ('a * 'e) m
      end

      module Make(T : T)(M : Monad): S
        with type 'a t := 'a T1(T)(M).t
         and type 'a m := 'a T1(T)(M).m
         and type 'a e := 'a T1(T)(M).e
         and type env := T.t

      module Make2(M : Monad) : S2
        with type ('a,'e) t := ('a,'e) T2(M).t
         and type 'a m     := 'a     T2(M).m
         and type ('a,'e) e := ('a,'e) T2(M).e
    end

    (** The Function Monad.

        The function monad delays the computation until it is actually
        run. This is not the only monad that has such behavior, i.e.,
        State, Cont, Lazy, and Reader monads are also delayed, but
        they all have other additional behaviors on top of the
        delaying the computation. The function monad can be seen as a
        Reader monad with [unit] environment, or as a Lazy monad
        without the memoization. *)
    module Fun : sig
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

      (** A function monad computation is a thunk that returns a value
          of type ['a]. *)
      type 'a thunk

      include S with type 'a t = 'a thunk
                 and type 'a m = 'a
                 and type 'a e = 'a

      module T1(M : Monad) : sig
        type 'a m = 'a M.t
        type 'a t = 'a m thunk
        type 'a e = 'a m
      end

      module Make(M : Monad) : S
        with type 'a t := 'a T1(M).t
         and type 'a m := 'a T1(M).m
         and type 'a e := 'a T1(M).e

      module T2(M : Monad2) : sig
        type ('a,'e) m = ('a,'e) M.t
        type ('a,'e) t = ('a,'e) m thunk
        type ('a,'e) e = ('a,'e) m
      end

      module Make2(M : Monad2) : S2
        with type ('a,'e) t := ('a,'e) T2(M).t
         and type ('a,'e) m := ('a,'e) T2(M).m
         and type ('a,'e) e := ('a,'e) T2(M).e
    end

    (** The Lazy monad.

        The lazy monad implements a call-by-need evaluation
        strategy. The computation is delayed until it is run. It uses
        OCaml built in lazy computations to implement memoization.
  *)
    module Lazy : sig
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

      include S with type 'a t = 'a Lazy.t
                 and type 'a m = 'a
                 and type 'a e = 'a

      module T1(M : Monad) : sig
        type 'a m = 'a M.t
        type 'a t = 'a m Lazy.t
        type 'a e = 'a m
      end

      module Make(M : Monad) : S
        with type 'a t := 'a T1(M).t
         and type 'a m := 'a T1(M).m
         and type 'a e := 'a T1(M).e

      module T2(M : Monad2) : sig
        type ('a,'e) m = ('a,'e) M.t
        type ('a,'e) t = ('a,'e) m Lazy.t
        type ('a,'e) e = ('a,'e) m
      end

      module Make2(M : Monad2) : S2
        with type ('a,'e) t := ('a,'e) T2(M).t
         and type ('a,'e) m := ('a,'e) T2(M).m
         and type ('a,'e) e := ('a,'e) T2(M).e
    end

    (** The continuation monad.

        The continuation monad reifies computations by making them
        available as first class values. A continuation encapsulates
        an evaluation context of a host language (OCaml in our
        case). The continuation monad behaves like it is a function
        monad, except that it provides the [call/cc] operator. The
        [call/cc] operator calls a user provided function with a
        current continuaton. This continuation can be stored in a
        state (i.e., if the Continuation monad is composed with the
        State monad) and later resumed. It may also be used to
        implement various control structures, i.e., exceptions or
        coroutines.

        The same as with the state monad we provide two interfaces,
        one with the type of final result of the computation is fixed
        on the module level, and another where it is a type variable.

    *)
    module Cont : sig

      (** The unary monad interface  *)
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t

        (** type of the final result of computation  *)
        type  r

        (** [call ~f] calls [f ~cc] with the current continuation [cc].

            The [call ~f] computation may be computed more than
            once, i.e., it would be resumed every time the
            continuation is invoked. The captured continuation
            represents the computation around the [call]. Thus
            invoking this computation will effectively escape the [f]
            function (discarding the consequent computations) and
            continue with a computation that follows the [call]. The
            continuation is multi-shot, in the sense that it can be
            called (resumed) multiple times (or not called at all). Every
            time it is called, the computation will resume at the same
            point, thus a computation that contains the [call] can be
            seen as a reenterable computation, and the [call] itself
            marks the entry point, and the continuation acts like a
            key that allows any computation that has it to reenter the
            subroutine at this point.  *)
        val call : f:(cc:('a -> _ t) -> 'a t) -> 'a t
      end

      (** The binary monad interface.  *)
      module type S2 = sig
        include Trans.S1
        include Monad2 with type ('a,'e) t := ('a,'e) t
        val call : f:(cc:('a -> (_,'e) t) -> ('a,'e) t) -> ('a,'e) t
      end

      (** type of continuation monads  *)
      type ('a,'r) cont

      include S2 with type ('a,'r) t = ('a,'r) cont
                  and type 'a m = 'a
                  and type ('a,'e) e = ('a -> 'e) -> 'e

      module T1(T : T)(M : Monad) : sig
        type r = T.t
        type 'a m = 'a M.t
        type 'a t = ('a,r m) cont
        type 'a e = ('a -> r m) -> r m
      end

      module T2(M : Monad) : sig
        type 'a m = 'a M.t
        type ('a,'e) t = ('a, 'e m) cont
        type ('a,'e) e = ('a -> 'e m) -> 'e m
      end

      (** [Make(T)(M)] wraps the monad [M] into the continuation monad
          and fix the type of the whole computation to [T.t] *)
      module Make(T : T)(M : Monad): S
        with type 'a t := 'a T1(T)(M).t
         and type 'a m := 'a T1(T)(M).m
         and type 'a e := 'a T1(T)(M).e
         and type r := T.t

      (** [Make2(M)] wrapes the monad [M] into the continuation monad.  *)
      module Make2(M : Monad) : S2
        with type ('a,'e) t := ('a,'e) T2(M).t
         and type 'a m     := 'a     T2(M).m
         and type ('a,'e) e := ('a,'e) T2(M).e
    end
  end
end
