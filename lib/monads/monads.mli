open Core_kernel.Std



(** A library of Monad Transfomers.


    {2:abstract Abstract}

    A monad is an abstraction that can be used to parametrize code
    with the way how computations are sequenced. A monad can also be
    seen as design pattern, that sometimes make your code more
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
    programming language with a type of module type. In simplest
    cases, an algorithm is parametrized by simple scalar values, e.g.,

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


    However, there is even more opportunities for generalization. The
    semicolon is a sequencing operator that has a semantics that is
    usually defined by a programming language, and most often in
    regular deterministic languages [x := f a; y := f b] means: first
    compute [f a] and assign the result to [x], then compute [f b] and
    assign the result to [y]. However, what if we would like to
    parametrize our algorithm even with it:


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

    (** A choice monad.

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

    module Trans : sig
      module type S = sig
        type 'a t
        type 'a m
        type 'a e                    (* essence or effect *)

        val lift : 'a m -> 'a t
        val run : 'a t -> 'a e
      end

      module type S1 = sig
        type ('a,'e) t
        type 'a m
        type ('a,'e) e
        val lift : 'a m -> ('a,'e) t
        val run : ('a,'e) t -> ('a,'e) e
      end

      module type S2 = sig
        type ('a,'e) t
        type ('a,'e) m
        type ('a,'e) e

        val lift : ('a,'e) m -> ('a,'e) t
        val run : ('a,'e) t -> ('a,'e) e
      end
    end

    module type Basic = sig
      type 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val return : 'a -> 'a t
      val map : [ `Define_using_bind
                | `Custom of ('a t -> f:('a -> 'b) -> 'b t)
                ]
    end

    module type Basic2 = sig
      type ('a, 'e) t
      val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : [ `Define_using_bind
                | `Custom of (('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t)
                ]
      val return : 'a -> ('a, _) t
    end

    module Collection : sig
      module type Basic = sig
        type 'a t
        val return : 'a -> 'a t
        include Plus.S   with type 'a t := 'a t
      end

      module type Eager = sig
        include Basic
        val fold : 'a t -> init:'s -> f:('s -> 'a -> 's) -> 's
      end

      module type Delay = sig
        include Basic
        val fold : 'a t -> init:'s -> f:('s -> 'a -> ('s -> 'r) -> 'r) -> (('s -> 'r) -> 'r)
      end

      module type S2 = sig
        type ('a,'e) m
        type 'a t
        val all : ('a,'e) m t -> ('a t, 'e) m
        val all_ignore : ('a,'e) m t -> (unit,'e) m
        val sequence : (unit,'e) m t -> (unit,'e) m
        val map : 'a t -> f:('a -> ('b,'e) m) -> ('b t,'e) m
        val iter : 'a t -> f:('a -> (unit,'e) m) -> (unit,'e) m
        val fold : 'a t -> init:'b -> f:('b -> 'a -> ('b,'e) m) -> ('b,'e) m
        val fold_left  : 'a t -> init:'b -> f:('b -> 'a -> ('b,'e) m) -> ('b,'e) m
        val fold_right : 'a t -> f:('a -> 'b -> ('b,'e) m) -> init:'b -> ('b,'e) m
        val reduce : 'a t -> f:('a -> 'a -> ('a,'e) m) -> ('a option,'e) m
        val exists : 'a t -> f:('a -> (bool,'e) m) -> (bool,'e) m
        val for_all : 'a t -> f:('a -> (bool,'e) m) -> (bool,'e) m
        val count : 'a t -> f:('a -> (bool,'e) m) -> (int,'e) m
        val map_reduce : (module Monoid.S with type t = 'a) -> 'b t -> f:('b -> ('a,'e) m) -> ('a,'e) m
        val find : 'a t -> f:('a -> (bool,'e) m) -> ('a option,'e) m
        val find_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b option,'e) m
        val filter : 'a t -> f:('a -> (bool,'e) m) -> ('a t,'e) m
        val filter_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b t,'e) m
      end

      module type S = sig
        type 'a m
        type 'a t

        val all : 'a m t -> 'a t m
        val all_ignore : 'a m t -> unit m
        val sequence : unit m t -> unit m
        val map : 'a t -> f:('a -> 'b m) -> 'b t m
        val iter : 'a t -> f:('a -> unit m) -> unit m
        val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b m) -> 'b m
        val fold_left : 'a t -> init:'b -> f:('b -> 'a -> 'b m) -> 'b m
        val fold_right : 'a t -> f:('a -> 'b -> 'b m) -> init:'b -> 'b m
        val reduce : 'a t -> f:('a -> 'a -> 'a m) -> 'a option m
        val exists : 'a t -> f:('a -> bool m) -> bool m
        val for_all : 'a t -> f:('a -> bool m) -> bool m
        val count : 'a t -> f:('a -> bool m) -> int m
        val map_reduce : (module Monoid.S with type t = 'a) -> 'b t -> f:('b -> 'a m) -> 'a m
        val find : 'a t -> f:('a -> bool m) -> 'a option m
        val find_map : 'a t -> f:('a -> 'b option m) -> 'b option m
        val filter : 'a t -> f:('a -> bool m) -> 'a t m
        val filter_map : 'a t -> f:('a -> 'b option m) -> 'b t m
      end
    end

    module Syntax : sig
      module type S = sig
        type 'a t
        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
        val (>>|) : 'a t -> ('a -> 'b) -> 'b t
        val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
        val (!!)  : 'a -> 'a t
        val (!$)   : ('a -> 'b) -> ('a t -> 'b t)
        val (!$$)  : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)
        val (!$$$) : ('a -> 'b -> 'c -> 'd) -> ('a t -> 'b t -> 'c t -> 'd t)
        val (!$$$$) : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t)
        val (!$$$$$) : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t)
      end

      module type S2 = sig
        type ('a,'e) t
        val (>>=) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
        val (>>|) : ('a,'e) t -> ('a -> 'b) -> ('b,'e) t
        val (>=>) : ('a -> ('b,'e) t) -> ('b -> ('c,'e) t) -> ('a -> ('c,'e) t)
        val (!!) : 'a -> ('a,'e) t
        val (!$)   : ('a -> 'b) -> (('a,'e) t -> ('b,'e) t)
        val (!$$)  : ('a -> 'b -> 'c) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t)
        val (!$$$) : ('a -> 'b -> 'c -> 'd) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t -> ('d,'e) t)
        val (!$$$$) : ('a -> 'b -> 'c -> 'd -> 'e) -> (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t)
        val (!$$$$$) : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
          (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t -> ('f,'s) t)
      end
    end
    module type S = sig
      type 'a t

      val void : 'a t -> unit t
      val sequence : unit t list -> unit t
      val forever : 'a t -> 'b t

      module Fn : sig
        val id : 'a -> 'a t
        val ignore : 'a t -> unit t
        val nothing : unit -> unit t
        val non : ('a -> bool t) -> 'a -> bool t
        val apply_n_times : n:int -> ('a -> 'a t) -> 'a -> 'a t
        val compose : ('b -> 'c t) -> ('a -> 'b t) -> ('a -> 'c t)
      end

      module Pair : sig
        val fst: ('a * 'b) t -> 'a t
        val snd: ('a * 'b) t -> 'b t
      end

      module Triple : sig
        val fst : ('a * 'b * 'c) t -> 'a t
        val snd : ('a * 'b * 'c) t -> 'b t
        val trd : ('a * 'b * 'c) t -> 'c t
      end

      module Lift : sig
        val nullary : 'a -> 'a t
        val unary   : ('a -> 'b) -> ('a t -> 'b t)
        val binary  : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)
        val ternary : ('a -> 'b -> 'c -> 'd) -> ('a t -> 'b t -> 'c t -> 'd t)
        val quaternary : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t)
        val quinary : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t)
      end

      module Exn : sig
        val expect : ?finally:(unit -> unit t) -> f:(unit -> 'a t) -> catch:(exn -> 'a t) -> 'a t
      end

      module Collection : sig
        module type S = Collection.S with type 'a m := 'a t
        module Eager(T : Collection.Eager) : S with type 'a t := 'a T.t
        module Delay(T : Collection.Delay) : S with type 'a t := 'a T.t
      end

      module List : Collection.S with type 'a t := 'a list
      module Seq : Collection.S with type 'a t := 'a Sequence.t


      include Syntax.S with type 'a t := 'a t
      include Monad.S with type 'a t := 'a t
      module Syntax : Syntax.S with type 'a t := 'a t
    end

    module type S2 = sig
      type ('a,'e) t

      val void : ('a,'e) t -> (unit,'e) t
      val sequence : (unit,'e) t list -> (unit,'e) t
      val forever : ('a,'e) t -> ('b,'e) t


      module Fn : sig
        val id : 'a -> ('a,'e) t
        val ignore : ('a,'e) t -> (unit,'e) t
        val nothing : unit -> (unit,'e) t
        val non : ('a -> (bool,'e) t) -> 'a -> (bool,'e) t
        val apply_n_times : n:int -> ('a -> ('a,'e) t) -> 'a -> ('a,'e) t
        val compose : ('b -> ('c,'e) t) -> ('a -> ('b,'e) t) -> ('a -> ('c,'e) t)
      end

      module Pair : sig
        val fst: (('a * 'b),'e) t -> ('a,'e) t
        val snd: (('a * 'b),'e) t -> ('b,'e) t
      end

      module Triple : sig
        val fst : (('a * 'b * 'c),'e) t -> ('a,'e) t
        val snd : (('a * 'b * 'c),'e) t -> ('b,'e) t
        val trd : (('a * 'b * 'c),'e) t -> ('c,'e) t
      end

      module Lift : sig
        val nullary : 'a -> ('a,'e) t
        val unary   : ('a -> 'b) -> (('a,'e) t -> ('b,'e) t)
        val binary  : ('a -> 'b -> 'c) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t)
        val ternary : ('a -> 'b -> 'c -> 'd) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t -> ('d,'e) t)
        val quaternary : ('a -> 'b -> 'c -> 'd -> 'e) -> (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t)
        val quinary : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
          (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t -> ('f,'s) t)
      end

      module Exn : sig
        val expect :
          ?finally:(unit -> (unit,'s) t) ->
          f:(unit -> ('a,'s) t) ->
          catch:(exn -> ('a,'s) t) -> ('a,'s) t
      end

      module Collection : sig
        module type S = Collection.S2 with type ('a,'e) m := ('a,'e) t
        module Eager(T : Collection.Eager) : S with type 'a t := 'a T.t
        module Delay(T : Collection.Delay) : S with type 'a t := 'a T.t
      end

      module List : Collection.S with type 'a t := 'a list
      module Seq : Collection.S with type 'a t := 'a Sequence.t


      include Syntax.S2 with type ('a,'e) t := ('a,'e) t
      include Monad.S2 with type ('a,'e) t := ('a,'e) t
      module Syntax : Syntax.S2 with type ('a,'e) t := ('a,'e) t
    end

    module type Core = Monad.S
    module type Core2 = Monad.S2

    module type Minimal = sig
      type 'a t
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
    end

    module type Minimal2 = sig
      type ('a,'e) t
      val return : 'a -> ('a,'e) t
      val bind : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
    end


    module Make(M : Basic) : S with type 'a t := 'a M.t
    module Make2(M : Basic2) : S2 with type ('a,'s) t := ('a,'s) M.t
    module Core(M : Core) : S with type 'a t = 'a M.t
    module Core2(M : Core2) : S2 with type ('a,'e) t = ('a,'e) M.t
    module Minimal(M : Minimal) : S with type 'a t = 'a M.t
    module Minimal2(M : Minimal2) : S2 with type ('a,'e) t = ('a,'e) M.t

    module type Monad = S
    module type Monad2 = S2

    module Ident : Monad with type 'a t = 'a

    module Option : sig
      module type S = sig
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Choice.S  with type 'a t := 'a t
        include Plus.S    with type 'a t := 'a t
        include Fail.S    with type 'a t := 'a t
                           and type 'a error = unit
      end

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

      module Make(M : Monad) :
        S with type 'a m := 'a T1(M).m
           and type 'a t := 'a T1(M).t
           and type 'a e := 'a T1(M).e

      module Make2(M : Monad2) :
        S2 with type ('a,'e) t := ('a,'e) T2(M).t
            and type ('a,'e) m := ('a,'e) T2(M).m
            and type ('a,'e) e := ('a,'e) T2(M).e
    end

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

      module Make(T : T)(M : Monad) : S
        with type 'a t := 'a T1(T)(M).t
         and type 'a m := 'a T1(T)(M).m
         and type 'a e := 'a T1(T)(M).e
         and type err := T.t

      module Make2(M : Monad) : S2
        with type ('a,'e) t := ('a,'e) T2(M).t
         and type 'a m     := 'a     T2(M).m
         and type ('a,'e) e := ('a,'e) T2(M).e

      module Error : sig
        module type S = sig
          include S
          val failf : ('a, Format.formatter, unit, unit -> 'b t) format4 -> 'a
        end
        module T(M : Monad) : sig
          type 'a m = 'a M.t
          type 'a t = 'a Or_error.t m
          type 'a e = 'a Or_error.t m
        end

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

      module Make(M: Monad)
        : S with type 'a m := 'a T1(M).m
             and type 'a t := 'a T1(M).t
             and type 'a e := 'a T1(M).e

      module T2(M : T2) : sig
        type ('a,'e) t = ('a list, 'e) M.t
        type ('a,'e) m = ('a,'e) M.t
        type ('a,'e) e = ('a,'e) t
      end

      module Make2(M : Monad2)
        : S2 with type ('a,'e) m := ('a,'e) T2(M).m
              and type ('a,'e) t := ('a,'e) T2(M).t
              and type ('a,'e) e := ('a,'e) T2(M).e
    end


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

      module Make(M: Monad)
        : S with type 'a m := 'a T1(M).m
             and type 'a t := 'a T1(M).t
             and type 'a e := 'a T1(M).e

      module T2(M : T2) : sig
        type ('a,'e) t = ('a Sequence.t, 'e) M.t
        type ('a,'e) m = ('a,'e) M.t
        type ('a,'e) e = ('a,'e) t
      end

      module Make2(M : Monad2)
        : S2 with type ('a,'e) m := ('a,'e) T2(M).m
              and type ('a,'e) t := ('a,'e) T2(M).t
              and type ('a,'e) e := ('a,'e) T2(M).e
    end

    module Writer : sig
      module type S = sig
        type state
        include Trans.S
        val write : state -> unit t
        val read : 'a t -> state t
        val listen : 'a t -> ('a * state) t
        val exec : unit t -> state m
        val ignore : 'a t -> unit t
        val lift : 'a m -> 'a t
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        type state
        include Trans.S2
        val write : state -> (unit,'e) t
        val read : ('a,'e) t -> (state,'e) t
        val listen : ('a,'e) t -> (('a * state),'e) t
        val exec : (unit,'e) t -> (state,'e) m
        val ignore : ('a,'e) t -> (unit,'e) t
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

      type ('a,'b) writer

      module T1(T : Monoid.S)(M : Monad) : sig
        type state = T.t
        type 'a m = 'a M.t
        type 'a t = ('a,state) writer m
        type 'a e = ('a * state) m
      end

      module Make(T : Monoid.S)(M : Monad)
        : S with type 'a m := 'a T1(T)(M).m
             and type 'a t := 'a T1(T)(M).t
             and type 'a e := 'a T1(T)(M).e
             and type state := T1(T)(M).state


      module T2(T : Monoid.S)(M : Monad2) : sig
        type state = T.t
        type ('a,'e) m = ('a,'e) M.t
        type ('a,'e) t = (('a,state) writer,'e) M.t
        type ('a,'e) e = ('a * state, 'e) m
      end

      module Make2(T : Monoid.S)(M : Monad2)
        : S2 with type ('a,'e) m := ('a,'e) T2(T)(M).m
              and type ('a,'e) t := ('a,'e) T2(T)(M).t
              and type ('a,'e) e := ('a,'e) T2(T)(M).e
              and type state := T2(T)(M).state
    end

    module Reader : sig

      module type S = sig
        include Trans.S
        type env
        val read : unit -> env t
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S1
        val read : unit -> ('e,'e) t
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

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


    module State : sig

      type ('a,'e) storage
      type ('a,'e) state

      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
        type env
        val put : env -> unit t
        val get : unit -> env t
        val gets : (env -> 'r) -> 'r t
        val update : (env -> env) -> unit t
        val modify : 'a t -> (env -> env) -> 'a t
      end

      module type S2 = sig
        include Trans.S1
        include Monad2 with type ('a,'s) t := ('a,'s) t
        val put : 's -> (unit,'s) t
        val get : unit -> ('s,'s) t
        val gets : ('s -> 'r) -> ('r,'s) t
        val update : ('s -> 's) -> (unit,'s) t
        val modify : ('a,'s) t -> ('s -> 's) -> ('a,'s) t
      end

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

          val global : id

          val fork : unit -> unit t
          val switch : id -> unit t
          val parent : unit -> id t
          val ancestor : id list -> id t
          val current : unit -> id t
          val kill : id -> unit t
          val forks : unit -> id Sequence.t t
          val status : id -> status t

          include S with type 'a t := 'a t
                     and type 'a e := 'a e
                     and type 'a m := 'a m
        end

        module type S2 = sig
          include Trans.S1
          type id

          module Id : Identifiable.S with type t = id


          val global : id

          val fork : unit -> (unit,'e) t
          val switch : id -> (unit,'e) t
          val parent : unit -> (id,'e) t
          val ancestor : id list -> (id,'e) t
          val current : unit -> (id,'e) t
          val kill : id -> (unit,'e) t
          val forks : unit -> (id Sequence.t,'e) t
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

      val eval : ('a,'e) t -> 'e -> 'a
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

    module Fun : sig
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

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

    module Cont : sig
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
        type  r
        val call : cc:(('a -> _ t) -> 'a t) -> 'a t
      end

      module type S2 = sig
        include Trans.S1
        include Monad2 with type ('a,'e) t := ('a,'e) t
        val call : cc:(('a -> (_,'e) t) -> ('a,'e) t) -> ('a,'e) t
      end
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

      module Make(T : T)(M : Monad): S
        with type 'a t := 'a T1(T)(M).t
         and type 'a m := 'a T1(T)(M).m
         and type 'a e := 'a T1(T)(M).e
         and type r := T.t

      module Make2(M : Monad) : S2
        with type ('a,'e) t := ('a,'e) T2(M).t
         and type 'a m     := 'a     T2(M).m
         and type ('a,'e) e := ('a,'e) T2(M).e
    end
  end
end
