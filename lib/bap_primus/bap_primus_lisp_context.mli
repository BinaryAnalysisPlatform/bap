(** Primus Lisp Type Class Contexts.

    @primus-lisp-internals@

    {1 Introduction}

    Type context, not to be confused with the evaluation environment,
    defines a set of features that describe the current state of the
    world. The context is a more or less static property (to some
    definition of static). The context is defined by the program that
    we are analyzing (a project structure) and by explicit
    proclamations. Thus, once a lisp feature (or a set of features) is
    loaded the context is fixed and for each definition we should be
    able to find an implementation that suits the context.  We (will
    soon) extend the notion of a context with observation classes,
    that brings more dynamic flavor to the context systems, as an
    observation instance is created in the runtime. However, the type
    of an instance (i.e., the class) is still fixed at the compile
    time, thus it is a static feature.

    {1 Application}

    The context type class system allows polymoprhic definitions. That
    means, that a definition (i.e., a meaning of a name) can have
    multiple implementation (represented with different
    formulae). This is made by specifying a context of applicapibility
    of a definiton. During the compile time the most suitable
    definition is used (i.e., a defintion that is applicable and is
    the most specific from the set of all applicable definitions).

    The property of being most suitable is defined by the partial
    ordering defined on the set of context classes, in other words, by
    using the subtype (or inclusion) polymorphism.


    {1 Representation}


    We represent a context as a map of sets. The powerset lattice
    gives a subset partial order, with the empty set being the
    superclass (the least constraint context). Every definition in
    Primus Lisp has a context of applicapibility, i.e., it is defined
    only in a specific context. By default it is an empty set.

    A definition is considered only if current context is a subset of
    the definition context. For example, if a definition context is
    [(arch arm)] then it is applicable when current context is [(arch
    arm v7)] or just [(arch arm)], and not applicable if it is [(arch
    x86)].


    {1 Work in progress}

    {2 Observation classes}

    An observation class is a particular kind of a context class, that
    allows a user to interact with the Primus observation
    system. Basically the Observation class is a production rule, that
    defines new observations that are built from other observations.

    The syntax is an abuse of the defclass stanza, i.e.,

    (defclass uaf-memory-problem (?alloc ?free ?use)
      (memcheck-acquire ?alloc ?ptr)
      (memcheck-release ?free ?ptr)
      (memcheck-violate ?use ?ptr))


    The idea is that it defines a new class of observations, that are
    derived from three other classes: memcheck-acquire,
    memcheck-release, and memcheck-violate. An additional constraint
    is that the `?ptr` field of these threee events should be equal.

    The class above is represented as the following BARE rule:

      (((memcheck-acquire ?alloc ?ptr)
       (memcheck-release ?free ?ptr)
       (memcheck-violate ?use ?ptr))
      ((uaf-memory-problem ?alloc ?free ?use)))


    Thus a production rule generates a new observation. With respect
    to the subtyping a newly generated rule is always a subtype of
    the derived rules, so it plays well with the class notion. The
    only drawback here, is that we extend our notion of subtyping from
    a pure structural one, to the nominal.

    The defclass definition is planned to provide even more than just
    BARE substitutions, but

    - when clauses that will evaluate arbitrary Primus expressions for
      an extra constraint generation

    - defparameter that will compute parameters also using arbitrary Primus
      expressions.

    The expressions called from a body of an observation class are
    evaluated in the context of this class (and all its
    superclasses). Thus it would be possible to define the behavior of
    the definition based on a particular slot of a class, for example

    {v
    (defclass chunk-allocated (?ptr ?len)
       (call-return ?name ?args ?ptr)
       (defparameter ?len (compute-allocated-size ?args)))

    (defun compute-allocated-size (n m)
      (declare (context (observe (call-return calloc))))
      (multiply n m))
    v}

    Another planned feature is to provide a mechanism for callbacks
    invocation for each defined event, (ab)using the defmethod syntax:

    {v
    (defmethod chunk-allocated (ptr len)
      (memcheck-acquire ptr len))

    v}

    This is, however, requres each slot of an observation class to be
    a value, not a symbol. So, we need to think more about it, maybe
    slots that are values, should be defined differently.


    {2 Method combination or super notation}

    The idea of the subtyping polymorphism is to provide a mechanism
    for extensible refinement of a defintion. I.e., we start with a
    generic definition and then extend it (without modification) in
    some more specific context. However, currently, there is no
    mechanisms to access a more general definition from a context of
    more specific one. I.e., we don't have the [super] notation,
    upcasting, or some method combination mechanism. So, we can't
    actually _refine_ the definition, but rather we are forced to
    _redefine_ it. This is a severe limitation that we should try to
    overcome.

    Possible solutions:

    - The super notation. Conventional programming languages allows to
      tag a definition at the point of application with some super
      tag, that basically disables the dynamic dispatching. Though it
      is usual a rather intuitive to most of the users, it is hard to
      map this solution to Primus Lisp, as we don't have the nominal
      subtyping, but rather the structural one. So a derivative
      context, don't really know which base context (if any) it
      refines. Moreover, given the partial order, it may have several
      least super types, and since we don't have names to them, we
      can't really pick one. So, it is tempting to mark this approach
      as invalid.


    - Another approach would be to provide a mechanism to upcast
      (generalize) current context, so that a more general definition
      can be still accessible from a more specific definition. Though
      this sounds reasonable, it could be hard to implement it
      correctly. If we will introduce some syntactic notation, that
      will allow a user to specify a different context of invocation,
      the it will complicate the notion of the context a
      lot. Moreover, if the upcasting operator will become a term, it
      may even make the context to be a dynamic property that is hard
      to verify statically. However, with a careful approach, this
      still can be a valid solution. A possible syntax would be:
      [(f #(new-context) args...)]. This syntax, allows a user to
      specify an arbitrary context, including downcasting. We can, of
      course, limit this by throwing a type error. But this will
      complicate the notion of a context, and will disable programs
      that are valid, i.e., it actually makes sense to derive a
      definition from a definition that is not applicable, by using it
      and amending in some way. One case, contradict, that such
      approach just shows that an abstraction was missed, and the
      commonality between two definitions should be moved as a least
      upper bound definition of both of them. But, this will in fact,
      contradict the open/closed principle, as extracting the common
      functionality would require editing the existing code.


    - Method combination. The method combination is a CLOS mechanism
      common in common lisps. Instead of calling the most specific
      instance we are actually calling methods of all instances and
      combine them according to some rule. This approach is very
      natural to the Common Lisp style of methods that are defined
      externally to the class, and looks as the most appealing
      solution. However, there are a few problems:

          1. A definition is parametrized not by _a_ type context, but
          by a product of type contexts, thus we essentially have a
          multi-method here. The applicable methods should be combined
          in some deterministic and human understandable (predictable)
          order. In CLOS (though it depends on a chose method
          combination) the applicable methods are sorted in the
          precedence order. Where the order of direct superclasses of
          a class is defined syntactically by the list of
          super-classes of a class (since CLOS has nominal subtyping),
          and the order between different classes is specified by the
          order of corresponding method parameters. Since, in Primus
          Lisp we have structural subtyping we do not have an explicit
          order between the direct super classes of a class, thus in
          which order the methods are combined is hard to
          determine. We can, however, rely on the alphabetical order,
          to make it at least predictable.

          2. If we will provide the `call-next-method` function, that
          will basically upcast the context, and switch to the more
          general definition, then the question is whether the called
          method will call the next method itself. It is like a
          cooperative generalization. Something, that I personally
          don't like as whether a method will call the parent method
          is defined in the code.

          3. The call-next-method implies that each our definition is
          not a method, not a definition. And that a definition is spread
          across an open set of definition, that are combined in some
          dynamic order.


     - Do not introduce any mechanism but rely on the advice mechanism
       instead. In Common Lisp terminology this will only leave us
       with auxiliary methods, except that the after method allows us
       to override the return value. Thus, if someone would like to
       partially override a definition, it would be possible to use
       after method to get the result of a parent computation. Since
       definitions added via the advice mechanism do not compete with
       master methods we don't have the problem of the method
       ordering.

       1. we have plenty of method combinators, that are more clean
          than (call-next-method) but still quite flexible.

       2. :before, :after, :around, and there while,until variants,
          are reasily implementable. (We can implement the around
          using the same approach as the (call-next-method), except
          that we can use a much more concrete (call-the-advised)).



*)

open Core_kernel.Std
open Bap.Std

module Attribute = Bap_primus_lisp_attribute

type t

val t : t Attribute.t


(** [of_project p] initializes a context from the project [p].  *)
val of_project : project -> t

val empty : t



(** [cx <= cx'] is true if [cx] is same as [cx'] or if [cx] is more
    specific. Where a context [c] is the same as context [c'] if [c]
    is as specific as [c'], i.e., no more, no less.

    This is a partial order, that means that it is possible that both
    contexts are neigther same, nor one is a less than of another, nor
    vice verse.

    Examples:

    {v

    ((arch arm v7)) <= ((arch arm)) => true
    ((arch arm v7) (compiler gcc)) <= ((arch arm)) => false
    v}
*)
val (<=) : t -> t -> bool


(** Partial ordering between context classes.

    We define thepartial order in terms of how generic is a
    definition.

*)
type porder =
  | Less     (** less generic:     c1 <= c2  && not(c2 <= c1) *)
  | Same     (** exactly the same: c1 <= c2  &&   c2 <= c1  *)
  | Equiv    (** not comparable :  not(c1 <= c2) && not(c2 <= c1) *)
  | More     (** more generic :  not(c1 <= c2) &&   c2 <= c1  *)

val compare : t -> t -> porder
