(** Taint Analysis Framework.

    Taint analysis is a dynamic data flow analysis, that tracks values
    on demand, i.e., the engine doesn't track all values, but only
    those that are marked as tainted.

    In general, we say that a computation is tainted by an object, if
    its value is dependent (or somehow influenced) on the value of that
    object. However, the precise definition depends on the taint policy
    that is pluggabe and customizable. Some taint policies might be
    more relaxed, than others.

    Traditionally, we use the word "taint" to represent an abstract
    entity whose influence we consider as undesirable or unsafe. Thus a
    computation is tainted, if it depends on a program runtime
    representation of that entity. An actual application of the
    framework may, of course, bend the notion of taint, and instead of
    attaching a negative connotation, treat it in a positive way thus
    checking for the liveness property instead of the safety property.

    In the framework a taint represents an object of some kind, that
    we would like to track, i.e., we would like to know which values
    are affected by the value of that object, which program terms
    compute those values, etc. The engine can track several such
    objects at the same time and can distinguish objects of different
    kinds as well objects of the same kind. In other words a particular
    computation could be tainted by several taints, that
    correspondingly could have different kinds.  To make it easier to
    understand let's take a classical taint analysis example with SQL
    sanitization. We would like to track all objects that were produced
    by sources that we do not trust. These objects are abstract to the
    framework and are introduced arbitrary by the analysis. For
    example, suppose there exists several calls to [recv], and our
    analysis introduces a taint on each call to [recv], that it
    designates with the [unescaped-query] kind. Every call to [recv]
    will create a new object that will belong to the same kind. Now,
    let's assume that our analysis designates the [sql_escape] function
    as a function that negates the untrustworthy effect of the input,
    and essentially clears the taint, or sanitizes it in our
    parlance. A value passed to the [sql_escape] function may have
    several taints attached to it. Some of those taints may be
    irrelevant to the [unescaped-query] kind, i.e., attached by other
    analysis. Since the query could be built from multiple calls to
    recv, there could be more than one taints of the [unescaped-query]
    kind attached to the value. So our analysis will clear all taints
    of the specified kind. Finally, when the privileged location is
    reached, e.g., the [sql_exec] function is called, we check that
    values passed to that function doesn't have any taints of the
    specified kind.

    Other than classical approaches to taint analysis, decribed above,
    the framework could be used for checking liveness properties,
    lifetime analysis, and other analysis that requires tracking the
    flow of information.
*)


open Core_kernel.Std
open Bap.Std
open Bap_primus.Std

module Std : sig


  (** Abstract value.

      Anything isomorphic to the value, is a value. Since we define
      isomorphism modulo the machine monad it is possible to define
      abstract types that could be easily embedded in the Primus runtime
      value representation. *)
  module type Value = sig
    type t
    type 'a m
    (** [to_value x] injects [x] into the [value] domain  *)
    val to_value : t -> Primus.value m

    (** [of_value v] project the value [v] to the abstract domain of [t]  *)
    val of_value : Primus.value -> t m
  end

  (** Abstract taint.

      This module defines several abstractions that are used to track
      objects as well as provide the interface to the taint tracker,
      that could be used for introducing and sanitizing taints or even
      for specifying the taint propagation policies.
  *)
  module Taint : sig


    (** Each object that the engine tracks has an associated kind,
        that denotes a class of objects that share the same semantic
        properties.

        In layman terms, kind allows analyses to distinguish their
        taints from other taints, since it introduces namespacing
        between different analyses so that several analyses could be
        ran in parallel without intersecting with each other.
    *)
    module Kind : sig
      type t


      module Make(Machine : Primus.Machine.S) : sig

        (** [create name] creates a kind with the given name.  If a
            kind with the given name already exists, then returns that
            kind.

            In other terms, [create] interns name in the [Kind] module
            and together with the [name] function establishes an
            isomorphism (bijection) between the set of names (strings)
            and the set of kinds.   *)
        val create : string -> t Machine.t


        (** [name k] returns the symbolic name of the kind [k]  *)
        val name : t -> string Machine.t

        include Value with type t := t
                       and type 'a m := 'a Machine.t
      end
      include Comparable.S_plain
        with type t := t
         and type comparator_witness = Primus.Value.comparator_witness
    end

    (** Relation between a value and an object that we track.

        The framework tracks runtime values. Each value is a word,
        e.g., a byte, a bit, or a machine word, that can be somehow
        related to the actual object that we track.

        There could be different kinds of relations, denoted with
        different values of this abstract type. So far, we distinguish
        between the [direct] and [indirect] relation.
    *)
    module Rel : sig
      type t
      (** Denotes the direct relation between a value and the object
          that we track, e.g., a value contains the runtime
          representation of the object or a part of it. *)
      val direct : t


      (** Denotes the indirect relation between a value and the object
          that we track, e.g., a value is a pointer that points to a
          value that has the direct relation with the object.  *)
      val indirect : t
    end

    (** Each taint represents an abstract object that we would like
        to track. Every time a new taint is introduced we create a
        fresh new value, that represents the abstract object that we
        are tracking.

        The analysis may use this value as an identity of that object,
        and associate various attributes with it.
    *)
    module Object : sig
      type t

      module Make(Machine : Primus.Machine.S) : sig


        (** [create kind] creates a fresh new object identifier with
            the specified [kind].

            The created value is never equal to any value created
            before in the given machine. And the same kind can be
            shared by several different objects. Thus [create]
            establishes a surjection of objects onto the set of their
            kinds, i.e., it partitions the set of objects.  *)
        val create : Kind.t -> t Machine.t


        (** [kind obj] returns the kind of the object.  *)
        val kind : t -> Kind.t Machine.t

        include Value with type t := t
                       and type 'a m := 'a Machine.t
      end
      include Comparable.S_plain
        with type t := t
         and type comparator_witness = Primus.Value.comparator_witness
    end

    (** Taint tracker control module.

        This interface is machine specific, and thus is a functor. It
        is designed to be applied as follows:

        [module Tracker = Taint.Tracker(Machine)]
    *)
    module Tracker(Machine : Primus.Machine.S) : sig


      (** {3 The low-level interface}

          The low-level interface defines three primitives in terms of
          which we can express a more convenient high-level
          interface. It is recommened to get acquainted with these
          three primitives, to understand how the tracker works,
          however it is better to use the high level interface,
          whenever it is possible.

          To denote the concrete semantics of these primitives we will
          use the taint ternary relation [T] that establishes a
          relation between a value and an object, e.g., [T(v,r,x)] is
          the relation [r] between the value [v] and an object [x].

          Conceptually, the state of the tracker can be seen as a set
          of such relations. Since tracker operations may affect this set
          of relations we will denote relations that existed just
          before an operation with [pre], e.g., [pre(T(v,r,x)] means
          that before the operation there was the relation [T(v,r,x)]
      *)

      (** [attach v r xs] establishes the relation [r] between the
          value [v] and every object [x] in the set of objects [xs].
          All other relations, as well as relations with other values
          are unaffected.

          Post conditions:
          - forall x, x in xs -> T(v,r,x);
          - forall u,s,x, pre(T(u,s,x)) -> T(u,s,x);
          - forall u,s,x, T(v,s,x) -> pre(T(v,s,x)) \// s = r /\\ v = u.
      *)
      val attach : Primus.value -> Rel.t -> Object.Set.t -> unit Machine.t


      (** [lookup v r] returns a set [xs] of objects that are related
          with the value [v] by the relation [r]. The operation
          doesn't change the state of the tracker.

          Post conditions:
          - x in xs iff T(v,r,x);
          - forall u,s,y, Pre(T(u,s,y)) iff T(u,s,y);
      *)
      val lookup : Primus.value -> Rel.t -> Object.Set.t Machine.t

      (** [detach v r xs] removes all relations of type [r] between
          the value [v] and elements of the set of objects
          [xs]. Relations of other types as well as relations between
          other objects and values are unaffected.

          Post conditions:
          - forall x, x in xs -> not (T(v,r,x));
          - forall u,s,y, v <> u \// s <> r, pre(T(u,s,y)) -> T(u,s,y)
      *)
      val detach : Primus.value -> Rel.t -> Object.Set.t -> unit Machine.t



      (** {3 High-level interface}

          The high-level interface provides a set of easy to use (and
          sometimes more efficient) functions. All these functions
          could be expressed in terms of the three primitive operations.

      *)


      (** [new_direct v k] introduces a new direct relation between the
          value [v] and a freshly created object of the given kind
          [k]. The object is returned.

          Essentially:
          [attach v direct (add (lookup v k) (Object.create k as r)); r]

      *)
      val new_direct : Primus.value -> Kind.t -> Object.t Machine.t

      (** [new_indirect ~addr:v ~len:n k] establishes a new indirect
          relation between a set of addresses, denoted by interval
          [[v,v+n-1]], and a freshly created object of specified
          kind. *)
      val new_indirect :
        addr:Primus.value ->
        len:Primus.value ->
        Kind.t ->
        Object.t Machine.t

      (** [Taint.sanitize r k v] detaches all objects related to the
          value [v] by the relation [r] that has the given kind [k].

          In terms of the low-level operations:

          [detach v r (filter (has_kind k) (lookup v r))]
      *)
      val sanitize : Primus.value -> Rel.t -> Kind.t -> unit Machine.t


      (** [transfer rs rd srcs dst] select all objects
          related with values in [srcs] by the relation [rs] and
          associate them with the value [dst] using the relation
          [rd]. The relations of the source values are unaffected.

          In terms of the low-level operations:
          [attach dst rd (union (map srcs (fun v -> lookup v rs)))]
      *)
      val transfer : Kind.t -> Rel.t -> Rel.t -> Primus.value list -> Primus.value -> unit Machine.t
    end

    module Propagation : sig
      module Policy : sig
        type t
        module Make(Machine : Primus.Machine.S) : sig

          (** [select k p] selects the taint propagation policy [p]
              for objects of kind [k]. It is possible to select
              several policies for the same kind. If no policy is
              selected, then the default policy will be selected.
          *)
          val select : Kind.t -> t -> unit Machine.t

          (** [set_default policy] makes [policy] the default policy
              for all kinds that didn't select their own taint
              propagation policies.  *)
          val set_default : t -> unit Machine.t

          (** [kinds policy] returns a set of kinds that uses the
              specified taint propagation [policy]. *)
          val kinds : t -> Kind.Set.t Machine.t

          include Value with type t := t
                         and type 'a m := 'a Machine.t
        end
      end
    end

    module Gc : sig

      (** [taint_killed t] occurs when the taint [t] is no longer
          reachable and goes out of existence.

          The event occurs during the GC collection cycle and there
          could be a significant delay between the actual time when the
          taint become unreachable and the time when the observation is
          made. *)
      val taint_killed : Object.t Primus.observation


      (** Conservative Garbage Collector.

          The conservative garbage collector may keep taints alive
          even when they become unreachable. This is an
          over-approximation and it is possible to devise a more precise
          GC, especially if soundness is not required (or not strictly
          required).

          Taint is live if either of the following is true:
          1. it is attached to a value of a variable in [Env.all];
          2. it is attached to any address.

          The second clause gives a possibility for
          over-approximation, as we do not track, whether an address
          is reachable from the current program location. So once a
          tainted value is stored and the taint is attached to an
          address, the only way to kill this taint, is to overwrite it
          with another value using another store operation.

          Currently, the garbage collection runs every basic block,
          but this may change in future.
      *)
      module Conservative : Primus.Machine.Component
    end
  end
end
