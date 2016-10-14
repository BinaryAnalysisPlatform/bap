open Bap.Std

module Limit : sig
  class virtual t : object('s)
    method virtual update : 't 'p. ('p,'t) cls -> 't term -> 's
    method virtual reached : bool
  end

  val nothing : t
  val all : t list -> t
  val max_trace_length : int -> t
  val max_loop_length : int -> t
end


(** Execution policies.

    Execution policy defines how an interpreter will go through the
    program terms. The [consistent] policy implements a behavior that
    is consistent with the operational semantics of BIL. Other
    policies will adjust conqueror behavior in a such way, that it
    will visit more terms, but will violate the semantics, e.g., will take
    unfeasible paths.

    The [conqueror] interpreter, after evaluating a block, will also
    evaluate each jmp term of a block in the current context. A guard
    condition of a jmp term will be ignored. The resulting context
    will be stored in the crawler with the [save] method.

    Once a limit is reached, or the destination term became
    undetermined, a backtracking mechanism is triggered with the
    [backtrack] method.

*)
module Crawler : sig

  class type bookkeeping = object('s)
    (** [visit tid] is invoked every time a term is visited (entered)  *)
    method visit : tid  -> 's


    (** [visited] returns a set of visited terms  *)
    method visited : Tid.Set.t
  end

  class type ['c] backtracking = object('s)
    (** [save checkpoint ctxt] is called every time a conqueror is
        trying to store a check point.  *)
    method save : tid -> 'c -> 's



    (** [backtrack] is either [None] meaning that no more
        backtracking is available or desired, or [Some (crawler,ctxt)],
        where [ctxt] is the new [ctxt] with which an execution should
        continue, and [crawler] is a new [crawler], that should be used
        for the execution.   *)
    method backtrack : ('s * 'c) option
  end


  (** crawler interface.   *)
  class type ['c] t = object('s)
    inherit bookkeeping
    inherit ['c] backtracking
  end


  (** {2 Implementations}

      Policy implementations (or policies) are provided as factory
      functions, that will produce fresh new policies of a given
      kind.
  *)

  type 'a factory = unit -> 'a t

  (** [consistent] with the operational semantics crawler.*)
  val consistent : 'a factory

  (** [exponential] a crawler, that will visit all paths.  *)
  val exponential : unit -> 'a t

  (** [linear] a crawler that will visit linearly independent paths.  *)
  val linear : 'a factory

  (** Base classes for implementing custom policies.

      This module provides mixin classes that can be used to implement
      new crawling policies.*)
  module Mixin : sig


    (** [with_bookeeping] will implement method [visit] and [visited].
        The [visit] method will store each visited term into the set of
        terms, that is returned by the [visited] method.  *)
    class ['a] with_bookkeeping : bookkeeping

    (** [without_backtracking] disables backtracking.

      - method [save] is an identity;
      - method [backtrack] will always return [None].
    *)
    class ['a] without_backtracking : ['a] backtracking


    (** [without_backtracking] bactracks everything.
        Will store all checkpoints, that are proposed by an interpreter,
        and will restore all of them.  With this policy an interpreter
        will visit all paths (modulo limit constraint).  *)
    class ['a] with_backtracking : ['a] backtracking
  end


end

type limit = Limit.t
type 's crawler = 's Crawler.t

class context :
  ?limit:limit ->
  ?crawler:'s Crawler.factory ->
  program term -> object('s)
    inherit Biri.context
    method limit : limit
    method with_limit : limit -> 's
    method crawler : 's crawler
    method with_crawler : 's crawler -> 's
  end

class ['a] main : object
  inherit ['a] biri
  constraint 'a = #context
end
