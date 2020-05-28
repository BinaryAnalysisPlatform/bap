open Bap.Std
open Bap_primus.Std


(** Tracks program terms evaluated by Primus.

    This module provides a component and an interface to the
    visitation tracker that keeps records of all basic block terms
    visited during Primus evaluation.

    During the initialization the component adds all basic blocks that have
    the [visited] attribute to the list of already visited blocks. This
    is done only when the system starts and the visited set is not
    synchronized anymore. Also, when a Primus system is started, the
    component will mark with the [dead] attribute all terms that are
    not already marked with with the [visited] attribute.

    Finally, when the system is stopped, the component will update the
    product data structure and mark all terms that was visited during
    the system run with the [visited] attribute.

    Although the visitation tracks only basic blocks it adds the
    [visited] and [dead] attributes to all subterms of a visited
    block.

    If a function is stubbed then when a stub is invoked the whole
    body of a function is also marked as visited (and the blocks of
    the function are added to the visited set).
*)


(** initializes the tracker component.

    This function is called by the [primus-mark-visited] component,
    and is not necessary to call, unless this plugin is not used.
*)
val init : unit -> unit


(** [progress (visited,total)] is posted every time a Primus machine
    makes progress and evaluates a previously unvisited basic block.

    [visited] - is the total number of visited basic blocks;
    [total] - is the total number of blocks in the program.
*)
val progress : (int * int) Primus.observation

(** The online interface to the set of visited basic blocks.

    {3 Example}

    {[
      module Visited = Bap_primus_track_visited.Set.Make(Machine)

      let term_is_visited t = Visited.mem (Term.tid t)
    ]}
*)
module Set : sig

  module Make(Machine : Primus.Machine.S) : sig

    (** [mem tid] is if a term with [tid] was visited.

        A term with the term identifier [tid] is visited if some
        machine posted an [enter-term] event with this [tid], or
        if it was marked with the [visited] attribute, or if it
        was forcefully added to the visited set with the [add]
        function.
    *)
    val mem : tid -> bool Machine.t


    (** [add tid] forcefully adds [tid] to the set of visited terms.

        Does nothing if tid is already visited.
    *)
    val add : tid -> unit Machine.t


    (** [del tid] forcefully removes [tid] from the set of visited terms.

        Does nothing if it wasn't visited.
    *)
    val del : tid -> unit Machine.t


    (** [all] is the current set of all visited basic blocks. *)
    val all : Tid.Set.t Machine.t
  end
end
