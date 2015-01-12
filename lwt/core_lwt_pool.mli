(** Creating pools (for example pools of connections to a database). *)

(** Instead of creating a new connection each time you need one,
    keep a pool of opened connections and reuse opened connections
    that are free.
*)

type 'a t
(** [create n ?check ?validate f] creates a new pool with at most
    [n] members. [f] is the function to use to create a new pool
    member.

    An element of the pool is validated by the optional [validate]
    function before its {!use}. Invalid elements are re-created.

    The optional function [check] is called after a [use] of an
    element failed. It must call its argument exactly once with
    [true] if the pool member is still valid and [false]
    otherwise. *)
val create :
  int ->
  ?check : ('a -> (bool -> unit) -> unit) ->
  ?validate : ('a -> bool Lwt.t) ->
  (unit -> 'a Lwt.t) -> 'a t

val use : 'a t -> f:('a -> 'b Lwt.t) -> 'b Lwt.t
(** [use p ~f] takes one free member of the pool [p] and gives it to
    the function [f]. *)
