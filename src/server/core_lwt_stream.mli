open Core_kernel.Std
open Core_lwt_container_intf
open Lwt_stream

(** Data streams *)

type 'a t = 'a Lwt_stream.t
(** Type of a stream holding values of type ['a] *)

(** Type of sources for bounded push-streams. *)
type 'a bounded_push


(** Naming convention: in this module all function taking a function
    which is applied to all element of the streams are suffixed by:

    - [_s] when the function returns a thread and calls are serialised
    - [_p] when the function returns a thread and calls are parallelised
*)

(** {2 Construction} *)

(** [from f] creates an stream from the given input function. [f] is
    called each time more input is needed, and the stream ends when
    [f] returns [None]. *)
val from : (unit -> 'a option Lwt.t) -> 'a t

(** [from_direct f] does the same as {!from} but with a function
    that does not return a thread. It is better than wrapping [f]
    into a function which return a thread. *)
val from_direct : (unit -> 'a option) -> 'a t

val create : unit -> 'a t * ('a option -> unit Or_error.t)
(** [create ()] returns a new stream and a push function. *)

val create_with_reference : unit -> 'a t * ('a option -> unit Or_error.t) * ('b -> unit)
(** [create_with_reference ()] returns a new stream and a push
    function. The last function allows to set a reference to an
    external source. This prevent the external source from being
    garbage collected.

    For example, to convert a reactive event to a stream:

    {[
      let stream, push, set_ref = Lwt_stream.create_with_reference () in
      set_ref (map_event push event)
    ]}
*)

(** [create_bounded size] returns a new stream and a bounded push
    source. The stream can hold a maximum of [size] elements.  When
    this limit is reached, pushing a new element will block until
    one is consumed.

    Note that you cannot clone or parse (with {!parse}) a bounded
    stream. These functions will raise [Invalid_argument] if you try
    to do so.

    It raises [Invalid_argument] if [size < 0]. *)
val create_bounded : int -> 'a t * 'a bounded_push

(** [of_list l] creates a stream returning all elements of [l] *)
val of_list : 'a list -> 'a t

(** [of_array a] creates a stream returning all elements of [a] *)
val of_array : 'a array -> 'a t

(** [of_string str] creates a stream returning all characters of
    [str] *)
val of_string : string -> char t


(** [clone st] clone the given stream. Operations on each stream
    will not affect the other.

    For example:

    {[
      # let st1 = Lwt_stream.of_list [1; 2; 3];;
      val st1 : int Lwt_stream.t = <abstr>
                                   # let st2 = Lwt_stream.clone st1;;
      val st2 : int Lwt_stream.t = <abstr>
                                   # lwt x = Lwt_stream.next st1;;
      val x : int = 1
                    # lwt y = Lwt_stream.next st2;;
      val y : int = 1
    ]}

    It raises [Invalid_argument] if [st] is a bounded
    push-stream. *)
val clone : 'a t -> 'a t

(** {2 Destruction} *)

(** Returns the list of elements of the given stream.
    Returns an empty list if the stream is closed, *)
val to_list : 'a t -> 'a list Lwt.t


(** Returns the word composed of all characters of the given
    stream. Returns an empty string if the stream is closed. *)
val to_string : char t -> string Lwt.t

(** {2 Data retreival} *)

val peek : 'a t -> 'a option Lwt.t
(** [peek st] returns the first element of the stream, if any,
    without removing it. *)

val npeek : int -> 'a t -> 'a list Lwt.t
(** [npeek n st] returns at most the first [n] elements of [st],
    without removing them. *)

val get : 'a t -> 'a option Lwt.t
(** [get st] remove and returns the first element of the stream, if
    any. *)

val nget : int -> 'a t -> 'a list Lwt.t
(** [nget n st] remove and returns at most the first [n] elements of
    [st]. *)

val get_while : ('a -> bool) -> 'a t -> 'a list Lwt.t
val get_while_s : ('a -> bool Lwt.t) -> 'a t -> 'a list Lwt.t
(** [get_while f st] returns the longest prefix of [st] where all
    elements satisfy [f]. *)

(** [next st] remove and returns the next element of the stream, of
    fails if the stream is empty. *)
val next : 'a t -> 'a Or_error.t Lwt.t

(** [last_new st] returns the last element that can be obtained
    without sleepping, or wait for one if no one is already
    available.

    If fails if the stream has no more elements *)
val last_new : 'a t -> 'a Or_error.t Lwt.t

(** [junk st] remove the first element of [st]. *)
val junk : ?n:int -> 'a t -> unit Lwt.t

(** [junk_while f st] removes all elements at the beginning of the
    streams which satisfy [f]. *)
val junk_while : ('a -> bool) -> 'a t -> unit Lwt.t
val junk_while_s : ('a -> bool Lwt.t) -> 'a t -> unit Lwt.t

(** [junk_old st] removes all elements that are ready to be read
    without yeilding from [st].

    For example the [read_password] function of [Lwt_read_line] use
    that to junk key previously typed by the user.
*)
val junk_old : 'a t -> unit Lwt.t

(** [get_available st] returns all available elements of [l] without
    blocking *)
val get_available : 'a t -> 'a list

(** [get_available_up_to n st] returns up to [n] elements of [l]
    without blocking *)
val get_available_up_to : int -> 'a t -> 'a list

(** [is_empty st] returns whether the given stream is empty *)
val is_empty : 'a t -> bool Lwt.t

val on_terminate : 'a t -> (unit -> unit) -> unit
(** [on_terminate st f] executes [f] when the end of the stream [st]
    is reached. Note that the stream may still contains elements if
    {!peek} or similar was used. *)

(** {2 Stream transversal} *)

(** Note: all the following functions are destructive.

    For example:

    {[
      # let st1 = Lwt_stream.of_list [1; 2; 3];;
      val st1 : int Lwt_stream.t = <abstr>
                                   # let st2 = Lwt_stream.map string_of_int st1;;
      val st2 : string Lwt_stream.t = <abstr>
                                      # lwt x = Lwt_stream.next st1;;
      val x : int = 1
                    # lwt y = Lwt_stream.next st2;;
      val y : string = "2"
    ]}
*)



(** [choose l] creates an stream from a list of streams. The
    resulting stream will returns elements returned by any stream of
    [l] in an unspecified order. *)
val choose : 'a t list -> 'a t

(** [combine s1 s2] combine two streams. The stream will ends when
    the first stream ends. *)
val combine : 'a t -> 'b t -> ('a * 'b) t

(** [append s1 s2] returns a stream which returns all elements of
    [s1], then all elements of [s2] *)
val append : 'a t -> 'a t -> 'a t

(** [concat st] returns the concatenation of all streams of [st]. *)
val concat : 'a t t -> 'a t

(** [flatten st = map_list (fun l -> l) st] *)
val flatten : 'a list t -> 'a t

(** [map f st] maps the value returned by [st] with [f] *)
val map   : f:('a -> 'b) -> 'a t  -> 'b t
val map_s : f:('a -> 'b Lwt.t) -> 'a t -> 'b t

val filter   : f:('a -> bool) -> 'a t -> 'a t
val filter_s : f:('a -> bool Lwt.t) -> 'a t -> 'a t

val filter_map   : f:('a -> 'b option) -> 'a t -> 'b t
val filter_map_s : f:('a -> 'b option Lwt.t) -> 'a t -> 'b t

(** [map_list f st] applies [f] on each element of [st] and flattens
    the lists returned *)
val map_list   : f:('a -> 'b list) -> 'a t -> 'b t
val map_list_s : f:('a -> 'b list Lwt.t) -> 'a t -> 'b t

val fold   : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b Lwt.t
val fold_s : 'a t -> f:('b -> 'a -> 'b Lwt.t) -> init:'b -> 'b Lwt.t
(** [fold f s x] fold_like function for streams. *)

(** [iter f s] iterates over all elements of the stream *)
val iter   : f:('a -> unit) -> 'a t -> unit Lwt.t
val iter_p : f:('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
val iter_s : f:('a -> unit Lwt.t) -> 'a t -> unit Lwt.t


module Push_queue : sig

  type 'a t = 'a bounded_push

  (** Size of the stream. *)
  val size : _ t -> int

  (** Change the size of the stream queue. Note that the new size
      can smaller than the current stream queue size.

      It raises [Invalid_argument] if [size < 0]. *)
  val resize : _ t -> int -> unit

  (** Pushes a new element to the stream. If the stream is full then
      it will block until one element is consumed. Fails if another thread
      is already blocked on {!push}. *)
  val push : 'a t -> 'a -> unit Or_error.t Lwt.t

  val push_all : 'a t -> 'a list -> unit Or_error.t Lwt.t

  (** Closes the stream. Any thread currently blocked on {!push}
      will fail with {!Closed}. *)
  val close : _ t -> unit

  (** Number of elements in the stream queue. *)
  val length : _ t -> int

  (** Is a thread is blocked on {!push} ? *)
  val blocked : _ t -> bool

  (** Is the stream closed ? *)
  val closed : _ t -> bool

  (** Set the reference to an external source. *)
  val set_reference : _ t -> 'b -> unit
end
