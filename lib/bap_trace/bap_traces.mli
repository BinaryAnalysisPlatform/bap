open Core_kernel.Std
open Regular.Std
open Bap.Std
(** Trace is a sequence of events accompanied with meta information.

    The sequence is lazy if possible, i.e., underlying event
    transports shall not produce events unless they are requested.

    The event is a value of type [value]. Event type is reified with
    [Value.Tag]. A set of event types in a given trace is an
    intersection of the following three sets:

    - Event types supported by a trace tool;

    - Event types that are supported by an event transport (i.e.,
    protocol)

    - Types of events that has occurred in the course of the a given
    trace.

    Since it is worthwhile to know whether a particular event is not the
    trace because it didn't occur during program execution, but not
    because it wasn't detected by a trace tool or dropped by a given
    transport, we provide [supports] function, to query whether the
    a given event type is really supported (and thus might occur) by a given trace.

    The meta information is also represented using [value] type, and
    thus can contain virtually any data. Meta information is indexed
    with [tag] value.
*)
module Std : sig
  module Move : sig
    type 'a t = {
      cell : 'a;
      data : word;
    } with bin_io, compare, fields, sexp
  end

  module Chunk : sig
    type t = {
      addr : addr;
      data : string;
    } with bin_io, compare, fields, sexp
  end

  module Syscall : sig
    type t = {
      number : int;
      args : word array;
    } with bin_io, compare, fields, sexp
  end

  module Exn : sig
    type t = {
      number : int;
      src : addr option;
      dst : addr option;
    } with bin_io, compare, fields, sexp
  end

  module Location : sig
    type t = {
      name : string option;
      addr : addr;
    } with bin_io, compare, fields, sexp
  end

  type location = Location.t with bin_io, compare, sexp

  module Call : sig
    type t = {
      caller : location;
      callee : location;
      args : word array;
    } with bin_io, compare, fields, sexp
  end

  module Return : sig
    type t = {
      caller : string;
      callee : string;
    } with bin_io, compare, fields, sexp
  end

  module Modload : sig
    type t = {
      name : string;
      low : addr;
      high : addr;
    } with bin_io, compare, fields, sexp
  end

  type 'a move = 'a Move.t with bin_io, compare, sexp
  type chunk = Chunk.t with bin_io, compare, sexp
  type syscall = Syscall.t with bin_io, compare, sexp
  type exn = Exn.t with bin_io,compare,sexp
  type call = Call.t with bin_io,compare,sexp
  type return = Return.t with bin_io,compare,sexp
  type modload = Modload.t with bin_io,compare,sexp

  module Event : sig

    (** an read access to a memory cell  *)
    val memory_load : addr move tag

    (** a write access to a memory cell  *)
    val memory_store : addr move tag

    (** a value was read from a given register  *)
    val register_read : var move tag

    (** a value is written to the specified register  *)
    val register_write : var move tag

    (** this event can used to synchronize traces  *)
    val timestamp : int64 tag

    (** CPU PC register changed its value  *)
    val pc_update : addr tag

    (** CPU loaded this memory chunk for execution. This event
          occurs just before the execution. All side effects of
          the code execution occurs after this event. *)
    val code_exec : chunk tag

    (** operating system has performed context switching to a provided
        thread (process) id. *)
    val context_switch : int tag

    (** a system call has occured  *)
    val syscall : syscall tag

    (** a software exception has occured.  *)
    val exn : exn tag

    (** a control flow transfer from one procedure to another has occured  *)
    val call : call tag

    (** a return from a call has occured  *)
    val return : return tag

    (** represent an executable module being loaded *)
    val modload : modload tag
  end

  module Tracer : sig
    type t = {
      name : string;
      args : string array;
      version : string;   (** release or Git hash, or SVN number *)
    } with bin_io, compare, sexp
  end

  module Binary : sig
    type t = {
      path : string;
      stripped : bool option;     (* yes, no, unknown *)
    } with bin_io, compare, sexp
  end

  module File_stats : sig
    type t = {
      size  : int;
      atime : float;
      mtime : float;
      ctime : float;
    } with bin_io, compare, sexp
  end

  type tracer = Tracer.t with bin_io, compare, sexp
  type binary = Binary.t with bin_io, compare, sexp
  type file_stats = File_stats.t with bin_io, compare, sexp

  module Meta : sig

    (** description of a tracer that was used to create the trace  *)
    val tracer : tracer tag

    (** description of a target binary (executable) that was traced.*)
    val binary : binary tag

    (** description of binary architecture. *)
    val arch : arch tag

    (** file stats of the traced binary  *)
    val binary_file_stats : file_stats tag

    (** trace creation time  *)
    val trace_ctime : float tag

    (** trace last modification time  *)
    val trace_mtime : float tag

    (** a user that created the trace  *)
    val user : string tag

    (** a name of a host from where trace was born  *)
    val host : string tag

  end

  module Trace : sig
    type event = value with bin_io, sexp, compare
    type monitor
    type proto
    type tool with bin_io, sexp
    type id
    type t

    type io_error = [
      | `Protocol_error of Error.t   (** Data encoding problem         *)
      | `System_error of Unix.error  (** System error                  *)
    ]

    type error = [
      | io_error
      | `No_provider    (** No provider for a given URI               *)
      | `Ambiguous_uri  (** More than one provider for a given URI    *)
    ]

    (** {2 Serialization}

        Serialization is dispatched by a URI describing data source or
        destination. URI contains enough information to uniquely designate
        data format and transporting options.
    *)


    (** [load ~monitor uri] fetches trace from a provided [uri].
        [monitor] is fail_on_error by default. *)
    val load : ?monitor:monitor -> Uri.t -> (t,error) Result.t

    (** [save uri] pushes trace to a provided [uri] *)
    val save : Uri.t -> t -> (unit,error) Result.t

    (** {2 Meta attributes}

        Meta information relates to the whole trace.

    *)

    val id : t -> id

    (** [set_attr trace attr value] updates [trace] meta attribute [attr]
        with a provided value. *)
    val set_attr : t -> 'a tag -> 'a -> t

    (** [get_attr trace attr] retrieves a value of a given attribute
        [attr] *)
    val get_attr : t -> 'a tag -> 'a option

    (** [has_attr trace attr] evaluates to [true] if [trace] has a given
        attribute [attr] *)
    val has_attr : t -> 'a tag -> bool

    (** [tool trace] returns a descriptor of a tool that was used to
        create the [trace]. *)
    val tool : t -> tool

    (** [meta trace] returns all [trace] attributes as a dictionary  *)
    val meta : t -> dict

    (** [set_meta trace meta] substitutes meta attributes of a [trace] with
        attributes taken from a dictionary [meta].*)
    val set_meta : t -> dict -> t

    (** {2 Querying  data}  *)

    (** [supports trace feature] is [true] if a tool that was used to
        generate the trace, as well as transporting protocol and
        underlying format support the a given feature. *)
    val supports : t -> 'a tag -> bool

    (** [memoize trace] eagerly loads all the trace into memory.   *)
    val memoize : t -> t

    (** [find trace tag] find an event with a given [trace]   *)
    val find : t -> 'a tag -> 'a option

    (** [find_all trace tag] returns a sequence of all event with a given tag  *)
    val find_all : t -> 'a tag -> 'a seq

    (** [find_all_matching trace matcher] returns a sequence of events
        matching with a provided [matcher]. *)
    val find_all_matching : t -> 'a Value.Match.t -> 'a seq

    (** [fold_matching trace matcher ~f ~init] applies function [f]
        consequently to all matching trace event. Matching is defined
        using value [matcher]. The following example will collect all
        memory operations from a trace:
        {[
          let collect_memory_operations trace =
            List.rev @@
            fold_matching trace ~init:[] ~f:(fun xs x -> x @ xs)
              Value.Match.(begin
                  case memory_load  (fun x  -> [`Load x])  @@
                  case memory_store (fun x  -> [`Store x]) @@
                  default           (fun () -> [])
                end)
        ]}
    *)
    val fold_matching : t -> 'a Value.Match.t -> f:('b -> 'a -> 'b) -> init:'b -> 'b

    (** [contains trace tag] returns [Some true] if a provided event
        occurs in a trace, [Some false] if it may occur (i.e., is
        supported), but is not in the trace, and [None] if the event is not
        supported at all. *)
    val contains : t -> 'a tag -> bool option

    (** [event trace] returns a sequence of events of the [trace].
        This function should be used if the above specified functions
        doesn't answer your needs.*)
    val events : t -> event seq

    (** {2 Trace construction}  *)

    (** [create tool] creates an trace that will contain events, produced
        by a specified [tool]. Initially trace contains an empty sequence
        of events. *)
    val create : tool -> t

    (** [unfold ~monitor tool ~f ~init] creates a trace by unfolding a function [f].
        The produces sequence is lazy, i.e., functions are called as
        demanded. [monitor] is fail_on_error by default. *)
    val unfold : ?monitor:monitor -> tool -> f:('a -> (event Or_error.t * 'a) option) -> init:'a -> t

    (** [unfold' ~monitor tool ~f] is a simplified version of [unfold] *)
    val unfold' : ?monitor:monitor -> tool -> f:(unit -> event Or_error.t option) -> t

    (** [add_event trace tag] appends an event to a sequence of events of
        [trace]. *)
    val add_event : t -> 'a tag -> 'a -> t

    (** [append trace events] creates a trace with a sequence of events
        composed from events of the [trace] following the [events],
        provided as an argument. *)
    val append : t -> event seq -> t


    (** {2 Extension mechanism}

        A trace is a collaborative work of several underlying layers:
        - a trace tool itself
        - a transport, that delivers data from the [tool]
        - a protocol that is used to deliver and interpret data.

        For example, a tools is an instrumented qemu-user, a transport can
        be just a file, and protocol can be Google protobuf.

        Ideally, this three instances should be totally orthogonal, so
        that one can match them. In real life, we will strive to support
        only specific combinations.

        The extension mechanism allows a user to add support for new
        transports and protocols. The separation between transport and
        protocol is left beyond the scope of this interface. A user is
        welcome to built its own protocol stacks and reify them into
        explicit API.

        The interface is designed to support both static and dynamic trace
        tools. Although, the interface to control a dynamic tool is also
        left outside of the trace interface.

        In order to establish a correspondence between a concrete trace
        instance and a trace generator a unique id is used. Each time a
        trace is opened a fresh new unique id is generated that is passed
        to both sides: to the trace (accessible via [id] function) and to
        the trace reader (passed as a parameter). This [id] can be used
        from the client side to dynamically control a trace tool.

    *)

    module type S = sig
      val name: string
      val supports: 'a tag -> bool
    end

    module type P = sig
      include S
      val probe: Uri.t -> bool
    end

    val register_tool  : (module S) -> tool
    val register_proto : (module P) -> proto

    (** Reader interface.  *)
    module Reader : sig
      (** This is an interface that should be implemented to add a new
          backend.
      *)

      type t = {
        tool : tool;                (** a tool descriptor read from trace *)
        meta : dict;                (** meta information read from trace  *)
        next : unit -> event Or_error.t option;     (** a stream function  *)
      }
    end

    type reader = Reader.t

    val register_reader : proto -> (Uri.t -> id -> (reader, io_error) Result.t) -> unit

    val register_writer : proto -> (Uri.t -> t -> (unit, io_error) Result.t) -> unit

    module Id : Regular with type t = id

    (** Monitor defines an error handling policy.*)
    module Monitor : sig
      type t = monitor

      (** [ignore_errors] filters good events and silently drops error events  *)
      val ignore_errors : t
      (** [warn_on_error on_error] same as [ignore_errors] but calls
          [on_error] function when an error has occured *)
      val warn_on_error : (Error.t -> unit) -> t

      (** [fail_on_error] will fail with an [error] [Error.raise error] *)
      val fail_on_error : t

      (** [stop_on_error] will silently finish a stream in case of error.  *)
      val stop_on_error : t

      (** [pack_errors pack] will transform any occured error into event
            using [pack] function.  *)
      val pack_errors : (Error.t -> event) -> t

      (** [create filter] creates a user defined monitor from function
          [filter] that is applied to a sequence of events or errors, and
          returns a sequence of events.  *)
      val create : (event Or_error.t seq -> event seq) -> t
    end
  end
end
