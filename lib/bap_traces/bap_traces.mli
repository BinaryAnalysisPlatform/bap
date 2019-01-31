open Core_kernel
open Regular.Std
open Bap.Std

(** Traces of execution. *)
module Std : sig


  (** the trace  *)
  type trace


  (** Trace is a stream of events plus meta data.

      It can be viewed as an input channel. In fact, [Trace.t] is an
      abstract data type usually inhabited with codata, i.e., some
      entity with hidden state. The [Trace] may be an interface to a
      remote server, virtual machine or just a file. So treat the
      trace as something that works as an input channel.

      Since it is worthwhile to know whether a particular event is not the
      trace because it didn't occur during program execution, but not
      because it wasn't detected by a trace tool or dropped by a given
      transport, we provide [supports] function, to query whether the
      given event type is really supported (and thus might occur) by a
      given trace.

      The meta information is also represented using [value] type,
      and thus can contain virtually any data. Meta information is
      indexed with [tag] value. Unlike the events, that are codata,
      meta is a regular data, obtained from a trace source at the
      time of trace creation and is not changed magically in the trace
      lifetime. *)
  module Trace : sig

    type event = value [@@deriving bin_io, sexp, compare]
    type monitor
    type proto
    type tool [@@deriving bin_io, sexp]
    type id
    type t = trace

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

        Serialization is dispatched by an URI describing data source or
        destination. URI contains enough information to uniquely designate
        data format and transporting options.
    *)


    (** [load ~monitor uri] fetches trace from a provided [uri].
        [monitor] is fail_on_error by default. *)
    val load : ?monitor:monitor -> Uri.t -> (t,error) Result.t

    (** [save uri] pushes trace to a provided [uri] *)
    val save : Uri.t -> t -> (unit,error) Result.t


    (** {2 Creating}  *)

    (** [create tool next] creates a new trace from the observer
        [next].  *)
    val create : ?monitor:monitor -> tool -> (unit -> event Or_error.t option) -> t

    (** {2 Meta attributes}

        Meta information relates to the whole trace.
    *)

    (** Trace global unique identifier. *)
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
        underlying format support the given feature. *)
    val supports : t -> 'a tag -> bool


    (** [read_all trace tag] reads all event of the a given type *)
    val read_all : t -> 'a tag -> 'a seq

    (** [read_all_matching trace matcher] reads all events
        matching with a provided [matcher]. *)
    val read_all_matching : t -> 'a Value.Match.t -> 'a seq

    (** [read_events trace] reads a sequence of events from the [trace].*)
    val read_events : t -> event seq

    (** [next trace tag] reads and discards events until an event with a
        given tag is found. *)
    val next : t -> 'a tag -> 'a option

    (** [next_event trace] reads next event from the trace  *)
    val next_event : t -> event option


    (** [next_matching trace matcher] reads and discards trace events
        until an event matching with the [matcher] is found.*)
    val next_matching : t -> 'a Value.Match.t -> 'a option

    (** {2 Transformations} *)


    (** [filter_map t ~f] will return a trace where all events a
        filter-mapped with the provided function [f] *)
    val filter_map : t -> f:(event -> event option) -> t



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

    module Id : Regular.S with type t = id

    type step = [
      | `Stop
      | `Skip
      | `Fail
      | `Make of event
    ]

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
      val create : (Error.t -> step) -> t
    end
  end


  (** Loaded traces.

      This is a static container for the traces. Frontends and plugins
      can load traces and add it to the repository.

  *)
  module Traces : sig


    (** [to_list ()] returns a list of currently loaded traces.  *)
    val to_list : unit -> trace list

    (** [enum ()] enumerates all currently loaded traces  *)
    val enum : unit -> trace seq


    (** [add trace] registers a [trace] into the repository  *)
    val add : trace -> unit

    (** [remove trace] removes a [trace] from the repository, do
        nothing if it wasn't registered.  *)
    val remove : trace -> unit
  end


  (** {2 Trace Events}

      The trace may contain arbitrary events. The events below is a
      good starting point. Other libraries may add more event. *)




  (** {3 Supporting data types}  *)


  (** Represent a movement of data  *)
  module Move : sig
    type 'a t = {
      cell : 'a;                 (** source or destination  *)
      data : word;              (** moved data   *)
    } [@@deriving bin_io, compare, fields, sexp]
  end


  (** Represent a memory chunk.  *)
  module Chunk : sig
    type t = {
      addr : addr;              (** an address of the first byte *)
      data : string;            (** the bytes *)
    } [@@deriving bin_io, compare, fields, sexp]
  end


  (** a system call  *)
  module Syscall : sig
    type t = {
      number : int;             (** system call number *)
      args : word array;        (** arguments passed to a system call *)
    } [@@deriving bin_io, compare, fields, sexp]
  end


  (** hardware exception  *)
  module Exn : sig
    type t = {
      number : int;             (** an exception number *)
      src : addr option;        (** a source address *)
      dst : addr option;        (** a destination address *)
    } [@@deriving bin_io, compare, fields, sexp]
  end

  (** A code location *)
  module Location : sig
    type t = {
      name : string option;     (** a symbolic name *)
      addr : addr;              (** a virtual address *)
    } [@@deriving bin_io, compare, fields, sexp]
  end

  type location = Location.t [@@deriving bin_io, compare, sexp]


  (** A subroutine call.   *)
  module Call : sig
    type t = {
      caller : location;        (** location of a caller *)
      callee : location;        (** location of a calee *)
      args : word array;        (** call arguments *)
    } [@@deriving bin_io, compare, fields, sexp]
  end


  (** A return from a call.  *)
  module Return : sig
    type t = {
      caller : string;          (** caller name *)
      callee : string;          (** calee name *)
    } [@@deriving bin_io, compare, fields, sexp]
  end


  (** linking event  *)
  module Modload : sig
    type t = {
      name : string;            (** a name of linked module *)
      low : addr;               (** the lowest mapped address *)
      high : addr;              (** the hightest mapped address *)
    } [@@deriving bin_io, compare, fields, sexp]
  end

  type 'a move = 'a Move.t [@@deriving bin_io, compare, sexp]
  type chunk = Chunk.t [@@deriving bin_io, compare, sexp]
  type syscall = Syscall.t [@@deriving bin_io, compare, sexp]
  type exn = Exn.t [@@deriving bin_io, compare, sexp]
  type call = Call.t [@@deriving bin_io, compare, sexp]
  type return = Return.t [@@deriving bin_io, compare, sexp]
  type modload = Modload.t [@@deriving bin_io, compare, sexp]

  (** Types of events.  *)
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

    (** a module (shared library) is dynamically linked into a host program. *)
    val modload : modload tag
  end



  (** {2 Meta information}  *)

  (** Information about a tracer tool. *)
  module Tracer : sig
    type t = {
      name : string;            (** name of a tool *)
      args : string array;      (** the tool arguments *)
      envp : string array;      (** environment variables *)
      version : string;         (** tool version *)
    } [@@deriving bin_io, compare, sexp]
  end


  (** Information about a traced binary.  *)
  module Binary : sig
    type t = {
      path : string;            (** a path to the binary *)
      args : string array;      (** arguments passed to the binary *)
      envp : string array;      (** environment variables *)
      md5sum : string;          (** digest of the binary contents *)
    } [@@deriving bin_io, compare, sexp]
  end


  (** File information.  *)
  module File_stats : sig
    type t = {
      size  : int;              (** size of a file *)
      atime : float;            (** last access time *)
      mtime : float;            (** last modification time  *)
      ctime : float;            (** the creation time *)
    } [@@deriving bin_io, compare, sexp]
  end

  (** Information about the trace itself  *)
  module Trace_stats : sig
    type t = {
      user : string;   (** Name of a trace creator  *)
      host : string;   (** A host where trace was created *)
      time : float;   (** Time when tracing started  *)
    } [@@deriving bin_io, compare, sexp]
  end

  type tracer = Tracer.t [@@deriving bin_io, compare, sexp]
  type binary = Binary.t [@@deriving bin_io, compare, sexp]
  type file_stats = File_stats.t [@@deriving bin_io, compare, sexp]
  type trace_stats = Trace_stats.t [@@deriving bin_io, compare, sexp]



  (** Types of meta information.  *)
  module Meta : sig

    (** description of a tracer that was used to create the trace  *)
    val tracer : tracer tag

    (** description of a target binary (executable) that was traced.*)
    val binary : binary tag

    (** description of binary architecture. *)
    val arch : arch tag

    (** file stats of the traced binary  *)
    val binary_file_stats : file_stats tag

    (** generic information about the trace.  *)
    val trace_stats : trace_stats tag

  end

end
