(** Event subsystem.

    The event subsystem is a way of communicating between different
    components of BAP.
*)

open Bap_future.Std
open Bap_plugins.Std

type t = ..
type event = t = ..

(** the global stream of events  *)
val stream : t stream


(** [send event] sends the [event] to the global stream of events [stream]. *)
val send : t -> unit


(** [register_printer f] when the event [e] is printed, [f e] must be
    [None] if [f] is not a subset of events, that is intended to be
    printed by [f]. If it is [Some str], then [str] is printed
    out.

    If more than one printer returns [Some thing] for the same event,
    then the last registered has the precedence.
*)
val register_printer : (t -> string option) -> unit


(** Logging Events  *)
module Log : sig


  (** predefined log levels  *)
  type level =
    | Debug
    | Info
    | Warning
    | Error


  (** each logging message carries this inforamtion  *)
  type info = {
    level : level;              (** the event importance *)
    section : string;           (** the event section, e.g., plugin name *)
    message : string;           (** the actual message *)
  }

  type event += Message of info


  (** the progress bar message  *)
  type event += Progress of {
      task  : string;           (** the task name *)
      note  : string option;    (** an additional note *)
      stage : int option;       (** the current stage *)
      total : int option;       (** the total number of stages *)
    }


  (** [progress message] sends the progress report message.

      See the [report_progress] function for the detailed
      description.
  *)
  val progress : ?note:string -> ?stage:int -> ?total:int -> string -> unit

  (** [message level ~section "my message: %s" "hello"]  constructs
      and sends the logging message.
  *)
  val message :  level -> section:string -> ('a,Format.formatter,unit) format -> 'a


  (** generates reporters specialized to the current plugin.

      Use [include Bap_main_event.Create()] to bring to the scope
      logging and debugging functions.

      The functions will post logging events if the corresponding
      level of debugging is enabled. Right now only [BAP_DEBUG] and
      [BAP_DEBUG_<plugin>] environment variables are consulted to
      enable/disable the events of the {!Log.Debug} level. When
      neither of these variables is set, the [debug] function will be
      a no-op.

  *)
  module Create() : sig
    open Format


    (** [debug "my message: %s" "hello"] sends the debugging message. *)
    val debug   : ('a,formatter,unit) format -> 'a

    (** [info "my message: %s" "hello"] sends the info message. *)
    val info    : ('a,formatter,unit) format -> 'a

    (** [warning "my message: %s" "hello"] sends the warning message. *)
    val warning : ('a,formatter,unit) format -> 'a

    (** [error "my message: %s" "hello"] sends the error message. *)
    val error   : ('a,formatter,unit) format -> 'a

    (** [report_progress ~task:t ~note:n ~state:s ~total:s' ()] reports
        a progress of the task [t].

        Reports that the task [t] made a progress to the stage [s] out
        the total number of stages [s']. The note [n] may provide an
        additional textual explanation of the current stage. The report
        doesn't mean that the stage is finished, but rather that it is
        entered. Thus for [s'] stages we expect to receive [s'-1]
        reports. (This approach works fine with functional programming
        and iterating - as in functional programming it is more
        convenient to report before computation, and during the indexed
        iteration the index of the last element is one less than the
        total number of elements).

        All parameters are optional, and have the following default
        values if not specified:

        @param task defaults to the plugin [name];
        @param note defaults to the empty string;
        @param stage defaults to [None]
        @param total defaults to [None] or to the last value of this
             parameter for the given task.

        The [report_progress] bar is an easy way to provide some
        feedback to the system, either in the form of a progress (if the
        total number of stages is known) or in the form of a friendly
        ping back.

        The mechanism should be used by analyses that expect to take
        some time to complete. Usually, one plugin implements only one
        task, so the task name may be omitted. If an analysis is built
        from several tasks, then they can be represented as subtasks,
        and the main task should represent the whole work.

        Example:
        {[

          let find_interesting_points prog =
            report_progress ~task:"discover" ~total:(Term.length sub_t prog) ();
            Term.enum sub_t prog |> Seq.concat_mapi ~f:(fun stage sub ->
                report_progress ~note:(Sub.name sub) ~task:"discover" ~stage ();
                interesting_points_of_sub sub)

          let check_interesting_points points =
            report_progress ~task:"checking" ~total:(Seq.length points) ();
            Seq.iteri ~f:(fun stage p ->
                report_progress ~note:(Point.name p) ~task:"checking" ~stage ();
                check_point p)
        ]}
    *)
    val report_progress :
      ?task:string ->
      ?note:string ->
      ?stage:int ->
      ?total:int -> unit -> unit


    (** the formatter that could be used to send debug messages.  *)
    val debug_formatter : formatter

    (** the formatter that could be used to send info messages.  *)
    val info_formatter : formatter

    (** the formatter that could be used to send warnings.  *)
    val warning_formatter : formatter

    (** the formatter that could be used to send errors  *)
    val error_formatter : formatter
  end
end

(** [pp ppf event] outputs [event] to the formatter [ppf].  *)
val pp : Format.formatter -> t -> unit
