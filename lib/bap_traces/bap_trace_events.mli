open Bap.Std
open Bap_trace_event_types

(** Common trace events.

    This is a place where common trace events consolidate.
    A particular trace is not restricted to this event types,
    and can hold more or less event types.
*)

(** an read access to a memory cell  *)
val memory_load : addr move tag

(** a write access to a memory cell  *)
val memory_store : addr move tag

(** a value was read from a given register  *)
val register_read : var move tag

(** a value is written to the specified register  *)
val register_write : var move tag

(** this event can used to synchronize traces.
    The semantics is unspecified and remains open, so that a particular
    user can define its own meaning. But, the idea behind it, is that
    this event introduces a countable ordering. Basically, one can
    define a timeline, based on its own definition of a timescale.
    To keep it more concrete, here are the examples of different time
    scales:
    1. every new event (except the timestamp itself) increments the
       clock (basically all events are interleaved with the timestamp events)
    2. every new instruction increments the clock (e.g., timestamp is
       inserted after each `code_exec` event)
    3. the clock is incremented every cpu cycle (e.g., timestamps are
       incremented after each `code_exec` for the number of cpu cycles,
       that the executed instruction took)
    4. the clock is incremented every realtime second (e.g., obvious)
    5. the clock is incremented every realtime second,  and is initialized
       with the number of second that has passed since the start of the Epoch. *)
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
