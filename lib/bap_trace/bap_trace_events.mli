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
