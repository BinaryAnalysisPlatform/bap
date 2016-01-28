open Bap_types.Std
open Bap_trace_meta_types

(** Common trace meta attributes.

    This file contains common meta attributes. Real traces may or may not
    contain them. They may also contain attributes that are not
    specified in this module.
*)


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
