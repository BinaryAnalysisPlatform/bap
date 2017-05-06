type ida_kind = [`idal | `idal64 | `idaq | `idaq64]

(** [register ida_path ida_kind is_headless] registers the IDA service with
    Bap_ida library *)
val register : string -> ida_kind option -> bool -> unit
