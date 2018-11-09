
(** [register ida_info ida_mode] registers the IDA service with
    Bap_ida library *)
val register : Bap_ida_info.t -> Bap_ida_info.mode option -> unit
