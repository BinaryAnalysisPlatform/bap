open Core_kernel
val to_list : unit -> Bap_trace.t list
val enum : unit -> Bap_trace.t Sequence.t
val add : Bap_trace.t -> unit
val remove : Bap_trace.t -> unit
