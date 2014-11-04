open Core_kernel.Std
(** [load ~system] loads all finlib packages with entry
    [plugin_system] equal to [system], and returns a list of results
    of each load operation  *)
val load: system:string -> (string * unit Or_error.t) list
