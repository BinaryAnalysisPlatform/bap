open Core_kernel[@@warning "-D"]

val init : ?base:int64 -> ?pdb_path:string -> unit -> unit Or_error.t
