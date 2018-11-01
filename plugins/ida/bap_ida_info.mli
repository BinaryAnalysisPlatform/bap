open Core_kernel

type mode = [ `m32 | `m64 ]

type t

(** [create path is_headless] *)
val create : string -> bool -> t Or_error.t

(** [find_ida info mode target] *)
val find_ida : t -> mode option -> string -> string

val is_headless : t -> bool

val check : t -> unit Or_error.t
