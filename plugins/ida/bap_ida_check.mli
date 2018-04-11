open Core_kernel

val check_headless : bool -> (unit, Error.t) Result.t

val check_path : string -> (unit, Error.t) Result.t
