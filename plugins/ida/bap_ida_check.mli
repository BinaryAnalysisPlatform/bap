open Core_kernel.Std

val check_headless : bool -> (unit, Error.t) Result.t

val check_path : string -> (unit, Error.t) Result.t
