open Bap.Std
open Regular.Std

val run : [`readers | `writers] -> (module Data.S) -> unit
