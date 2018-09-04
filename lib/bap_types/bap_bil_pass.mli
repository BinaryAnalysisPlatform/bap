open Bap_bil
open Regular.Std

type pass

val register_pass : string -> (bil -> bil) -> unit

val passes : unit -> pass list

val select_passes : pass list -> unit

val selected_passes : unit -> (bil -> bil) list

module Printable : Printable.S with type t := pass
