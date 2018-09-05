open Bap_bil
open Regular.Std

type pass

val register_pass : string -> desc:string -> (bil -> bil) -> unit

val passes : unit -> pass list

val select_passes : pass list -> unit

val selected_passes : unit -> (bil -> bil) list

val pass_name : pass -> string

module Pass_pp : Printable.S with type t := pass
