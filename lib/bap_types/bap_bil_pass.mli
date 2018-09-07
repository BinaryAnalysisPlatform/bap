open Bap_bil
open Regular.Std

type pass

val register_pass : ?desc:string -> string -> (bil -> bil) -> pass

val passes : unit -> pass list

val select_passes : pass list -> unit

val selected_passes : unit -> (bil -> bil) list

module Pass_pp : sig
  val name : pass -> string
  include Printable.S with type t := pass
end
